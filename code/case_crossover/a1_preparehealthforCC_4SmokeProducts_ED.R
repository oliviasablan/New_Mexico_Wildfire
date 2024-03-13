# --------------------------------------------------------------------------------------------
# Adapted from Grace Kuiper's code titled "00d_HFDR_data_cleaning.R"
# Olivia Sablan
# 2/2024
# --------------------------------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)

#import ED data
ED <- read_csv("C:/Users/olivia.sablan/Desktop/Data/ED_data.csv")

# subset just the necessary columns and the format the dates
ED_sub <- ED %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
  select("Date","Zip","HispEth","age","sex","allresp1","asthma1","COPD1","pneumonia1","bronchitis1","allcardio1",
         "cardiacarrest1","arrythmia1","heartfail1","ischemic1","MI1",
         "cerebrovascular1", "New_Patient_ID")
rm(ED)

# Not super efficient, but I am going to subset the necessary dates (2022 Warm season aka April - August) 
# by first making new columns of the months and years for the ED data
ED_sub$month <- factor(format(ED_sub$Date, "%B"))
ED_sub$year <- factor(format(ED_sub$Date, "%Y"))

# Now I can filter the ED data for this time frame 
ED_sub <- ED_sub %>%
  filter((ED_sub$month == 'April') | (ED_sub$month == 'May') | (ED_sub$month == 'June') | (ED_sub$month == 'July')|
           (ED_sub$month == 'August'))
ED_sub <- ED_sub %>%
  filter(ED_sub$year == '2022') 

# Create a dataframe for the things that I want to keep that are not diagnoses
dates <- ED_sub[,c("Date","New_Patient_ID", "Zip","HispEth","age","sex")]

# Get dataset of whole year
dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7))
# Need to subset the referent period, as done above
ref_dates$month <- factor(format(ref_dates$Date, "%B"))
ref_dates$year <- factor(format(ref_dates$Date, "%Y"))
ref_dates <- ref_dates %>%
  filter((ref_dates$month == 'April') | (ref_dates$month == 'May') | (ref_dates$month == 'June') | (ref_dates$month == 'July')|
           (ref_dates$month == 'August'))
ref_dates <- ref_dates %>%
  filter(ref_dates$year == '2022') 
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
ED_full <- bind_rows(ED_sub,ref_dates) %>%
  arrange(Zip,Date) %>%
  mutate_at(c("allresp1","asthma1","COPD1","pneumonia1","bronchitis1","allcardio1",
              "cardiacarrest1","arrythmia1","heartfail1","ischemic1","MI1",
              "cerebrovascular1"),replaceNA)

#'Create list of casecrossover dataframes

allresp_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(allresp1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=allresp1) %>%
  mutate(out_name="allresp1")

asthma_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(asthma1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma1) %>%
  mutate(out_name="asthma1")

copd_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(COPD1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=COPD1) %>%
  mutate(out_name="COPD1")

pneumonia_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(pneumonia1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia1) %>%
  mutate(out_name="pneumonia1")

bronchitis_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(bronchitis1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis1) %>%
  mutate(out_name="bronchitis1")

cardio_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(allcardio1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=allcardio1) %>%
  mutate(out_name="allcardio1")

cardiacarrest_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(cardiacarrest1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiacarrest1) %>%
  mutate(out_name="cardiacarrest1")

arrhythmia_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(arrythmia1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia1) %>%
  mutate(out_name="arrythmia1")

heartfailure_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(heartfail1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfail1) %>%
  mutate(out_name="heartfail1")

ischemic_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(ischemic1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic1) %>%
  mutate(out_name="ischemic1")

myocardial_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(MI1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=MI1) %>%
  mutate(out_name="MI1")

cerebrovascular1_full <- ED_full %>%
  group_by(New_Patient_ID) %>%
  mutate(n=length(unique(cerebrovascular1))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular1) %>%
  mutate(out_name="cerebrovascular1")


casecross_list <- list(allresp_full, asthma_full,copd_full,pneumonia_full,bronchitis_full,
                       cardio_full,cardiacarrest_full, arrhythmia_full,heartfailure_full,
                       ischemic_full,myocardial_full,cerebrovascular1_full)

# Need to merge the case crossover for the health data with the lagged smoke exposure
# Will loop through this process for the 4 smoke exposures 

# Define the input file names from the previous code:
infiles = c('smoke_expKATE.rds', 'smoke_expBONNE.rds', 'smoke_expBONNE_KOvgp.rds', 'smoke_expKAMAL.rds')

#Define the output file names
outfiles = c('casecross_list2022_KATE_ED_ALLDIAG.rds', 'casecross_list2022_BONNE_ED.rds', 'casecross_list2022_BONNE_KOvgp_ED.rds', 'casecross_list2022_KAMAL_ED.rds')

for (i in (1:4)){
  print(infiles[i])
  oneinfile = paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/', infiles[i])
  # Read in the smoke exposure from "preparesmokeforCCloop"
  smoke_exp <- readRDS(oneinfile) %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
  # Merge with the health casecrossover on the zipcode
  casecross_list_withsmoke <- casecross_list %>%
    map(~left_join(.,smoke_exp %>%
                     rename(Zip=Zip) %>%
                     mutate(Zip=as.integer(Zip)),
                   by=c("Date","Zip")))
  oneoutfile = paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/', outfiles[i])
  saveRDS(casecross_list_withsmoke,oneoutfile)
}

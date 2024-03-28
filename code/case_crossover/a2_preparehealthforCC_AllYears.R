# --------------------------------------------------------------------------------------------
# Title: a_2preparehleathforCC_allYears.R
# Author: Olivia Sablan
# Created: March 2024
# Adapted from Grace Kuiper's code titled "00d_HFDR_data_cleaning.R"
# --------------------------------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)
# We will need to lag the heat data as well so read this function in to more easily do so
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}
infiles = c('ALLCLEANED_ESSENCE.csv', 'ED_data.csv')
outfiles = c('casecross_listAllYears_KATE_ESSENCE.rds', 'casecross_listAllYears_KATE_ED.rds')

for (i in (1:2)){
# import health data
  oneinfile = paste0('C:/Users/olivia.sablan/Desktop/Data/', infiles[i])
  health <- read_csv(oneinfile)
# subset just the necessary columns and the dates
  if (i == 1){
    health_sub <- health %>%
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
      select("Date","Zip","BroadResp", "AQResp", "Asthma", "Cardio", "New_Patient_ID", "Facility Name")
    }
  if (i == 2){
    health_sub <- health %>%
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
      select("Date","Zip","allresp1", "asthma1", "allcardio1", "New_Patient_ID")
    }

  # remove the large health dataset because it's not needed anymore
  rm(health) 
  # make new columns in this dataset for the month and year so we can subset the timeframe we need
  health_sub$month <- factor(format(health_sub$Date, "%B")) 
  health_sub$year <- factor(format(health_sub$Date, "%Y"))

# Now we can filter the health data for wildfire smoke season (April - September) 
  health_sub <- health_sub %>%
  filter((health_sub$month == 'April') | (health_sub$month == 'May') | (health_sub$month == 'June') | (health_sub$month == 'July')|
           (health_sub$month == 'August') | (health_sub$month == 'September'))
  health_sub <- health_sub %>%
  filter((health_sub$year == 2019) | (health_sub$year == 2020) | (health_sub$year == 2021) | (health_sub$year == 2022)) #Need to look at 2019 and beyond because syndromic is > 2019

# Create a dataframe for the things that we want to carry through the analysis that are not diagnoses
# Need to keep the facility name for ESSENCE because I need it to match to the offline periods
  if (i == 1){
    dates <- health_sub[,c("Date","New_Patient_ID", "Zip", "Facility Name")]
  }
  if (i == 2){
    dates <- health_sub[,c("Date","New_Patient_ID", "Zip")]
  }
# Make dataset of whole year
dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7))
# Need to subset the referent period, as done above for health data
ref_dates$month <- factor(format(ref_dates$Date, "%B"))
ref_dates$year <- factor(format(ref_dates$Date, "%Y"))
ref_dates <- ref_dates %>%
  filter((ref_dates$month == 'April') | (ref_dates$month == 'May') | (ref_dates$month == 'June') | (ref_dates$month == 'July')|
           (ref_dates$month == 'August')| (ref_dates$month == 'September'))
ref_dates <- ref_dates %>%
  filter((ref_dates$year == 2019) | (ref_dates$year == 2020) | (ref_dates$year == 2021) | (ref_dates$year == 2022))
  # For ESSENCE, we need to only use dates when the facilities systems were online as referent period
  # Read in the onlinetimes and merge with the ref_date dataframe to remove the referent dates when offline
  if (i == 1){
    onlinetimes <- read_csv("C:/Users/olivia.sablan/Desktop/Data/ONLINEdates.csv")
    ref_dates <- merge(ref_dates, onlinetimes, by = c("Date", "Facility Name"))
    rm(onlinetimes)
    }
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
  if (i == 1){
    health_full <- bind_rows(health_sub,ref_dates) %>%
      arrange(Zip,Date) %>%
      mutate_at(c("BroadResp","AQResp", "Asthma", "Cardio"),replaceNA)
    
    broadresp_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(BroadResp))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=BroadResp) %>%
      mutate(out_name="BroadResp")
    
    asthma_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(Asthma))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=Asthma) %>%
      mutate(out_name="Asthma")
    
    cardio_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(Cardio))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=Cardio) %>%
      mutate(out_name="Cardio")
    
    AQ_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(Cardio))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=Cardio) %>%
      mutate(out_name="AQResp")
    
    casecross_list <- list(broadresp_full, asthma_full, cardio_full, AQ_full)
    rm(broadresp_full, asthma_full, cardio_full, AQ_full)
       }
  
  if (i == 2){
    health_full <- bind_rows(health_sub,ref_dates) %>%
      arrange(Zip,Date) %>%
      mutate_at(c("allresp1", "asthma1", "allcardio1"),replaceNA)
    allresp_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(allresp1))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=allresp1) %>%
      mutate(out_name="allresp1")
    
    asthma_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(asthma1))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=asthma1) %>%
      mutate(out_name="asthma1")
    
    cardio_full <- health_full %>%
      group_by(New_Patient_ID) %>%
      mutate(n=length(unique(allcardio1))) %>%
      filter(n==2) %>%
      select(-n) %>%
      rename(outcome=allcardio1) %>%
      mutate(out_name="allcardio1")
    
    casecross_list <- list(allresp_full, asthma_full, cardio_full)
    rm(allresp_full, asthma_full, cardio_full)
    }

# Read in the smoke exposure from "00_preparesmokeforCCloop.R", for this analysis, we need Kate O'Dell's product
smoke_exp <- readRDS("C:/Users/olivia.sablan/Desktop/Code from Grace/Data/smoke_expKATE.rds") %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'))

# Read in population-weighted heat index data to be merged
HI <- read_csv('C:/Users/olivia.sablan/Desktop/Data/MaxPop_HI_OS.csv')%>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
# apply lag function to lag the heat index (just like the smoke exposure lag, but shorter code) 
HI_lagged <- HI %>%
  group_by(Zip) %>%
  mutate(., !!!funlag(maxpopHI,5))
rm(HI)
# Merge both with the casecrossover list that was made above
casecross_list_withsmoke <- casecross_list %>%
  map(~left_join(.,smoke_exp %>%
                   mutate(Zip=as.integer(Zip)),
                 by=c("Date","Zip")))
casecross_list_withHI <- casecross_list_withsmoke %>%
  map(~left_join(.,HI_lagged %>%
                   mutate(Zip=as.integer(Zip)),
                 by=c("Date","Zip")))

  oneoutfile = paste0("C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/", outfiles[i])
  saveRDS (casecross_list_withHI, oneoutfile)
} 
print('Done')
# --------------------------------------------------------------------------------------------
# Title: a3_preparehealthforCC_EachYear.R
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
library(readxl)

# We will need to lag the heat data as well so read this function in to more easily do so
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}
infiles = c('ALLCLEANED_ESSENCE.csv', 'ED_data.csv')

for (q in (1:2)){
  #import health data
  oneinfile <- paste0('C:/Users/olivia.sablan/Desktop/Data/', infiles[q])
  health <- read_csv(oneinfile)
  if (q == 1){
# subset just the necessary columns and the dates
    health_sub <- health %>%
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
      select("Date","Zip","BroadResp", "Asthma", "Cardio", "New_Patient_ID", "Facility Name")
  }
  if (q == 2){
    health_sub <- health %>%
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
      select("Date","Zip","allresp1", "asthma1", "allcardio1", "New_Patient_ID")
  }
  
rm(health)
# by first making new columns of the months and years for the ED data
health_sub$month <- factor(format(health_sub$Date, "%B"))
health_sub$year <- factor(format(health_sub$Date, "%Y"))

# Now I can filter the ED data for this time frame 
health_sub <- health_sub %>%
  filter((health_sub$month == 'April') | (health_sub$month == 'May') | (health_sub$month == 'June') | (health_sub$month == 'July')|
           (health_sub$month == 'August') | (health_sub$month == 'September'))
onlinetimes <- read_csv("C:/Users/olivia.sablan/Desktop/Data/ONLINEdates.csv")
for (i in c('2016', '2017', '2018', '2019', '2020', '2021', '2022'))
{ if ((q == 1) & (i %in% c('2016', '2017', '2018')) ){
  message<- paste0('wrong years', infiles[q], i)
  print(message)
}
  else{
    
  yearly <- health_sub %>%
  filter(health_sub$year == i) 
if (q == 1){
  dates <- yearly[,c("Date","New_Patient_ID", "Zip", "Facility Name")]
}
if (q == 2){
  dates <- yearly[,c("Date","New_Patient_ID", "Zip")]
}
dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7))
ref_dates$month <- factor(format(ref_dates$Date, "%B"))
ref_dates$year <- factor(format(ref_dates$Date, "%Y"))
ref_dates <- ref_dates %>%
  filter((ref_dates$month == 'April') | (ref_dates$month == 'May') | (ref_dates$month == 'June') | (ref_dates$month == 'July')|
           (ref_dates$month == 'August')| (ref_dates$month == 'September'))
ref_dates <- ref_dates %>%
  filter(ref_dates$year == i)
if (q == 1){
  ref_dates <- merge(ref_dates, onlinetimes, by = c("Date", "Facility Name"))
}
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
if (q == 1){
  health_full <- bind_rows(yearly,ref_dates) %>%
    arrange(Zip,Date) %>%
    mutate_at(c("BroadResp", "Asthma", "Cardio"),replaceNA)
  
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
  
  casecross_list <- list(broadresp_full, asthma_full, cardio_full)
  rm(broadresp_full, asthma_full, cardio_full)}
if (q == 2){
  health_full <- bind_rows(yearly,ref_dates) %>%
    arrange(Zip,Date) %>%
    mutate_at(c("allresp1",  "asthma1", "allcardio1"),replaceNA)
  
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
  rm(allresp_full, asthma_full, cardio_full)}

# Read in smoke exposure data
smoke_exp <- readRDS("C:/Users/olivia.sablan/Desktop/Code from Grace/Data/smoke_expKATE.rds") %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
# Read in heat index data to be merged with the casecrossover list that was made above
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
if( q == 1){
  outfile <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/casecross_list', i, '_KATE_ESSENCE.rds' )
}
if( q == 2){
  outfile <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/casecross_list', i, '_KATE_ED.rds' )
}
saveRDS(casecross_list_withHI, outfile)}
} 
}
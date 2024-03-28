# --------------------------------------------------------------------------------------------
# Title: preparesmokeforCCloop.R
# Author: Olivia Sablan
# Created: March 2024
# Adapted from Grace Kuiper's code titled "00c_finalize_exposure_data.R"
# --------------------------------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(dplyr)

# There are 4 total smoke products that we need to generate lagged datasets 
# We will loop through the same process with the 4 different products.
# "KATE" refers to the smoke product created by Kate O'Dell et al. (2019), where the EPA AQS monitors are kriged
# "BONNE" refers to the product created by Bonne Ford, where the PurpleAir sensors are added to the O'Dell et al. product
# "BONNE_KOvgp" refers to the product created by Bonne Ford, where the PurpleAir sensors are added to the O'Dell et al. product and the
# variogram parameters (vgp) remain the same as O'Dell et al. (2019) product
# "KAMAL" refers to the product created by Kamal Jyoti Maji with CMAQ modeling

# First define the names of the input smoke products 
infiles = c('AllZipSmoke_Total_os.csv', 'bigPurpleAirZip_os.csv', 'bigPurpleAirZip_os_KOvgp.csv',
            'Kamal_average_reformat_all_OS.csv', 'Kamal_max_reformat_all_OS.csv')

# Next define names for the final smoke product output, aka the lagged dataframes
outfiles = c('smoke_expKATE.rds', 'smoke_expBONNE.rds', 'smoke_expBONNE_KOvgp.rds', 'smoke_expKAMAL.rds',
             'smoke_expKAMALmax.rds')

for (i in (1:5)){
  # Loop through reading in each smoke product
  oneinfile = paste0('C:/Users/olivia.sablan/Desktop/Data/', infiles[i])
  smoke <- read_csv(oneinfile)
  # Generate dataframes with lagged exposure data
  # Select only the necessary variables (Zipcode, Date, and PM2.5 Concentration from smoke)
  smoke_lag0_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%Y'))
  smoke_lag1_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date=as.Date(Date, format = '%m/%d/%Y')+1) %>%
    rename(smokepm25_lag1=smokepm25)
  smoke_lag2_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date=as.Date(Date, format = '%m/%d/%Y')+2) %>%
    rename(smokepm25_lag2=smokepm25)
  smoke_lag3_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date=as.Date(Date, format = '%m/%d/%Y')+3) %>%
    rename(smokepm25_lag3=smokepm25)
  smoke_lag4_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date=as.Date(Date, format = '%m/%d/%Y')+4) %>%
    rename(smokepm25_lag4=smokepm25)
  smoke_lag5_df <- smoke %>%
    dplyr::select(Zip,Date,smokepm25) %>%
    mutate(Date=as.Date(Date, format = '%m/%d/%Y')+5) %>%
    rename(smokepm25_lag5=smokepm25)
  
  # Now merge together all 5 of the lags created above
  smoke_exp <- full_join(smoke_lag0_df,
                         smoke_lag1_df,by=c("Zip","Date")) %>%
    full_join(smoke_lag2_df,by=c("Zip","Date")) %>%
    full_join(smoke_lag3_df,by=c("Zip","Date")) %>%
    full_join(smoke_lag4_df,by=c("Zip","Date")) %>%
    full_join(smoke_lag5_df,by=c("Zip","Date")) %>%
    mutate(Zip=as.character(Zip))
  
  # Save this smoke exposure to be read in for "a1_preparehealthforCC_4SmokeProducts_ED.R"
  oneoutfile = paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/', outfiles[i])
  saveRDS(smoke_exp,oneoutfile)
  }
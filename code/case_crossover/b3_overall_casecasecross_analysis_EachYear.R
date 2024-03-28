# --------------------------------------------------------------------------------------------
# Title: b3_overall_casecasecross_analysis_EachYear.R
# Author: Olivia Sablan
# Created: March 2024
# Adapted from Grace Kuiper's code titled "01_overall_casecross_analysis.R"
# -------------------------------------------------------------------------------------------
library(tidyverse) # data wrangle/plot
library(survival) # conditional logistic models
library(splines) # splines
library(lubridate) # works with dates
library(broom)
library(stringr)
library(dlnm)
rm(list=ls())

# Since we are looking for a 10 ug/m3 increase in PM2.5, need to define this function
scale10 <- function(x) (x/10)

## Distributed Lag Associations
# From Grace Kuiper's code:
#"First step is to define a function (called distribut_that_lag)that will take a model with a distributed lag basis function 
#in it and extract the lagged and cumulative results from it. The lag_mod term is where you reference the model, and strata 
#is the strata you'd like to estimate it for. This is used to get out terms for PM~2.5~ smoke and non-smoke. 
#Right now this function only handles lagged matrices of 0-6 days, and degrees of freedom, and will need to be modified if 
#you want to vary this."

distribute_that_lag <- function(lag_mod, strata, exposure_basis) {
  # output pm basis estimates
  parms <- broom::tidy(lag_mod) %>% 
    filter(stringr::str_detect(term, strata)) %>% 
    select(estimate) %>% 
    as_vector()
  # output estimate names for cov matrix
  names <- stringr::str_subset(names(lag_mod$coefficients), strata)
  # estimate associations
  est <- exposure_basis %*% parms
  # estimate standard error for each interval
  # time variable
  time <- ((rep(1:length(est))-1))
  # covariance matrix for knots 
  cov_mat <- as.matrix(vcov(lag_mod))[names, names]
  # estimate variance of spline
  var <- exp_b %*% cov_mat %*% t(exp_b)
  # estimate lag ----
  # estimate standard error for each lag day for smoke
  l_se <- sqrt(diag(var))
  # calculate lower and upper bound for smoke
  l_est_l95 <- est + (l_se*qnorm(1-0.975))
  l_est_u95 <- est + (l_se*qnorm(0.975))
  l_type <- "lag"
  # lag dataframe
  l_df <- data.frame(strata, l_type, time, 
                     exp(est), exp(l_est_l95), exp(l_est_u95), 
                     row.names = NULL) 
  # assign column names
  colnames(l_df) <- c("strata", "type", "time", 
                      "odds_ratio", "lower_95", "upper_95")
  # cumulative estimates
  c_est <- sapply(seq_along(est), function(x){
    sum(est[1:x])
  })
  # stderr cumulative effect smk
  c_se <- sapply(seq_along(c_est), function(y){
    sqrt(sum(var[1:y,1:y]))
  })
  # estimate 95% CI
  c_l95 <- c_est+(c_se*qnorm(1-0.975))
  c_u95 <- c_est+(c_se*qnorm(0.975))
  # type
  c_type <- "cumulative"
  # return dataframe
  c_df <- data.frame(strata, c_type, time, exp(c_est), 
                     exp(c_l95), exp(c_u95), row.names = NULL) 
  # assign column names
  colnames(c_df) <- c("strata", "type", "time", 
                      "odds_ratio", "lower_95", "upper_95")
  # bind lagged and cumulative 
  lag_est <- rbind(l_df, c_df) %>% 
    mutate(strata = as.character(strata),
           type = as.character(type))
  # return lagged estimate
  return(lag_est)
} # end lag estimate function

fileends = c('_KATE_ED', '_KATE_ESSENCE')

for (w in (1:2)){
# Read in case-crossover dataset from 'a3_preparehealthforCC_EachYear.R"
# Then use the scale10 function on just the columns with smokepm25 in the name
for (q in c('2016', '2017', '2018', '2019', '2020', '2021', '2022'))
{    if ((w == 2) & (q %in% c('2016', '2017', '2018')) ){
  message<- paste0('wrong years', fileends[w], q)
  print(message)
}
  else{
  infile = paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/casecross_list', q, fileends[w], '.rds')
casecross_list <- readRDS(infile) 
casecross_list <- casecross_list %>%
  map(~mutate_at(.,vars(contains("smokepm25")),scale10))

# distributed lag function
lag_est <- data.frame()
for (i in 1:length(casecross_list)) {
  # output dataframe from list
  data <- casecross_list[[i]] %>% 
    ungroup() %>%
    mutate(outcome = as.numeric(as.character(outcome))) %>%
    # remove missing lagged data
    filter(!is.na(smokepm25_lag5))
  # output outcome name
  out_name <- as.character(unique(data$out_name))
  # create lagged matrix
  pm_mat <- as.matrix(select(data, smokepm25, contains("smokepm25_lag")))
  HI_mat <- as.matrix(select(data, contains("maxpopHI")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 3, intercept = T)
  # pm basis
  pm_basis <- pm_mat %*% exp_b
  # HI basis
  HI_basis <- HI_mat %*% exp_b
  # run lagged model
  lag_mod <- clogit(outcome ~ pm_basis + HI_basis + strata(New_Patient_ID), data = data)
  
  # estimate lag estimate
  lag_est_temp <- distribute_that_lag(lag_mod, strata = "pm", 
                                      exposure_basis = exp_b) %>% 
    mutate(outcome = out_name) %>% select(outcome, strata:upper_95)
  lag_est <- rbind(lag_est,lag_est_temp)
}
mort_dl_pm_results <- lag_est

lagged_results <- mort_dl_pm_results %>% filter(type == "lag")
cumulative_results <- mort_dl_pm_results %>% filter(type == "cumulative")
outfile1 <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/cumulative_results/cumulative_results', q, fileends[w], '.csv')
outfile2 <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/lagged_results/lagged_results', q, fileends[w], '.csv')
write_csv(cumulative_results, outfile1)
write_csv(lagged_results, outfile2)
}}}
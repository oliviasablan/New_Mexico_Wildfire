# --------------------------------------------------------------------------------------------
# Adapted from Grace Kuiper's code titled "00d_HFDR_data_cleaning.R"
# Olivia Sablan
# 2/2024
# --------------------------------------------------------------------------------------------
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

fileends = c('_KATE_ED', '_KATE_ESSENCE')

for (w in (1:2)){
# Read in case-crossover dataset from 'a3_preparehealthforCC_EachYear.R"
# Then use the scale10 function on just the columns with smokepm25 in the name
for (q in c('2016', '2017', '2018', '2019', '2020', '2021', '2022'))
{if (w == 1){
  if(q %in% c('2016', '2017', '2018')){
    print('Wrong years')
  }

  else{
  infile = paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/casecross_list/casecross_list', q, fileends[w], '.rds')
casecross_list <- readRDS(infile) 
casecross_list <- casecross_list %>%
  map(~mutate_at(.,vars(contains("smokepm25")),scale10))

#### Same-Day Associations ####

# Evaluating a 10 ug/m^3^ increase in wildfire smoke-related PM~2.5~ and risk for emergency department visit 
# for certain cardio and respiratory outcomes. Same-day association with smoke PM2.5 and ED visits during wildfire season
outcomes <- c()
for (i in 1:length(casecross_list)) {
  outcomes <- c(outcomes,unique(casecross_list[[i]]$out_name))
}
pm_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25"))# %>%
    #mutate_all(exp))
  }) %>% # end map
  cbind(outcomes, .)
# print results
print(pm_results)
#saveRDS(pm_results,"pm_results.rds")

# Repeat this again for the lagged PM2.5

# 1 day lag association with WFS PM2.5 and ED visits during wildfire season
pm_lag1_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25_lag1 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25_lag1"))
  }) %>% # end map
  cbind(outcomes, .)
# print results
#print(pm_lag1_results)
#saveRDS(pm_lag1_results,"pm_lag1_results.rds")

# 2 day lag association with WFS PM2.5 and ED visits during wildfire season
pm_lag2_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25_lag2 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25_lag2"))
  }) %>% # end map
  cbind(outcomes, .)
#print(pm_lag2_results)
#saveRDS(pm_lag2_results,"pm_lag2_results.rds")

# 3 day lag association with WFS PM2.5 and ED visits during wildfire season
pm_lag3_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25_lag3 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25_lag3"))
  }) %>% # end map
  cbind(outcomes, .)
#print(pm_lag3_results)
#saveRDS(pm_lag3_results,"pm_lag3_results.rds")

# 4 day lag association with WFS PM2.5 and ED visits during wildfire season
pm_lag4_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25_lag4 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25_lag4"))
  }) %>% # end map
  cbind(outcomes, .)
#print(pm_lag4_results)
#saveRDS(pm_lag4_results,"pm_lag4_results.rds")

# 5 day lag association with WFS PM2.5 and ED visits during wildfire season
pm_lag5_results <- casecross_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ smokepm25_lag5 + strata(New_Patient_ID), data = df)
    est <- cbind(WF_est <- broom::tidy(mod) %>% filter(term == "smokepm25_lag5"))
  }) %>% # end map
  cbind(outcomes, .)
print(pm_lag5_results)
#saveRDS(pm_lag5_results,"pm_lag5_results.rds")


## Distributed Lag Associations
# 
# First step is to define a function (called distribut_that_lag)that will take a model with a distributed lag basis function 
#in it and extract the lagged and cumulative results from it. The lag_mod term is where you reference the model, and strata 
#is the strata you'd like to estimate it for. This is used to get out terms for PM~2.5~ smoke and non-smoke. 
#Right now this function only handles lagged matrices of 0-6 days, and degrees of freedom, and will need to be modified if 
#you want to vary this.


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
  # print(out_name) # track which outcome dataframe it's on
  # create lagged matrix
  pm_mat <- as.matrix(select(data, smokepm25, contains("_lag")))
  #temp_mat <- as.matrix(select(data, contains("mean_tmpf")))
  #relh_mat <- as.matrix(select(data, contains("mean_relh")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 3, intercept = T)
  # pm basis
  pm_basis <- pm_mat %*% exp_b
  # temp basis
  #temp_basis <- temp_mat %*% exp_b
  #relh_basis <- relh_mat %*% exp_b
  # run lagged model
  lag_mod <- clogit(outcome ~ pm_basis + strata(New_Patient_ID), data = data)
  
  # estimate lag estimate
  lag_est_temp <- distribute_that_lag(lag_mod, strata = "pm", 
                                      exposure_basis = exp_b) %>% 
    mutate(outcome = out_name) %>% select(outcome, strata:upper_95)
  lag_est <- rbind(lag_est,lag_est_temp)
}
mort_dl_pm_results <- lag_est

lagged_results <- mort_dl_pm_results %>% filter(type == "lag")
cumulative_results <- mort_dl_pm_results %>% filter(type == "cumulative")
outfile1 <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/lagged_results/lagged_results', q, fileends[w], '.rds')
outfile2 <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/cumulative_results/cumulative_results', q, fileends[w], '.rds')
outfile3 <- paste0('C:/Users/olivia.sablan/Desktop/Code from Grace/Data/lagged_results/lagged_results', q, fileends[w], '.csv')
saveRDS(lagged_results, outfile1)
saveRDS(cumulative_results, outfile2)
write_csv(lagged_results,outfile3)
}}}}
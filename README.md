# Association between smoke exposure and emergency department visits and syndromic surveillance during wildfire season in New Mexico
## Created and maintain by: Olivia Sablan <br> Last Updated: May 1, 2025 <br>
##### Email: osablan@colostate.edu

## Overview
This Github repository contains R code, Python code, and files for our study which investigates associations between exposure to fine particulate matter (PM<sub>2.5</sub>) from wildfire smoke and emergency department visits along with syndromic surveillance (ESSENCE) reports in the state of New Mexico from 2016-2022. This repository corresponds to the study detailed in the manuscript "Health impacts of wildfire smoke exposure in New Mexico during 2022 and sensitivity to exposure estimate and referent period," which is under review. This paper compares four smoke exposure estimates (discussed further below) during the 2022 wildfire smoke season. A second manuscript will be published in the future to compare the emergency department visits to ESSENCE reports for 2016-2022.

## General Method Outline
The goal of this project was to assess if the associations between PM<sub>2.5</sub> from wildfire smoke and respiratory and cardiovascular emergency department visits. We compared several smoke products as well as two different health data sources (emergency department records and the syndromic surveillance system). We used data from 2016-2022 for emergency department visits and 2019-2022 for syndromic surveillance. 

We used an existing smoke product (detailed in O'Dell et al. 2019:  https://doi.org/10.1021/acs.est.8b05430), and also incorporated low-cost PM<sub>2.5</sub> sensors (PurpleAir) into this existing product. Both products distinguish smoke using satellite observations from the National Oceanic and Atmospheric Administration's Hazard Mapping System (https://www.ospo.noaa.gov/Products/land/hms.html). We also compared to the Maji et al (2024) smoke product (https://doi.org/10.1016/j.scitotenv.2024.174197), which modeled smoked PM<sub>2.5</sub> using the Community Multiscale Air Quality Modeling System (CMAQ). We population-weighted the smoke exposure estimates to the ZIP code level to match the resolution of the health data. We then conducted a time-stratified case-crossover analysis for each cardiorespiratory outcome of interest.

The population-weighted datasets listed here can be accessed through Dryad (DOI: 10.5061/dryad.gb5mkkx2f):
- **O'Dell et al (2019) population-weighted ZIP code daily average smoke PM<sub>2.5</sub>**
  - Referred to in the manuscript as "Regulatory-only"
- **Regulatory monitor and PurpleAir population-weighted ZIP code daily average smoke PM<sub>2.5</sub>**
  - Referred to in the manuscript as "Regulatory + PA"
- **Maji et al (2024) population-weighted ZIP code 24-hour average smoke PM<sub>2.5</sub>**
  - Referred to in the manuscript as "CMAQ 24-hour average"
- **Maji et al (2024) population-weighted ZIP code 1-hour maximum smoke PM<sub>2.5</sub>**
  - Referred to in the manuscript as "CMAQ 1-hour maximum"
- **Population-weighted heat index by ZIP code**

The emergency department and syndromic surveillance data includes protected health information (e.g., patient identification number, patient ZIP code, patient county, etc.) covered by the Health Information Portability and Accountability Act. Therefore, these data are not available due to data use agreements with the New Mexico Department of Health. Parties interested in reproducing or extending this work will need to set up their own data use agreements with the New Mexico Department of Health to receive this data. 

## File Description 
1. **/code/clean_data/**
-  **CleanHealthStep1.ipynb** - removes duplicate entries and multiple visits as well as reformats the syndromic surveillance (ESSENCE) diagnosis columns
-  **CleanHealthStep2.ipynb** - removes erroneous ZIP codes and reformats ED dianosis columns
-  **reformatSmoke_TempData.ipynb**- merges and melts the smoke datafiles and the temperature data to create 4 final population-weighted ZIP code datasets for each smoke product and 1 for the temperature
-  **FindESSENCEofflinetimes.ipynb** - creates a dataset of dates when each ESSENCE facility was online
2. **/code/case_crossover/**
- **00_preparesmokeforCCloop.R** - applies a lag to each smoke product
We repeated the same case-crossover process for several analyses. First, we conducted the time-stratified case-crossover analysis using several different smoke products with the ED data (code labeled with "1" and ending with "4SmokeProducts"):
- **a1_preparehealthforCC_4SmokeProducts_ED.R** - combines the ED data and all four smoke products to create a case-crossover dataset
- **b1_overall_casecasecross_analysis_4SmokeProducts_ED.R** - conducts stratified case-crossover single day and distributed lag model analyses using conditional logistic regression

Next, we conducted our analysis with the ED and ESSENCE data across 2019-2022 to see long-term trends with the O'Dell et al. (2019) smoke product (code labeled with "2" and ending with "AllYears"):
- **a2_preparehealthforCC_AllYears.R** - combines the ESSENCE data and the O'Dell et al. (2019) smoke product to create a case-crossover dataset and then repeats with ED data
- **b2_overall_casecasecross_analysis_AllYear.R** - conducts stratified case-crossover single day and distributed lag model analyses using conditional logistic regression

We also conducted our analysis for each smoke season separately for ED and ESSENCE with the O'Dell et al. (2019) smoke product (code labeled with "3" and ending with "EachYear"):
- **a3_preparehealthforCC_EachYear.R** - combines the ESSENCE data and the O'Dell et al. (2019) smoke product to create a case-crossover dataset and then repeats with ED data
- **b3_overall_casecasecross_analysis_EachYear.R** - conducts stratified case-crossover single day and distributed lag model analyses using conditional logistic regression

3. **/code/plotting/**
- **1lags_comparing4SmokeProducts.ipynb**- plotting distributed lag results to compare the different smoke products
- **2lags_AllYears.ipynb** - plotting distributed lag results over all smoke seasons (2019-2022) for ESSENCE and ED data
- **3lags_EachYear.ipynb** - plotting distributed lag results to compare each year (2016 - 2022) of smoke seasons for ESSENCE and ED data
- 

4. **/code/misc**
- **CropGridToNM.ipynb** - crops gridded netCDF dataset to the state of New Mexico

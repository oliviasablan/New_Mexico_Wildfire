# Association between smoke exposure and emergency department visits and syndromic surveillance wildfire season in New Mexico
## Created and maintain by: Olivia Sablan <br> Date: March 29, 2024 <br>
##### Email: osablan@colostate.edu

## Overview
This Github repository contains R code and files for our study which investigates associations between exposure to fine particulate matter (PM<sub>2.5</sub>) from wildfire smoke and emergency department visits along with syndromic surveillance (ESSENCE) reports in the state of New Mexico from 2016-2022. This repository corresponds to the study detailed in the manuscript "ADD TITLE," which can be accessed here: ADD LINK

## General Method Outline
The goal of this project was to assess if the associations between PM<sub>2.5</sub> from wildfire smoke and respiratory and cardiovascular emergency department visits. We compared several smoke products as well as two different health data sources (emergency department records and the syndromic surveillance system). We used data from 2016-2022 for emergency department visits and 2019-2022 for syndromic surveillance. 

We used an existing smoke product (detailed in O'Dell et al. 2019:  https://doi.org/10.1021/acs.est.8b05430), and also incorporated low-cost PM<sub>2.5</sub> sensors (PurpleAir) into this existing product. Both products distinguish smoke using satellite observations from the National Oceanic and Atmospheric Administration's Hazard Mapping System (https://www.ospo.noaa.gov/Products/land/hms.html). We population-weight both smoke exposure estimates to the ZIP code level to match the resolution of the health data. We then conducted a time-stratified case-crossover analysis for each cardiopulmonary outcome of interest.

The code created to conduct these analyses is included in this repository. The smoke gridded exposure datasets can be found at the CSU data repository detailed below:
O'Dell et al (2019) data
- https://mountainscholar.org/items/89896cf7-1a64-43be-a61b-44cd460e9632 (2006-2018)
- https://mountainscholar.org/items/cf8053c6-6a16-49e7-8b0f-1044322d867e (2019-2020)
- https://mountainscholar.org/items/370ac2d9-422f-4256-a764-b75a5a84e724 (2021)
- The gridded 2022 estimate is not publicly available at this time, but can be obtained from Bonne Ford (bonne@atmos.colostate.edu).
- ADD DRYAD FOR POPULATION WEIGHTED

PurpleAir population-weight ZIP code data
- ADD DYRAD

The emergency department and syndromic surveillance data includes protected health information (e.g., patient identification number, patient ZIP code, patient county, etc.) covered by the Health Information Portability and Accountability Act. Therefore, these data are not available due to data use agreements with the New Mexico Department of Health. Parties interested in reproducing or extending this work will need to set up their own data use agreements with the New Mexico Department of Health to receive this data. 

## File Desciption (for all code in the "code" directory)
1. clean_data
-  **"CleanHealthStep1.ipynb"** - removes duplicate entries and multiple visits as well as reformats the syndromic surveillance (ESSENCE) diagnosis columns
-  **"CleanHealthStep2.ipynb"** - removes erroneous ZIP codes and reformats ED dianosis columns
-  **"reformatSmokeData.ipynb"**- merges and melts the smoke datafiles to create 4 final population-weighted ZIP code datasets for each smoke product
2. case_crossover
   

######################################################
# Title : reproduce_cote                             #
# Goal : Replicate mémoire (2016) with canadian data #
# Author : Christopher Blier-Wong                    #
# Date : January 2019                                #
######################################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
library(mgcv)


# Import data -------------------------------------------------------------

data_original <- read_csv("src/data/canada-2014-2017.csv")


# Rename variables --------------------------------------------------------

names(data_original)

data <- data_original %>% 
  filter(RA_COL_INDICATOR == 1) %>% 
  filter(RA_CMP_INDICATOR == 1) %>% 
  mutate(expo = RA_EXPOSURE_DAY / 365.25) %>% 
  mutate(lnexpo = log(expo)) %>% 
  mutate(expo2 = RA_DISTANCE_DRIVEN / 10000) %>% 
  mutate(lnexpo2 = log(expo2))

data <- data %>% 
  mutate(cat_km5000 = RA_ANNUAL_KMS_DRIVEN_SYSTEM < 5000) %>% 
  mutate(cat_km10000 = (RA_ANNUAL_KMS_DRIVEN_SYSTEM >= 5000 & RA_ANNUAL_KMS_DRIVEN_SYSTEM < 10000)) %>% 
  mutate(cat_km15000 = (RA_ANNUAL_KMS_DRIVEN_SYSTEM >= 10000 & RA_ANNUAL_KMS_DRIVEN_SYSTEM < 15000)) %>% 
  mutate(cat_km25000 = (RA_ANNUAL_KMS_DRIVEN_SYSTEM >= 15000 & RA_ANNUAL_KMS_DRIVEN_SYSTEM < 25000)) %>% 
  mutate(cat_km35000 = (RA_ANNUAL_KMS_DRIVEN_SYSTEM >= 25000 & RA_ANNUAL_KMS_DRIVEN_SYSTEM < 35000)) %>% 
  mutate(cat_km99999 = RA_ANNUAL_KMS_DRIVEN_SYSTEM > 35000)

data <- data %>% 
  mutate(cat_age18 = RA_DRIVERAGE < 18) %>% 
  mutate(cat_age25 = (RA_DRIVERAGE >= 18 & RA_DRIVERAGE < 25)) %>% 
  mutate(cat_age30 = (RA_DRIVERAGE >= 25 & RA_DRIVERAGE < 30)) %>% 
  mutate(cat_age50 = (RA_DRIVERAGE >= 30 & RA_DRIVERAGE < 50)) %>% 
  mutate(cat_age70 = (RA_DRIVERAGE >= 50 & RA_DRIVERAGE < 70)) %>% 
  mutate(cat_age99 = RA_DRIVERAGE >= 70)

data <- data %>% 
  mutate(cat_ageveh0 = RA_VEHICLE_AGE < 0) %>% 
  mutate(cat_ageveh3 = (RA_VEHICLE_AGE >= 0 & RA_VEHICLE_AGE < 3)) %>% 
  mutate(cat_ageveh6 = (RA_VEHICLE_AGE >= 3 & RA_VEHICLE_AGE < 6)) %>% 
  mutate(cat_ageveh10 = (RA_VEHICLE_AGE >= 6 & RA_VEHICLE_AGE < 10)) %>% 
  mutate(cat_ageveh20 = (RA_VEHICLE_AGE >= 10 & RA_VEHICLE_AGE < 20)) %>% 
  mutate(cat_ageveh99 = RA_VEHICLE_AGE >= 20)

data <- data %>% 
  mutate(civil = ifelse(RA_MARITALSTATUS == "M", "M", "O")) %>% 
  mutate(RA_GENDER = ifelse(RA_GENDER == "NOT AVAILABLE", "UNKNOWN", RA_GENDER)) %>% 
  mutate(use = ifelse(substr(RA_VEH_USE, 1, 7) == "Commute", "Commute", "Other"))

data <- data %>% 
  mutate(x1 = cat_age18) %>% 
  mutate(x2 = cat_age25) %>% 
  mutate(x3 = cat_age70) %>% 
  mutate(x4 = cat_age99) %>% 
  mutate(x5 = RA_GENDER == "FEMALE") %>% 
  mutate(x6 = RA_GENDER == "UNKNOWN") %>% 
  mutate(x7 = cat_ageveh0) %>% 
  mutate(x8 = cat_ageveh6) %>% 
  mutate(x9 = cat_ageveh10) %>% 
  mutate(x10 = cat_ageveh20) %>% 
  mutate(x11 = civil == "O") %>% 
  mutate(x12 = use == "Other") %>%
  mutate(nbrtot = RA_ACCIDENT_IND) %>% 
  dplyr::select(expo, lnexpo, expo2, lnexpo2, x1, x2, x3, x4, x5, 
         x6, x7, x8, x9, x10, x11, x12, nbrtot, RA_PARTITION)

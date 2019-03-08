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
select <- dplyr::select


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
  mutate(x3 = cat_age50) %>% 
  mutate(x4 = cat_age70) %>% 
  mutate(x5 = cat_age99) %>% 
  mutate(x6 = RA_GENDER == "FEMALE") %>% 
  mutate(x7 = RA_GENDER == "UNKNOWN") %>% 
  mutate(x8 = cat_ageveh0) %>% 
  mutate(x9 = cat_ageveh6) %>% 
  mutate(x10 = cat_ageveh10) %>% 
  mutate(x11 = cat_ageveh20) %>% 
  mutate(x12 = cat_ageveh99) %>% 
  mutate(x13 = civil == "O") %>% 
  mutate(x14 = use == "Other") %>%
  mutate(nbrtot = RA_ACCIDENT_IND) %>% 
  select(expo, lnexpo, expo2, lnexpo2, x1, x2, x3, x4, x5, 
         x6, x7, x8, x9, x10, x11, x12, x13, x14, nbrtot, RA_PARTITION)

data %>% select(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) %>% summary
data %>% select(nbrtot) %>% as.matrix %>% table

data %>% select(expo, expo2) %>% summary

# Seperate dataset 

data_test <- data %>% filter(RA_PARTITION == "TEST") %>% select(-RA_PARTITION)
data_train <- data %>% filter(RA_PARTITION == "TRAIN") %>% select(-RA_PARTITION)


# Modeles classiques ------------------------------------------------------

glm.poisson <- glm(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                     offset(lnexpo), family = poisson, data = data_train)

glm.nb2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + offset(lnexpo), 
                  family = NBI, data = data_train)

glm.nb1 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + offset(lnexpo), 
                  family = NBII, data = data_train)

glm.pig <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + offset(lnexpo), 
                  family = PIG, data = data_train)

glm.zip <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + offset(lnexpo), 
                  family = ZIP, data = data_train, method = RS(1000))

glm.poisson$aic
glm.nbrtot$aic
glm.nb1$aic
glm.pig$aic
glm.zip$aic



data.frame("Obervé" = data_train %>% select(nbrtot) %>% table,
           "Poisson" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.poisson, newdata = data_train))))),
           "NB2" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.nbrtot, newdata = data_train))))),
           "NB1" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.nb1, newdata = data_train))))),
           "PIG" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.pig, newdata = data_train))))),
           "ZIP" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.zip, newdata = data_train))))))

data.frame("Obervé" = data_test %>% select(nbrtot) %>% table,
           "Poisson" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.poisson, newdata = data_test))))),
           "NB2" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nbrtot, newdata = data_test))))),
           "NB1" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb1, newdata = data_test))))),
           "PIG" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.pig, newdata = data_test))))),
           "ZIP" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.zip, newdata = data_test))))))

# Avec kilometrage --------------------------------------------------------

data <- data %>% mutate(x15 = expo2 < 0.5) %>% 
  mutate(x16 = (expo2 >= 0.5 & expo2 < 1)) %>% 
  mutate(x17 = (expo2 >= 1 & expo2 < 1.5)) %>% 
  mutate(x18 = (expo2 >= 1.5 & expo2 < 2)) %>% 
  mutate(x19 = (expo2 >= 2 & expo2 < 2.5))

data_test <- data %>% filter(RA_PARTITION == "TEST") %>% select(-RA_PARTITION)
data_train <- data %>% filter(RA_PARTITION == "TRAIN") %>% select(-RA_PARTITION)

glm.poisson.km <- glm(nbrtot ~ 1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                        x15 + x16 + x17 + x18 + x19 + 
                        offset(lnexpo), family = poisson, data = data_train)

glm.nb2.km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                          x15 + x16 + x17 + x18 + x19 + offset(lnexpo), 
                     family = NBI, data=data_train)

glm.nb1.km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                       x15 + x16 + x17 + x18 + x19 + offset(lnexpo), 
                     family = NBII, data=data_train)

glm.pig.km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                       x15 + x16 + x17 + x18 + x19 + offset(lnexpo), 
                     family = PIG, data=data_train)

glm.zip.km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                       x15 + x16 + x17 + x18 + x19 + offset(lnexpo), 
                     family = ZIP, data=data_train, method = RS(1000))


glm.poisson.km$aic
glm.nb2.km$aic
glm.nb1.km$aic
glm.pig.km$aic
glm.zip.km$aic

data.frame("Obervé" = data_train %>% select(nbrtot) %>% table,
           "Poisson" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.poisson.km, newdata = data_train))))),
           "NB2" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.nb2.km, newdata = data_train))))),
           "NB1" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.nb1.km, newdata = data_train))))),
           "PIG" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.pig.km, newdata = data_train))))),
           "ZIP" = sapply(0:2, function(t) sum(dpois(t, exp(predict(glm.zip.km, newdata = data_train))))))

data.frame("Obervé" = data_test %>% select(nbrtot) %>% table,
           "Poisson" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.poisson.km, newdata = data_test))))),
           "NB2" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb2.km, newdata = data_test))))),
           "NB1" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb1.km, newdata = data_test))))),
           "PIG" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.pig.km, newdata = data_test))))),
           "ZIP" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.zip.km, newdata = data_test))))))


# Fit GAMLSS order 2 ------------------------------------------------------

gamlss.poisson.order2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                  pb(expo2, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                                  pb(expo, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                                family = PO, data = data_train)

gamlss.nb2.order2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = NBI, data = data_train)

gamlss.nb1.order2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = NBII, data = data_train)

gamlss.pig.order2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = PIG, data = data_train, method = mixed(2,40))

gamlss.zip.order2 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = ZIP, data = data_train, method = mixed(2,40))

gamlss.poisson.order2$aic
gamlss.nb2.order2$aic
gamlss.nb1.order2$aic
gamlss.pig.order2$aic
gamlss.zip.order2$aic


# Fit GAMLSS order 3 ------------------------------------------------------

gamlss.poisson.order3 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                  pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                                  pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                family = PO, data = data_train)

gamlss.nb2.order3 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = NBI, data = data_train)

gamlss.nb1.order3 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = NBII, data = data_train)

gamlss.pig.order3 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = PIG, data = data_train, method=mixed(2,20))

gamlss.zip.order3 <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                              pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = ZIP, data = data_train, method=mixed(2,20))


gamlss.poisson.order3$aic
gamlss.nb2.order3$aic
gamlss.nb1.order3$aic
gamlss.pig.order3$aic
gamlss.zip.order3$aic


# Move regressors to dispersion -------------------------------------------

gamlss.nb2.order3_distortion_d <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                           pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                         sigma.formula = ~pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                         family = NBII, data = data_train, method=mixed(2,20))

gamlss.nb1.order3_distortion_d <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                           pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                         sigma.formula = ~pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                         family = NBI, data = data_train, method=mixed(2,20))

gamlss.nb2.order3_distortion_km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                            pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                          sigma.formula = ~pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                          family = NBII, data = data_train, method=mixed(2,20))

gamlss.nb1.order3_distortion_km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                            pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                          sigma.formula = ~pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                          family = NBI, data = data_train, method=mixed(2,20))

gamlss.zip.order3_distortion_d <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                           pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                         sigma.formula = ~pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                         family = ZIP, data = data_train, method=mixed(2,20))

gamlss.zip.order3_distortion_km <- gamlss(nbrtot ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + 
                                            pb(expo, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                          sigma.formula = ~pb(expo2, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                          family = ZIP, data = data_train, method=mixed(2,20))

gamlss.nb2.order3_distortion_d$aic
gamlss.nb2.order3_distortion_km$aic
gamlss.nb1.order3_distortion_d$aic
gamlss.nb1.order3_distortion_km$aic
gamlss.zip.order3_distortion_d$aic
gamlss.zip.order3_distortion_km$aic

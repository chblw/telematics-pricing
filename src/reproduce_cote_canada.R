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
  mutate(nb2 = RA_ACCIDENT_IND) %>% 
  mutate(km = RA_DISTANCE_DRIVEN) %>% 
  mutate(age = RA_DRIVERAGE) %>% 
  mutate(sexe = RA_GENDER) %>% 
  mutate(d = RA_exposure_time) %>% 
  mutate(ageveh = RA_VEHICLE_AGE) %>% 
  select(c(km, d, age, ageveh, sexe, nb2, RA_PARTITION))

# Reproduce section 3.2 ---------------------------------------------------

data %>% nrow()

# Figure 3.1
data %>% select(km) %>% as.matrix %>% hist(breaks = 1000)
data %>% select(km) %>% summary

# Figure 3.2
data %>% select(d) %>% as.matrix %>% hist(breaks = 1/0.05)
data %>% select(d) %>% summary()

# Tableau 3.1
data %>% select(nb2) %>% table
data %>% select(nb2) %>% summary

# Tableau 3.2
data %>% select(km, d, nb2) %>% summary
data %>% select(km, d, nb2) %>% var %>% sqrt %>% diag

# Figure 3.3
data %>% select(km, nb2) %>% group_by(km = 500 * floor(km / 500)) %>% 
  summarise_all(mean) %>% plot

# Figure 3.4
data %>% select(d, nb2) %>% group_by(d = 0.01 * floor(100 * d)) %>% 
  summarise_all(mean) %>% plot

# Tableau 3.3
data %>% select(age) %>% summary
data %>% select(ageveh) %>% summary

# Tableau 3.4
data %>% select(sexe) %>% table


# Reproduce section 3.3 ---------------------------------------------------

# Rename variables

data <- data %>% mutate(sexe = sexe == "MALE")
  
# Seperate dataset 

data_test <- data %>% filter(RA_PARTITION == "TEST") %>% select(-RA_PARTITION)
data_train <- data %>% filter(RA_PARTITION == "TRAIN") %>% select(-RA_PARTITION)

# Fit model 3.1 -----------------------------------------------------------

gam1.out <- gam(nb2 ~ 1 + s(km, bs="cr", k = 7) + s(d, bs="cr", k = 3), 
                family = poisson(link = log),
                data = data_train, scale = -1, gamma = 1)

summary(gam1.out)
plot(gam1.out)


# Fit model 3.2 -----------------------------------------------------------

gam2.out <- gam(nb2 ~ 1 + te(km, d, k = c(7, 3), bs = c("cr", "cr")), 
                family = poisson(link = log), 
                data = data_train, scale = -1, gamma = 1)

summary(gam2.out)

plot(gam2.out, scheme = 1, theta = -45)

gam2.out <- gam(nb2 ~ 1 + te(km, d, k = c(4, 3), bs = c("cr", "cr")), 
                family = poisson(link = log), 
                data = data_train, scale = -1, gamma = 1)

summary(gam2.out)

plot(gam2.out, scheme = 1, theta = -45)

# Fit model 3.3 -----------------------------------------------------------

data_model3.3 <- data_train %>% select(nb2, km, d, age) %>% 
  mutate(x1 = km <= 1000) %>% 
  mutate(x2 = (km > 5000 & km <= 10000)) %>% 
  mutate(x3 = (km > 10000 & km <= 15000)) %>% 
  mutate(x4 = (km > 15000 & km <= 20000)) %>% 
  mutate(x5 = km > 20000) %>% 
  mutate(x6 = age <= 25) %>% 
  mutate(x7 = (age > 25 & age <= 30))

glm3.out <- glm(nb2 ~ 1 + x1 + x2 + x3 + x4 + x5 + offset(I(log(d))), 
                family = poisson(link = log), data = data_model3.3)

summary(glm3.out)


# Fit models 3.4, 3.5 and 3.6 ---------------------------------------------

data_train <- data_train %>% mutate(x6 = age <= 25) %>% 
  mutate(x7 = (age > 25 & age <= 30))

gam4.out <- gam(nb2 ~ 1 + s(km, bs="cr", k = 7) + s(d, bs="cr", k = 3) + 
                  x6 + x7, family = poisson(link = log),
                data = data_train, scale = -1, gamma = 1)

gam4.out$coefficients[2:3]
summary(gam4.out)$se[2:3]
plot(gam4.out)


gam5.out <- gam(nb2 ~ 1 + te(km, d, k = c(7, 3), bs = c("cr", "cr")) + 
                  x6 + x7, family = poisson(link = log), 
                data = data_train, scale = -1, gamma = 1)

gam5.out$coefficients[2:3]
summary(gam5.out)$se[2:3]

plot(gam5.out, scheme = 1, theta = -45)

gam5.out <- gam(nb2 ~ 1 + te(km, d, k = c(4, 3), bs = c("cr", "cr")) + 
                  x6 + x7, family = poisson(link = log), 
                data = data_train, scale = -1, gamma = 1)

gam5.out$coefficients[2:3]
summary(gam5.out)$se[2:3]

plot(gam5.out, scheme = 1, theta = -45)

glm6.out <- glm(nb2 ~ 1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + offset(I(log(d))), 
                family = poisson(link = log), data = data_model3.3)

summary(glm6.out)$coefficients[7:8, 1:2]


# Modele tarifaire --------------------------------------------------------

pricing_data_base <- data.frame(km = rep(0, 5), d = rep(0, 5)) 
pricing_data_km <- data.frame(km = c(3500, 4500, 9000, 15500, 19000), d = rep(0, 5)) 
pricing_data_duration <- data.frame(km = rep(0, 5), d = c(0.35, 0.5, 0.65, 0.9, 1)) 
pricing_data <- data.frame(km = c(3500, 4500, 9000, 15500, 19000), d = c(0.35, 0.5, 0.65, 0.9, 1)) 

data.frame(rel_km = exp(predict.gam(gam1.out, pricing_data)) / 
             exp(predict.gam(gam1.out, pricing_data_duration)), 
           rel_d = exp(predict.gam(gam1.out, pricing_data)) / 
             exp(predict.gam(gam1.out, pricing_data_km)),
           rel_tot = exp(predict.gam(gam1.out, pricing_data)) / 
             exp(predict.gam(gam1.out, pricing_data_base)),
           premium = exp(predict.gam(gam1.out, pricing_data)))

data.frame(rel_km = exp(predict.gam(gam2.out, pricing_data)) / 
             exp(predict.gam(gam2.out, pricing_data_duration)), 
           rel_d = exp(predict.gam(gam2.out, pricing_data)) / 
             exp(predict.gam(gam2.out, pricing_data_km)),
           rel_tot = exp(predict.gam(gam2.out, pricing_data)) / 
             exp(predict.gam(gam2.out, pricing_data_base)),
           premium = exp(predict.gam(gam2.out, pricing_data)))

#######################################
# Title : fit_gam                     #
# Goal : Fit a GAM like Alepin (2018) #
# Author : Christopher Blier-Wong     #
# Date : January 2019                 #
#######################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
library(gamlss)
select <- dplyr::select
options(scipen = 999)

# Import data -------------------------------------------------------------

columns_not_in_2009_data <- c(86:94, 101:118)

data_original_2011 <- read_csv("src/data/Spain-2011.csv") %>% mutate(year = 2011)
data_original_2010 <- read_csv("src/data/Spain-2010.csv") %>% mutate(year = 2010)
data_original_2009 <- read_csv("src/data/Spain-2009.csv") %>% mutate(year = 2009)

data_original <- rbind(data_original_2009, 
                       data_original_2010[, -columns_not_in_2009_data], 
                       data_original_2011[, -columns_not_in_2009_data])

# Rename variables --------------------------------------------------------

data <- data_original %>% mutate(nb1 = N_mat_ase, nb2 = N_mat_con, nb3 = N_per_ase, nb4 = N_per_con) %>% 
  mutate(km = km_anuales) %>% 
  mutate(d = expo / 365) %>% 
  mutate(age = year - as.numeric(substr(fec_con, 1, 4))) %>% 
  mutate(ageveh = year - as.numeric(substr(fec_mat, 1, 4))) %>% 
  mutate(sexe = 1 * (sexo_con == "HOMBRE")) %>% 
  mutate(stn = 1 * (garaje == "Garaje privado")) %>% 
  dplyr::select(c(id, km, d, age, ageveh, sexe, stn, nb2, year, id))


# Summary statistics ------------------------------------------------------

# Tableau 3.1
data %>% select(year) %>% table
data %>% group_by(year) %>% select(year, id) %>% unique %>% count

# Tableau 3.3
data %>% select(nb2, d, km, age, ageveh) %>% summary
data %>% select(nb2, d, km, age, ageveh) %>% var %>% sqrt %>% diag

# Tableau 3.4 
data %>% select(sexe) %>% as.matrix %>% mean
data %>% select(stn) %>% as.matrix %>% mean

# Tableau 3.5
data %>% select(nb2) %>% table

# Figure 3.1
data %>% select(km) %>% as.matrix %>% hist(breaks = 2000)

# Figure 3.2
data %>% select(d) %>% as.matrix %>% hist(breaks = 1/0.025)

# Figure 3.3
data %>% select(km, nb2) %>% filter(km < 30000) %>% group_by(km = 500 * floor(km / 500)) %>% 
  summarise_all(mean) %>% plot

# Figure 3.4
data %>% select(d, nb2) %>% group_by(d = 0.01 * floor(100 * d)) %>% 
  summarise_all(mean) %>% plot


# Filter outliers ---------------------------------------------------------

data <- data %>% 
  filter(km < 30000) %>%
  filter(km > nb2)

# Create variables --------------------------------------------------------

data <- data %>% mutate(x2 = age <= 25) %>% 
  mutate(x3 = (age > 25 & age <= 30)) %>% 
  mutate(x4 = ageveh <= 2) %>% 
  mutate(x5 = (ageveh > 2 & ageveh <= 5)) %>% 
  mutate(x6 = (ageveh > 5 & ageveh <= 10)) %>% 
  mutate(x7 = sexe) %>% 
  mutate(x8 = stn) %>% 
  dplyr::select(c(id, km, d, x2, x3, x4, x5, x6, x7, x8, nb2))

# Seperate dataset --------------------------------------------------------

set.seed(32)

test_index <- data %>% select(id) %>% unique %>% as.matrix %>% sort %>% sample(14634, replace = FALSE)

data_train <- data %>% filter(!(id %in% test_index))
data_test <- data %>% filter(id %in% test_index)


# Modeles classiques ------------------------------------------------------

glm.poisson <- glm(nb2 ~ 1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                     offset(log(d)), family = poisson, data = data_train)

glm.nb2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + offset(log(d)), 
                  family = NBI, data=data_train)

glm.nb1 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + offset(log(d)), 
                  family = NBII, data=data_train)

glm.pig <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + offset(log(d)), 
                  family = PIG, data=data_train)

glm.zip <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + offset(log(d)), 
                  family = ZIP, data=data_train, method = RS(100))

glm.poisson$aic
glm.nb2$aic
glm.nb1$aic
glm.pig$aic
glm.zip$aic



data.frame("Poisson" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.poisson, newdata = data_train))))),
           "NB2" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.nb2, newdata = data_train))))),
           "NB1" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.nb1, newdata = data_train))))),
           "PIG" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.pig, newdata = data_train))))),
           "ZIP" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.zip, newdata = data_train))))), 
           "Obervé" = data_train %>% select(nb2) %>% table)

data.frame("Poisson" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.poisson, newdata = data_test))))),
           "NB2" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb2, newdata = data_test))))),
           "NB1" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb1, newdata = data_test))))),
           "PIG" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.pig, newdata = data_test))))),
           "ZIP" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.zip, newdata = data_test))))), 
           "Obervé" = data_test %>% select(nb2) %>% table)

# Avec kilometrage --------------------------------------------------------

data <- data %>% mutate(x9 = km < 5000) %>% 
  mutate(x10 = (km >= 5000 & km < 10000)) %>% 
  mutate(x11 = (km >= 10000 & km < 15000)) %>% 
  mutate(x12 = (km >= 15000 & km < 20000)) %>% 
  mutate(x13 = (km >= 20000 & km < 25000))

data_train <- data %>% filter(!(id %in% test_index))
data_test <- data %>% filter(id %in% test_index)

glm.poisson.km <- glm(nb2 ~ 1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                        x9 + x10 + x11 + x12 + x13 + 
                     offset(log(d)), family = poisson, data = data_train)

glm.nb2.km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                       x9 + x10 + x11 + x12 + x13 + offset(log(d)), 
                  family = NBI, data=data_train)

glm.nb1.km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                       x9 + x10 + x11 + x12 + x13 + offset(log(d)), 
                  family = NBII, data=data_train)

glm.pig.km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                       x9 + x10 + x11 + x12 + x13 + offset(log(d)), 
                  family = PIG, data=data_train)

glm.zip.km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                       x9 + x10 + x11 + x12 + x13 + offset(log(d)), 
                  family = ZIP, data=data_train, method = RS(100))


glm.poisson.km$aic
glm.nb2.km$aic
glm.nb1.km$aic
glm.pig.km$aic
glm.zip.km$aic

data.frame("Poisson" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.poisson.km, newdata = data_train))))),
           "NB2" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.nb2.km, newdata = data_train))))),
           "NB1" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.nb1.km, newdata = data_train))))),
           "PIG" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.pig.km, newdata = data_train))))),
           "ZIP" = sapply(0:4, function(t) sum(dpois(t, exp(predict(glm.zip.km, newdata = data_train))))), 
           "Obervé" = data_train %>% select(nb2) %>% table)

data.frame("Poisson" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.poisson.km, newdata = data_test))))),
           "NB2" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb2.km, newdata = data_test))))),
           "NB1" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.nb1.km, newdata = data_test))))),
           "PIG" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.pig.km, newdata = data_test))))),
           "ZIP" = sapply(0:3, function(t) sum(dpois(t, exp(predict(glm.zip.km, newdata = data_test))))), 
           "Obervé" = data_test %>% select(nb2) %>% table)


# Fit GAMLSS order 2 ------------------------------------------------------

gamlss.poisson.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                           pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                family = PO, data = data_train)

gamlss.nb2.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                           pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                           pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                         family = NBI, data = data_train)

gamlss.nb1.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = NBII, data = data_train)

gamlss.pig.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = PIG, data = data_train)

gamlss.zip.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = ZIP, data = data_train)

gamlss.pig.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = PIG, data = data_train, method = mixed(2,40))

gamlss.zip.order2 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 2, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 2, method = "GAIC")), 
                            family = ZIP, data = data_train, method = mixed(2,40))

gamlss.poisson.order2$aic
gamlss.nb2.order2$aic
gamlss.nb1.order2$aic
gamlss.pig.order2$aic
gamlss.zip.order2$aic


# Fit GAMLSS order 3 ------------------------------------------------------

gamlss.poisson.order3 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                  pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                                  pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                family = PO, data = data_train)

gamlss.nb2.order3 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = NBI, data = data_train)

gamlss.nb1.order3 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = NBII, data = data_train)

gamlss.pig.order3 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = PIG, data = data_train, method=mixed(2,20))

gamlss.zip.order3 <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                              pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")) +
                              pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                            family = ZIP, data = data_train, method=mixed(2,20))


gamlss.poisson.order3$aic
gamlss.nb2.order3$aic
gamlss.nb1.order3$aic
gamlss.pig.order3$aic
gamlss.zip.order3$aic


# Move regressors to dispersion -------------------------------------------

gamlss.nb2.order3_distortion_d <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                         pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = NBII, data = data_train, method=mixed(2,20))

gamlss.nb1.order3_distortion_d <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                         pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = NBI, data = data_train, method=mixed(2,20))

gamlss.nb2.order3_distortion_km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                         pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = NBII, data = data_train, method=mixed(2,20))

gamlss.nb1.order3_distortion_km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                         pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = NBI, data = data_train, method=mixed(2,20))

gamlss.zip.order3_distortion_d <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                       pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = ZIP, data = data_train, method=mixed(2,20))

gamlss.zip.order3_distortion_km <- gamlss(nb2 ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                                         pb(d, control = pb.control(inter = 35, order = 3, method = "GAIC")),
                                       sigma.formula = ~pb(km, control = pb.control(inter = 35, order = 3, method = "GAIC")), 
                                       family = ZIP, data = data_train, method=mixed(2,20))

gamlss.nb2.order3_distortion_d$aic
gamlss.nb2.order3_distortion_km$aic
gamlss.nb1.order3_distortion_d$aic
gamlss.nb1.order3_distortion_km$aic
gamlss.zip.order3_distortion_d$aic
gamlss.zip.order3_distortion_km$aic

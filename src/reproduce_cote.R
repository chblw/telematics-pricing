###################################
# Title : reproduce_cote          #
# Goal : Replicate mémoire (2016) #
# Author : Christopher Blier-Wong #
# Date : January 2019             #
###################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
library(mgcv)

# Import data -------------------------------------------------------------

data_original <- read_csv("src/data/Spain-2011.csv")
data_original <- as.tibble(data_original)


# Rename variables --------------------------------------------------------

data <- data_original %>% 
  mutate(nb1 = N_mat_ase, nb2 = N_mat_con, nb3 = N_per_ase, nb4 = N_per_con) %>% 
  mutate(km = km_anuales) %>% 
  mutate(d = expo / 365) %>% 
  mutate(age = 2011 - as.numeric(substr(fec_con, 1, 4))) %>% 
  mutate(ageveh = 2011 - as.numeric(substr(fec_mat, 1, 4))) %>% 
  mutate(sexe = 1 * (sexo_con == "HOMBRE")) %>% 
  mutate(stn = 1 * (garaje == "Garaje privado")) %>% 
  select(c(km, d, age, ageveh, sexe, stn, nb2))


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
data %>% select(sexe) %>% as.matrix %>% mean
data %>% select(stn) %>% as.matrix %>% mean


# Reproduce section 3.3 ---------------------------------------------------

# Rename variables

data <- data_original %>% 
  mutate(nb1 = N_mat_ase, nb2 = N_mat_con, nb3 = N_per_ase, nb4 = N_per_con) %>% 
  mutate(km = km_anuales) %>% 
  mutate(d = expo / 365) %>% 
  mutate(age = 2011 - as.numeric(substr(fec_con, 1, 4))) %>% 
  mutate(ageveh = 2011 - as.numeric(substr(fec_mat, 1, 4))) %>% 
  mutate(sexe = 1 * (sexo_con == "HOMBRE")) %>% 
  mutate(stn = 1 * (garaje == "Garaje privado")) %>% 
  dplyr::select(c(km, d, age, ageveh, sexe, stn, nb2)) %>% 
  filter(nb2 < km)

# Seperate dataset 

set.seed(23)

test_index <- sort(sample(1:nrow(data), 5000))

data_train <- data[-test_index, ]
data_test <- data[test_index, ]

# Different que Steven ici car je sous-echantillonne sur les observations
# retenues et non toutes les observations. Pour cette raison, 55862 donnees
# sont differentes. Ainsi, ce decallage cause que 4293/66486 des observations 
# ne sont pas equivalantes.


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
plot(gam2.out)


# Fit model 3.3 -----------------------------------------------------------

data_model3.3 <- data_train %>% select(nb2, km, d, age) %>% 
  mutate(x1 = km <= 1000) %>% 
  mutate(x2 = (km > 5000 & km <= 10000)) %>% 
  mutate(x3 = (km > 10000 & km <= 15000)) %>% 
  mutate(x4 = (km > 15000 & km <= 20000)) %>% 
  mutate(x5 = km > 20000) %>% 
  mutate(x6 = age <= 25) %>% 
  mutate(x7 = (age > 25 & age <= 30))

glm3.out <- glm(nb2 ~ 1 + x1 + x2 + x3 + x4 + x5 + offset(log(d)), 
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

gam5.out <- gam(nb2 ~ 1 + te(km, d, k = c(7, 3), bs = c("cr", "cr")) + 
                x6 + x7, family = poisson(link = log), 
                data = data_train, scale = -1, gamma = 1)

gam5.out$coefficients[2:3]
summary(gam5.out)$se[2:3]

glm6.out <- glm(nb2 ~ 1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + offset(log(d)), 
                family = poisson(link = log), data = data_model3.3)

summary(glm6.out)$coefficients[7:8, 1:2]


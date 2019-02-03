#####################################
# Title : fit_gam                   #
# Goal : Fit a GAM like Coté (2017) #
# Author : Christopher Blier-Wong   #
# Date : January 2019               #
#####################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
library(mgcv)

# Import data -------------------------------------------------------------

data_original <- read_csv("data/Spain-2011.csv")
data_original <- as.tibble(data_original)

# Rename variables --------------------------------------------------------

data <- data_original %>% mutate(nb1 = N_mat_ase, nb2 = N_mat_con, nb3 = N_per_ase, nb4 = N_per_con) %>% 
  mutate(km = km_anuales) %>% 
  mutate(d = expo / 365) %>% 
  mutate(age = 2011 - as.numeric(substr(fec_con, 1, 4))) %>% 
  mutate(ageveh = 2011 - as.numeric(substr(fec_mat, 1, 4))) %>% 
  mutate(sexe = 1 * (sexo_con == "HOMBRE")) %>% 
  mutate(stn = 1 * (garaje == "Garaje privado")) %>% 
  dplyr::select(c(km, d, age, ageveh, sexe, stn, nb2)) %>% 
  # filter(km < 30000) %>%
  filter(km > nb2)

# Seperate dataset --------------------------------------------------------

set.seed(23)

train_index <- sample(1:nrow(data), nrow(data) - 5000)

data_train <- data[train_index, ]
data_test <- data[-train_index, ]

# set.seed(23)
# 
# test_index <- sample(1:nrow(data), 5000, replace = TRUE)
# 
# data_train <- data[-test_index, ]
# data_test <- data[test_index, ]

# Fit GAM -----------------------------------------------------------------

gam1.out <- gam(nb2 ~ 1 + s(km, k = 9) + s(d, k = 5), 
               family = poisson(link = log),
               data = data_train)

gam1.out$coefficients
summary(gam1.out)
# plot(gam1.out)
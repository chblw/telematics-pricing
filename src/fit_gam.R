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

data_original <- read_csv("src/data/Spain-2011.csv")
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
  filter(nb2 < km)

# Seperate dataset --------------------------------------------------------

set.seed(23)

test_index <- sort(sample(1:nrow(data), 5000))

data_train <- data[-test_index, ]
data_test <- data[test_index, ]

# Different que Steven ici car je sous-echantillonne sur les observations
# retenues et non toutes les observations. Pour cette raison, 55862 donnees
# sont differentes. Ainsi, ce decallage cause que 4293/66486 des observations 
# ne sont pas equivalantes.

# Fit GAM -----------------------------------------------------------------

gam1.out <- gam(nb2 ~ 1 + s(km, bs="cr", k = 7) + s(d, bs="cr", k = 3), 
               family = poisson(link = log),
               data = data_train, scale = -1, gamma = 1)

summary(gam1.out)
plot(gam1.out)


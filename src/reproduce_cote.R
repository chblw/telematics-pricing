###################################
# Title : reproduce_cote          #
# Goal : Replicate mémoire (2016) #
# Author : Christopher Blier-Wong #
# Date : January 2019             #
###################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)

# Import data -------------------------------------------------------------

data <- read_csv("data/Spain-2011.csv")
data <- as.tibble(data)

# Rename variables --------------------------------------------------------

data <- data %>% mutate(nb1 = N_mat_ase, nb2 = N_mat_con, nb3 = N_per_ase, nb4 = N_per_con) %>% 
  mutate(km = km_anuales) %>% 
  mutate(d = expo / 365) %>% 
  mutate(age = 2011 - as.numeric(substr(fec_con, 1, 4))) %>% 
  mutate(ageveh = 2011 - as.numeric(substr(fec_mat, 1, 4))) %>% 
  mutate(sexe = 1 * (sexo_con == "HOMBRE")) %>% 
  mutate(stn = 1 * (garaje == "Garaje privado")) %>% 
  select(c(km, d, age, ageveh, sexe, stn, nb2))

# Reproduce explanatory analysis ------------------------------------------

data %>% select(km) %>% as.matrix %>% hist(breaks = 1000)
data %>% select(km) %>% summary

data %>% select(d) %>% as.matrix %>% hist(breaks = 1/0.05)
data %>% select(d) %>% summary()

data %>% select(nb2) %>% table
data %>% select(nb2) %>% summary

data %>% select(km, nb2) %>% group_by(km = 500 * floor(km / 500)) %>% summarise_all(mean) %>% plot
data %>% select(d, nb2) %>% group_by(d = 0.01 * floor(100 * d)) %>% summarise_all(mean) %>% plot

data %>% select(age) %>% summary
data %>% select(ageveh) %>% summary

data %>% select(sexe) %>% as.matrix %>% mean
data %>% select(stn) %>% as.matrix %>% mean

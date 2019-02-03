###################################
# Title : exploratory_analysis    #
# Goal : Replicate Boucher (2013) #
# Author : Christopher Blier-Wong #
# Date : January 2019             #
###################################

# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
subtract <- function(x, y) x - y

# Import data -------------------------------------------------------------

data <- read_csv("data/Spain-2011.csv")
data <- as.tibble(data)
data <- data %>% filter(expo == 365)

# Explore data ------------------------------------------------------------

names(data)
nrow(data)

data %>% select(km_anuales) %>% as.matrix() %>% mean()
data %>% select(km_anuales) %>% as.matrix() %>% var() %>% sqrt

data %>% select(N_mat_ase) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_mat_con) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_per_ase) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_per_con) %>% as.matrix() %>% table() / nrow(data)

data %>% select(km_anuales) %>% as.matrix() %>% hist(breaks = 200)

data %>% select(km_anuales) %>% as.matrix %>% summary()

data %>% select(fec_con) %>% as.matrix %>% substr(start = 1, stop = 4) %>% as.numeric %>% subtract(x = 2011) %>% summary

data %>% select(fec_carne) %>% as.matrix %>% substr(start = 1, stop = 4) %>% as.numeric %>% subtract(x = 2011) %>% summary

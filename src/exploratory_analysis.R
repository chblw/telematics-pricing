##############################################
# Title : exploratory_analysis               #
# Goal : Replicate Boucher (2013) statistics #
# Author : Christopher Blier-Wong            #
# Date : January 2019                        #
##############################################


# Library and source functions --------------------------------------------

library(readr)
library(tidyverse)
subtract <- function(x, y) x - y


# Import data -------------------------------------------------------------

data <- read_csv("src/data/Spain-2011.csv")
data <- as.tibble(data)
data <- data %>% filter(expo == 365)


# Explore data ------------------------------------------------------------

names(data)
nrow(data)


# Table 1a ----------------------------------------------------------------

data %>% select(N_mat_ase) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_mat_con) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_per_ase) %>% as.matrix() %>% table() / nrow(data)
data %>% select(N_per_con) %>% as.matrix() %>% table() / nrow(data)


# Figure 1 ----------------------------------------------------------------

data %>% select(km_anuales) %>% as.matrix() %>% hist(breaks = 200)
data %>% select(km_anuales) %>% as.matrix %>% summary()

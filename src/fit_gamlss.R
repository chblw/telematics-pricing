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

# Fit regression ----------------------------------------------------------

glm.poisson <- glm(nb2 ~ 1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                   offset = log(d), family = poisson, 
                   data = data_train)

summary(glm.poisson)

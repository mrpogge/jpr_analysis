#-------------------------------------------------------------------------------
# Data wrangling
#-------------------------------------------------------------------------------
#                             Description
# This script is used to calculate to make the data long, create effect coding 
# and calculate the error variables. The result is saved to data/dat_long.csv.

#-------------------------------------------------------------------------------
# Dependencies
library(haven)
library(tidyverse)

#-------------------------------------------------------------------------------
# Reading data

dat = read_sav("data/jpr_dat.sav")

#creating long data
dat_long = dat %>%
  mutate(ID = as.factor(1:nrow(dat))) %>%
  pivot_longer(cols = c(3:ncol(dat)), names_to = "item", values_to = "score")

# Creating effect coding
# - ACT / PASS - active passive rotation
# - 150 / 30 - start position in degree
# - 60/90/120 - end position in degree
# - 12/24 - speed of back rotation
# - 2/4 - seconds spendt in the goal position

# create separate variables for all values deliminted by "_" in item
dat_long = dat_long %>%
  separate(item, into = c("act_pass", "start", "end", "speed", "time"), sep = "_") %>%
  mutate(act_pass = factor(act_pass, levels = c("ACT", "PASS")),
         speed = factor(speed, levels = c("12", "24"), ordered = TRUE),
         time = factor(time, levels = c("2", "4"), ordered = TRUE))
#-------------------------------------------------------------------------------
# Cleaning data
# make the scores missing if it is larger than the difference between the start and end

dat_long = dat_long %>%
  mutate(score = ifelse((abs(score-as.numeric(end)) > abs(as.numeric(end)-as.numeric(start))),
                        NA, score)) %>%
  filter(!is.na(score))

#create error variables and make the id in the same format
dat_long = dat_long %>%
  mutate(end = as.numeric(end),
         start = as.numeric(start),
         dist = as.factor(end-start),
         squared_error = (score - end)^2,
         squared_error = ifelse(squared_error != 0, 
                                squared_error, 
                                min(squared_error[squared_error != 0])), #minimum imputing
         error_direction = factor(ifelse(score-end > 0, "over", "under"), levels = c("under", "over")),
         abs_error = abs(score - end),
         abs_error = ifelse(abs_error != 0, 
                            abs_error, 
                            min(abs_error[abs_error != 0])),
         direction = as.factor(ifelse(end-start > 0, "out", "in")),
         abs_dist = factor(abs(end-start), ordered = FALSE),
         act_pass = factor(act_pass, levels = c("ACT", "PASS")),
         start = factor(start, levels = c("150", "30")),
         end = factor(end, levels = c("60", "90", "120")),
         speed = factor(speed, levels = c("12", "24"), ordered = FALSE),
         time = factor(time, levels = c("2", "4"), ordered = FALSE))

#-------------------------------------------------------------------------------
# Save data
saveRDS(dat_long, "data/dat_long.rds")




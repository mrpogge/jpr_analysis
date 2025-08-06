#-------------------------------------------------------------------------------
# Exploratory Data Analysis
#-------------------------------------------------------------------------------
#                             Description
# This script is used to calculate descriptive statistics and make plots about the
# data prior to the analysis. The figures are saved to the folder figures/.

#-------------------------------------------------------------------------------
# Dependencies
library(tidyverse)

#-------------------------------------------------------------------------------
# Read long data

dat_long = read_csv("data/dat_long.csv")

#-------------------------------------------------------------------------------
# Distribution of errors

# absolute error histogram
abs_hist = ggplot(dat_long, aes(x = absolute_distance)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Hello",
       x = "Absolute Error in degrees",
       y = "Absolute Frequency") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# systematic error histogram
sys_hist = ggplot(dat_long, aes(x = systematic_distance)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Systematic errors",
       x = "Systematic error",
       y = "Frequency") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# over-under histogram
over_under_hist = ggplot(dat_long, aes(x = is_error_direction)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Over/Under errors",
       x = "Error direction",
       y = "Frequency") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#------------------------------------------------------------------------------
# Number of NA-s 
dat_long %>% summarise(n_NA = sum(is.na(absolute_distance)),
                       n_NA_act = sum(is.na(act_pass)),
                       n_NA_start = sum(is.na(start)),
                       n_NA_end = sum(is.na(end)),
                       n_NA_speed = sum(is.na(speed)),
                       n_NA_time = sum(is.na(time)))

dat_long %>%
  summarise(m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE),
            m_abs = median(absolute_distance, na.rm = TRUE),
            sd_abs = IQR(absolute_distance, na.rm = TRUE))

#------------------------------------------------------------------------------
# Marginal means plots

start_me = dat_long %>%
  group_by(start) %>%
  summarise(m_abs = mean(absolute_distance, na.rm = TRUE),
            sd_abs = sd(absolute_distance, na.rm = TRUE),
            m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE))

end_me = dat_long %>%
  group_by(end) %>%
  summarise(m_abs = mean(absolute_distance, na.rm = TRUE),
            sd_abs = sd(absolute_distance, na.rm = TRUE),
            m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE))

startXend = dat_long %>%
  group_by(start, end) %>%
  summarise(m_abs = mean(absolute_distance, na.rm = TRUE),
            sd_abs = sd(absolute_distance, na.rm = TRUE),
            m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE)) %>%
  mutate(dist = end-start)

startXspeed = dat_long %>%
  group_by(start, speed) %>%
  summarise(m_abs = mean(absolute_distance, na.rm = TRUE),
            sd_abs = sd(absolute_distance, na.rm = TRUE),
            m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE)) 

endXspeed = dat_long %>%
  group_by(end, speed) %>%
  summarise(m_abs = mean(absolute_distance, na.rm = TRUE),
            sd_abs = sd(absolute_distance, na.rm = TRUE),
            m_sys = mean(systematic_distance, na.rm = TRUE),
            sd_sys = sd(systematic_distance, na.rm = TRUE)) 

start_me_plt = dat_long %>%
  ggplot(aes(x = factor(start), y = systematic_distance, group = ID_2)) +
  geom_line(alpha = 0.2) +
  geom_errorbar(aes(ymin = m_abs - sd_abs, ymax = m_abs + sd_abs), width = 0.1) +
  labs(title = "Marginal means of start position",
       x = "Start position",
       y = "Absolute error") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())





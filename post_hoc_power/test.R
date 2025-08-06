#-------------------------------------------------------------------------------
# In this script I estimate the achievable power of the full glmm model
#-------------------------------------------------------------------------------

# Dependencies
library(tidyverse)
library(lme4)
library(car)
library(performance)

# using sum coding (orthogonal contrast) to get interpretable type III results
options(contrasts = c("contr.sum","contr.poly"))

#-------------------------------------------------------------------------------
# Reading the long data format

dat_long = read_csv("data/dat_long.csv")

#-------------------------------------------------------------------------------
# Removing NA-s, since the only source of missingness comes from the dependent variables, we can remove all rows with missing values

dat_analysis = dat_long %>%
  filter(!is.na(score)) %>%
  mutate(dist = as.factor(end-start),
         squared_error = ifelse(squared_error != 0, 
                                squared_error, 
                                min(squared_error[squared_error != 0])), #minimum imputing
         direction = as.factor(ifelse(end-start > 0, "out", "in")),
         abs_dist = factor(abs(end-start), ordered = FALSE),
         act_pass = factor(act_pass, levels = c("ACT", "PASS")),
         start = factor(start, levels = c("150", "30")),
         end = factor(end, levels = c("60", "90", "120")),
         speed = factor(speed, levels = c("12", "24"), ordered = FALSE),
         time = factor(time, levels = c("2", "4"), ordered = FALSE),
         is_error_direction = factor(is_error_direction, levels = c("under", "over")))

#-------------------------------------------------------------------------------
# fitting the full model
m1 =  glmer(squared_error ~ (abs_dist + direction + is_error_direction + act_pass + speed + time)^3 + (1|ID_2), 
              data = dat_analysis,
              control = glmerControl(optimizer = "bobyqa"),
              family = Gamma(link = "log"))

m1_twoway = glmer(squared_error ~ (abs_dist + direction + is_error_direction + act_pass + speed + time)^2 + (1|ID_2), 
                  data = dat_analysis,
                  control = glmerControl(optimizer = "bobyqa"),
                  family = Gamma(link = "log"))

m1_anova = car::Anova(m1_twoway, type = "III")
r2_nakagawa(m1)$R2_marginal
r2_nakagawa(m1_twoway)$R2_marginal
anova(m1, m1_twoway)

terms_to_test = rownames(m1_anova)[2:length(rownames(m1_anova))]

for (i in seq_along(terms_to_test)) {
  term <- terms_to_test[i]
  
  # Create reduced model formula (drop one term)
  reduced_formula <- as.formula(
    paste("squared_error ~", 
          paste(setdiff(terms_to_test, term), collapse = " + "),
          "+ (1 | ID_2)")
  )
  
  # Fit reduced model
  reduced_model <- glmer(
    reduced_formula,
    data = dat_analysis,
    control = glmerControl(optimizer = "bobyqa"),
    family = Gamma(link = "log")
  )
  
  # Get marginal R² of reduced model
  reduced_r2 <- r2_nakagawa(reduced_model)$R2_marginal
  
  # Store results
  delta_r2_results$R2_dropped[i] <- reduced_r2
  delta_r2_results$Delta_R2[i] <- full_r2 - reduced_r2
}

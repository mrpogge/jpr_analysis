#-------------------------------------------------------------------------------
# In this script I estimate the achievable power of the full glmm model
#-------------------------------------------------------------------------------

# Dependencies
library(tidyverse)
library(lme4)
library(car)

# using sum coding (orthogonal contrast) to get interpretable type III results
options(contrasts = c("contr.sum","contr.poly"))

#-------------------------------------------------------------------------------
# Reading the long data format

dat_long = readRDS("data/dat_long.rds")

#-------------------------------------------------------------------------------
# get fitted models

m_theoretical = readRDS("m_theoretical.rds")
m_overshoot = readRDS("m_overshoot.rds")

#-------------------------------------------------------------------------------
# Generate data under the model and count whether the given effect is significant

glmm_power_sim = function(model, type){
  # generate data under the model
  sim_data = simulate(model, nsim = 1)[[1]]
  
  # Replace the original response with simulated data
  sim_df = model.frame(model)
  sim_df$dependent = sim_data
  
  #get model formula from model object
  sim_form = formula(model)
  
  #change the dependent variable to name "dependent" in the formula
  sim_form[[2]] = as.name("dependent")
  
  
  # Fit the model to the simulated data
  if(type == "gamma"){
    sim_model = glmer(sim_form, 
                      data = sim_df,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                      family = Gamma(link = "log"))
  } else {
    sim_model = glmer(sim_form, 
                      data = sim_df,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                      family = binomial(link = "logit"))
  }

  
  # Check if the interaction term is significant
  # Type III test
  anova_results = Anova(sim_model, type = "III")
  
  # Return p-values for selected effects
  return(as.numeric(anova_results$`Pr(>Chisq)` < 0.05))
}

# Run the simulations and store the p-values
set.seed(13181917)
sim_results_theoretical = replicate(200, glmm_power_sim(m_theoretical, "gamma"))
sim_results_overshoot = replicate(200, glmm_power_sim(m_overshoot, "binomial"))

power_estimates_theoretical = rowMeans(sim_results_theoretical, na.rm = TRUE)
power_estimates_overshoot = rowMeans(sim_results_overshoot, na.rm = TRUE)

# View power for each main effect and interaction
print(round(power_estimates_theoretical, 4))
print(round(power_estimates_overshoot, 3))




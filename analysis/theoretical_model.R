#-------------------------------------------------------------------------------
# Generalised Linear Mixed Models
#-------------------------------------------------------------------------------
#                             Description
#  In this script we fit the model built on previous results in the literature

#-------------------------------------------------------------------------------
# Dependencies
source("util.R")

library(tidyverse)
library(ggpubr)
library(patchwork)
library(officer)
library(flextable)

library(lme4)
library(emmeans)
library(moments)
library(MuMIn)



# using sum coding (orthogonal contrast) to get interpretable type III results
options(contrasts = c("contr.sum","contr.poly"))

#-------------------------------------------------------------------------------
# Reading the long data format

dat_long = readRDS("data/dat_long.rds")

#-------------------------------------------------------------------------------
# building the model formula and a dictionary for plotting and printing

model_formula = abs_error ~ 
  act_pass +
  direction +
  abs_dist + 
  speed + 
  time +
  error_direction + 
  abs_dist*direction*act_pass + 
  act_pass*speed*abs_dist*error_direction +
  act_pass*speed*abs_dist*direction +
  (1 | ID)


var_labels <- c(
  abs_dist    = "Amplitude",
  direction   = "Starting Position",
  error_direction   = "Error Direction",
  act_pass    = "Movement Type",
  speed       = "Speed",
  time        = "Time"
)

#-------------------------------------------------------------------------------
# Checking normality of residuals in the linear model
#-------------------------------------------------------------------------------
m_assumptions = lmer(model_formula, 
                     data = dat_long, 
                     control = lmerControl(optimizer = "bobyqa"))

resid_assumptions = resid(m_assumptions)

# Checking normality of residuals
agostino.test(resid_assumptions)
anscombe.test(resid_assumptions)

# Create Q-Q plot
qq_m_assumption <- ggplot(data.frame(resid_assumptions), aes(sample = resid_assumptions)) +
  stat_qq(size = 1) +
  stat_qq_line(color = "red", linewidth = 0.8) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_pubr(base_size = 12, border = FALSE)



#-------------------------------------------------------------------------------
# Model fitting: Generalised linear mixed model with log-link
#-------------------------------------------------------------------------------
m_theoretical = glmer(model_formula,
                      family = Gamma(link = "log"),
                      data = dat_long,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

#compare the generalised model
m_comparision = anova(m_assumptions, m_theoretical)


#check whether random effects is necessary
fixed_model_formula <- update(model_formula, . ~ . - (1 | ID))

m_fixed <- glm(fixed_model_formula,
                 family = Gamma(link = "log"),
                 data = dat_long)

anova(m_theoretical, m_fixed)

# check the two data sources
two_data_formula <- update(model_formula, . ~ . + (1 | study))
m_theoretical_twodata = glmer(two_data_formula,
                      family = Gamma(link = "log"),
                      data = dat_long,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
anova(m_theoretical_twodata, m_theoretical)

#save model
saveRDS(m_theoretical, "m_theoretical.rds")

#-------------------------------------------------------------------------------
# Model summary and ANOVA

#-------------------------------------------------------------------------------
# model fit
AIC(m_theoretical)
BIC(m_theoretical)
r.squaredGLMM(m_theoretical)

#pseduo r2
pseudo_r2 = effect_contributions_hierarchical(m_theoretical, dat_long)

anova_th = as.data.frame(car::Anova(m_theoretical, type = "III"))
anova_th$Effect <- rename_effects(rownames(anova_th), var_labels)
rownames(anova_th) <- NULL
anova_th <- anova_th %>%
  select(Effect, everything()) %>%
  mutate(pseudo_R2 = c(0, pseudo_r2$Pseudo_R2_Contribution))


anova_th <- anova_th %>%
  select(Effect, Chisq, Df, `Pr(>Chisq)`, pseudo_R2) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

ft <- flextable(anova_th) %>%
  set_header_labels(
    Effect = "Effect",
    Chisq = "Khi²",
    Df = "df",
    `Pr(>Chisq)` = "p",
    pseudo_R2 = "Pseudo R²"
  ) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table X. Wald Khi² Tests (Type III ANOVA) for Fixed Effects of the theoretical model", style = "heading 2") %>%
  body_add_flextable(ft) %>%
  body_add_par("Note. ??² = Wald chi-square test statistic. p < .05 is considered statistically significant.
               To fit the model we used the BOBYQA optimiser using the glmer function in the lme4 package.", style = "Normal")

print(doc, target = "figures/anova_table.docx")

#-------------------------------------------------------------------------------
# Post-hoc analysis and emmeans

#act_pass
emm_act_pass = emmeans(m_theoretical, ~ act_pass, type = "response")
emm_act_pass_df = as.data.frame(emm_act_pass)
contrast(emm_act_pass, method = "pairwise", adjust = "holm")

emm_act_pass_df = emm_act_pass_df %>%
  mutate(act_pass = recode(act_pass, 
                           "ACT" = "Active", 
                           "PASS" = "Passive"))
p_actpass = ggplot(emm_act_pass_df, aes(x = act_pass, y = response, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Movement Type",
    y = "Estimated Marginal Mean of Absolute Error"
  )  + 
  theme_pubr(base_size = 12, border = FALSE) 

#abs_dist
emm_abs_dist <- emmeans(m_theoretical, ~ abs_dist, type = "response")
emm_abs_dist_df <- as.data.frame(emm_abs_dist)
contrast(emm_abs_dist, method = "pairwise", adjust = "holm")

emm_abs_dist_df <- emm_abs_dist_df %>%
  mutate(abs_dist = paste0(abs_dist, "°")) # format absolute distance for better readability

p_absdist = ggplot(emm_abs_dist_df, aes(x = abs_dist, y = response, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude",
    y = "Estimated Marginal Mean of Absolute Error"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)

#speed
emm_speed <- emmeans(m_theoretical, ~ speed, type = "response")
emm_speed_df <- as.data.frame(emm_speed)
#contrast(emm_speed, method = "pairwise", adjust = "holm")

emm_speed_df <- emm_speed_df %>%
  mutate(speed = paste0(speed, "°/s")) # format speed for better readability

p_speed = ggplot(emm_speed_df, aes(x = speed, y = response, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Speed",
    y = "Estimated Marginal Mean of Absolute Error"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)

# patch plot for main effects
emm_main_effects = p_actpass + p_absdist + p_speed +
  plot_layout(ncol = 3)

ggsave(emm_main_effects, filename = "figures/emm_main_effects.png", width = 12, height = 4)

#directions x absolute distance
emm_dir_dist <- emmeans(m_theoretical, ~ direction * abs_dist, type = "response")
emm_dir_dist_df <- as.data.frame(emm_dir_dist) %>%
  mutate(abs_dist = paste0(abs_dist, "°")) %>% # format absolute distance for better readability
  mutate(direction = recode(direction, 
                           "in" = "120°", 
                           "out" = "30°")) # format direction for better readability
contrast(emm_dir_dist, by = "direction", adjust = "holm")
contrast(emm_dir_dist, by = "abs_dist", adjust = "holm")
contrast(emm_dir_dist, method = "pairwise", adjust = "holm")

p_dir_dist = ggplot(emm_dir_dist_df, aes(x = abs_dist, y = response, color = direction, group = direction)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude",
    y = "Estimated Marginal Mean of Absolute Error",
    color = "Starting Position"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)

#abs_dist x speed 
emm_dist_speed <- emmeans(m_theoretical, ~ abs_dist * speed, type = "response")
emm_dist_speed_df <- as.data.frame(emm_dist_speed) %>% 
  mutate(abs_dist = paste0(abs_dist, "°")) %>% # format absolute distance for better readability
  mutate(speed = paste0(speed, "°/s")) # format speed for better readability
contrast(emm_dist_speed, by = "abs_dist", adjust = "holm")
contrast(emm_dist_speed, by = "speed", adjust = "holm")
contrast(emm_dist_speed, method = "pairwise", adjust = "holm")

p_dist_speed = ggplot(emm_dist_speed_df, aes(x = abs_dist, y = response, color = speed, group = speed)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude",
    y = "",
    color = "Speed"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)


p_2interaction = p_dir_dist + p_dist_speed 

ggsave(p_2interaction, filename = "figures/emm_2interaction.png", width = 12, height = 4)



#abs_dist x speed x error_direction
emm_complex <- emmeans(m_theoretical, ~  abs_dist * speed * error_direction, type = "response")
emm_complex_df <- as.data.frame(emm_complex)  %>% 
  mutate(abs_dist = paste0(abs_dist, "°"))
contrast(emm_complex, by = "abs_dist", adjust = "holm")
contrast(emm_complex, by = "speed", adjust = "holm")
contrast(emm_complex, by = "error_direction", adjust = "holm")
contrast(emm_complex, method = "pairwise", adjust = "holm")

p_complex = ggplot(emm_complex_df, aes(x = abs_dist, y = response, color = error_direction, group =  error_direction)) +
  facet_wrap(~ speed, ncol = 2) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude",
    y = "Estimated Marginal Mean of Absolute Error",
    color = "Error Direction"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)+
  theme(strip.background = element_blank())

ggsave(p_complex, filename = "figures/emm_complex.png", width = 12, height = 6)

#-------------------------------------------------------------------------------
# abs_dist x speed x direction x error_direction

emm_complex2 <- emmeans(m_theoretical, ~  abs_dist * speed * direction * error_direction, type = "response")
emm_complex2_df <- as.data.frame(emm_complex2) %>%
  mutate(dir_errdir = paste0(direction, " - ", error_direction)) %>%
  mutate(dir_errdir = recode(dir_errdir, 
                           "in - over" = "Inwards - Undershoot",
                           "in - under" = "Inwards - Overshoot",
                           "out - over" = "Outwards - Overshoot",
                           "out - under" = "Outwards - Undershoot"),
         speed = paste0(speed, "°/s"),
         abs_dist = paste0(abs_dist, "°")) 
p_complex = ggplot(emm_complex2_df, aes(x = dir_errdir, y = response, color = speed, group =  speed)) +
  facet_wrap(~ abs_dist, ncol = 1, labeller = label_value) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude of Rotation",
    y = "Estimated Marginal Mean of Absolute Error",
    color = "Speed of Rotation"
  ) + 
  theme_pubr(base_size = 12, border = FALSE) +
  theme(strip.background = element_blank())

ggsave(p_complex, filename = "figures/emm_complex.png", width = 12, height = 6)


#-------------------------------------------------------------------------------
# overshoot model
dat_long = dat_long %>%
  mutate(b_error_direction = ifelse(error_direction == "over", 1, 0))

overshoot_model_formula <- b_error_direction ~ 
  act_pass +
  direction +
  abs_dist + 
  speed + 
  time +
  abs_error +
  abs_dist*direction*act_pass + 
  act_pass*speed*abs_dist*direction +
  (1 | ID)

m_overshoot = glmer(overshoot_model_formula,
                      family = binomial(link = "logit"),
                      data = dat_long,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

r.squaredGLMM(m_overshoot)
pred_prob = as.vector(predict(m_overshoot, type = "response"))
preds = rbinom(length(pred_prob), 1, as.vector(pred_prob))
confusion = table(Predicted = preds, Actual = dat_long$b_error_direction)
precision = confusion['1','1'] / (confusion['1','1'] + confusion['1','0'])
recall = confusion['1','1'] / (confusion['0','1'] + confusion['1','1'])
f1 = 2 * (precision * recall) / (precision + recall)

car::Anova(m_overshoot, type = "III")

fixed_overshoot_formula <- update(overshoot_model_formula, . ~ . - (1 | ID))
m_fixed_overshoot = glm(fixed_overshoot_formula,
                      family = binomial(link = "logit"),
                      data = dat_long)

anova(m_overshoot, m_fixed_overshoot)

#save model
saveRDS(m_overshoot, "m_overshoot.rds")

#-------------------------------------------------------------------------------
# ANOVA table 
pseudo_r2 = effect_contributions_hierarchical(m_overshoot, dat_long)

anova_os = as.data.frame(car::Anova(m_overshoot, type = "III"))
anova_os$Effect <- rename_effects(rownames(anova_os), var_labels)
rownames(anova_os) <- NULL
anova_os <- anova_os %>%
  select(Effect, everything()) %>%
  mutate(pseudo_R2 = c(0, pseudo_r2$Pseudo_R2_Contribution))


anova_os <- anova_os %>%
  select(Effect, Chisq, Df, `Pr(>Chisq)`, pseudo_R2) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

ft <- flextable(anova_os) %>%
  set_header_labels(
    Effect = "Effect",
    Chisq = "Khi²",
    Df = "df",
    `Pr(>Chisq)` = "p",
    pseudo_R2 = "Pseudo R²"
  ) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table X. Wald Khi² Tests (Type III ANOVA) for Fixed Effects of the theoretical model", style = "heading 2") %>%
  body_add_flextable(ft) %>%
  body_add_par("Note. ??² = Wald chi-square test statistic. p < .05 is considered statistically significant.
               To fit the model we used the BOBYQA optimiser using the glmer function in the lme4 package.", style = "Normal")

print(doc, target = "figures/anova_table2.docx")

emmeans_overshoot <- emmeans(m_overshoot, ~  abs_dist * direction, type = "response")
emmeans_overshoot_df1 <- as.data.frame(emmeans_overshoot) %>%
  mutate(abs_dist = paste0(abs_dist, "°")) %>% # format absolute distance for better readability
  mutate(direction = recode(direction, 
                            "in" = "120°", 
                            "out" = "30°")) # format direction for better readability

p_dist_dir2 = ggplot(emmeans_overshoot_df1, aes(x = abs_dist, y = prob, color = direction, group = direction)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Amplitude of Rotation",
    y = "Estimated Marginal Mean of P(observed > target)",
    color = "Starting Position"
  ) + 
  theme_pubr(base_size = 12, border = FALSE)

emmeans_overshoot = emmeans(m_overshoot, ~  speed * direction, type = "response")
emmeans_overshoot_df2 = as.data.frame(emmeans_overshoot) %>%
  mutate(speed = paste0(speed, "°/s")) %>% # format speed for better readability
  mutate(direction = recode(direction, 
                           "in" = "120°", 
                           "out" = "30°")) # format direction for better readability

p_speed_dir2 = ggplot(emmeans_overshoot_df2, aes(x = speed, y = prob, color = direction, group = direction)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, 
                size = 1, position = position_dodge(0.05)) +
  labs(
    x = "Speed",
    y = "",
    color = "Starting Position"
  ) + 
  theme_pubr(base_size = 12, border = FALSE) +
  theme(legend.position = "none")

# patch plot for main effects
emm_overshoot = p_dist_dir2 + p_speed_dir2
ggsave(emm_overshoot, filename = "figures/emm_overshoot.png", width = 12, height = 4)


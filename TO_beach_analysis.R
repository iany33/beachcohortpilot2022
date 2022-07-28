
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analysis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  lubridate,     # working with dates
  forcats,       # factors
  lme4,          # mixed-effects regression
  GLMMadaptive,  # mixed-effects regression with adaptive Gaussian quadrature
  ggeffects,     #  marginal effects/estimated marginal means for regression models
  DHARMa,        # diagnostics for mixed effects models
)









# Example logistic mixed-effects model code

model <- glmer(agi ~ contact_level + income + gender + age1 + (1 | houseID) + (1 | day) , data = data,
     family = binomial)
summary(model)

# Compare 1 vs. 2 random effects vs. none)
m1 <- glmer(agi ~ contact_level + income + gender + age1 + (1 | houseID), data = data,
               family = binomial)
m0 <- glm(agi ~ contact_level + income + gender + age1, data = data,
            family = binomial)
anova(model, m1, m0)


# Example diagnostics using DHARMa

diagnostics <- simulateResiduals(model, plot = T)
testDispersion(diagnostics)
diagnostics <- recalculateResiduals(model, group = data$houseID)
testDispersion(diagnostics)


# alternative code with adaptive quadrature

model <- mixed_model(fixed = agi ~ contact_level + income + gender + age1, 
                     random =  ~1 | houseID/day, data = data, family = binomial)
summary(model)



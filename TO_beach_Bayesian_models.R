

pacman::p_load(
  rio,          # File import
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  brms,          # Bayesian multi-level regression modelling
  bayesplot,     # Check brms diagnostics
  loo,           # Model comparisons with loo and waic criteria
  DHARMa        # diagnostics - can be used with brms = https://frodriguezsanchez.net/post/using-dharma-to-check-bayesian-models-fitted-with-brms/
)

# Test data
data <- import("G:/My Drive/Research/Projects-TMU/Beach water illness/Toronto pilot study/Analysis/data-test.xlsx")
data <- data |> mutate(age1 = as.factor(age1))


# Prior simulation - test

pacman::p_load(Rlab)

fakedata <- data.frame(agi = rbern(2000, 0.05),
  gender = sample(1:2, size=2000, replace=TRUE),
  age = sample(1:5, size=2000, replace=TRUE),
  contact_level = sample(1:3, size=2000, replace=TRUE))

fakedata <- fakedata |> 
  mutate(age = as.factor(age)) |> 
  mutate(contact_level = as.factor(contact_level))

m1 <- brm(agi ~ 0 + Intercept + contact_level + gender + age,
          family = bernoulli, data = fakedata, 
          prior = c(prior(normal(0, 1), class = "b"), 
                    prior(normal(0.6, 0.3), class = "b", coef = "contact_level2"),
                    prior(normal(0.2, 0.4), class = "b", coef = "contact_level3")), 
          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11)

plot(m1)
mcmc_plot(m1)
pp_check(m1, ndraws=50)
pp_check(m1, type = "stat", stat = "mean")


# Prior predictive simulation checking

get_prior(agi ~ 0 + Intercept + contact_level + gender + age1 + (1 | houseID),
          family = bernoulli, data = data)

m1 <- brm(agi ~ 0 + Intercept + contact_level + gender + age1 + (1 | houseID),
          family = bernoulli, data = data, 
          prior = c(prior(normal(0, 1), class = "b"), 
                    prior(normal(0.8, 0.4), class = "b", coef = "contact_levelPrimary"),
                    prior(normal(0.2, 0.4), class = "b", coef = "contact_levelSecondary")), 
          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11, sample_prior = "only")

m2 <- brm(agi ~ 0 + Intercept + contact_level + gender + age1 + (1 | houseID),
          family = bernoulli, data = data, 
          prior = c(prior(normal(0, 0.5), class = "b"), 
                    prior(normal(0.8, 0.4), class = "b", coef = "contact_levelPrimary"),
                    prior(normal(0.2, 0.4), class = "b", coef = "contact_levelSecondary")), 
          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11, sample_prior = "only")

m3 <- brm(agi ~ 0 + Intercept + contact_level + gender + age1 + (1 | houseID),
          family = bernoulli, data = data, 
          prior = c(prior(normal(0, 2), class = "b"), 
                    prior(normal(0.8, 0.4), class = "b", coef = "contact_levelPrimary"),
                    prior(normal(0.2, 0.4), class = "b", coef = "contact_levelSecondary")), 
          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11, sample_prior = "only")

plot(m1)
pp_check(m1, type = "stat", stat = "mean", prefix = "ppd")
pp_check(m1, ndraws=50, prefix = "ppd")

plot(m2)
pp_check(m2, type = "stat", stat = "mean", prefix = "ppd")
pp_check(m2, ndraws=50, prefix = "ppd")

plot(m3)
pp_check(m3, type = "stat", stat = "mean", prefix = "ppd")
pp_check(m3, ndraws=50, prefix = "ppd")


# Informative priors seem reasonable, other Betas normal(0,2) and (0,0.5) too wide or tight.
# Instead set other Betas at normal(0,1) which is weakly informative but within plausible range


# Run models with selected priors

prior <- c(set_prior("normal(0, 1)", class = "b"),
           set_prior("normal(0.8, 0.4)", class = "b", coef = "contact_levelPrimary"),
           set_prior("normal(0.2, 0.4)", class = "b", coef = "contact_levelSecondary"))

model1 <- brm(agi ~ 0 + Intercept + contact_level + gender + age1 + (1 | houseID),
              family = bernoulli, data = data, prior = prior,
              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11)

summary(model1, waic=TRUE)

# Diagnostics

plot(model1)
mcmc_plot(model1)
pp_check(model1)
pp_check(model1, type = "loo_pit")
conditional_effects(model1)

# Compare with model with another random/varying effect for recruitment day

model2 <- update(model1, formula. = ~ . - (1 | houseID) + (1 | houseID/date))

summary(model2, waic=TRUE)
loo(model1, model2)



# Choose one of two models above to build as framework model with additional confounders. 
# income; education; location of residence (e.g. Ontario vs. other?); ethnicity. 
# Possible confounders: chronic conditions; sand contact; consumed food; contact with algae, swam in past 14 days.

# Build separate AGI models with E. coli levels as predictor and other environmental variables (48-hr rainfall, air temp? wave height? streamflow? UV index?)

# Build models for other health outcomes (ear, eye, respiratory, and skin infections).











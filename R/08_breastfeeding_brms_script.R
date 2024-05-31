
### import data and libraries ----

library(dplyr); library(tidyr)
library(ggplot2)

# for macs: 
# remotes::install_github("paul-buerkner/brms")
library(brms)

load("outputs/prepped_data.RData")

### additional data conditioning

data_final <- data_final |> mutate(dep_cat2 = factor(dep_cat2, ordered = TRUE)) |> drop_na(vitD_lab)

pp <- filter(data_final, studypop ==  "postpartum")

### run models ----

fit_bfeed_a <- brm(formula = bf(dep_cat2 | weights(sample_wts) ~ log(vitD_lab):breastfeed + diet_pc1 + diet_pc2) +
                      lf(disc ~ 0 + breastfeed, cmc = FALSE), 
                    family = cumulative("logit"), 
                    warmup = 1000, iter = 2000, chains = 4,
                    data = pp)


fit_bfeed_b <- brm(formula = bf(dep_cat2 | weights(sample_wts) ~ log(vitD_lab):breastfeed + diet_pc1 + diet_pc2) +
                      lf(disc ~ 0 + breastfeed, cmc = FALSE), 
                    family = cumulative("probit"), 
                    warmup = 1000, iter = 2000, chains = 4,
                    data = pp) 

fit_bfeed_a <- add_criterion(fit_bfeed_a, "waic")
fit_bfeed_a <- add_criterion(fit_bfeed_a, "loo")
fit_bfeed_b <- add_criterion(fit_bfeed_b, "waic")
fit_bfeed_b <- add_criterion(fit_bfeed_b, "loo")

### save results ----

save(pp, fit_bfeed_a, fit_bfeed_b, file = "outputs/bayesian_objects_breastfeed.RData")

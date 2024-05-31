
### import data and libraries ----

library(dplyr); library(tidyr)
library(ggplot2)

# for macs: 
# remotes::install_github("paul-buerkner/brms")
library(brms)

load("outputs/prepped_data.RData")

### additional data conditioning

data_final <- data_final |> mutate(dep_cat2 = factor(dep_cat2, ordered = TRUE)) |> drop_na(vitD_lab)
men <- filter(data_final, studypop == "men") |> drop_na(dep_cat2)
preg_pp <- filter(data_final, studypop %in%  c("pregnant", "postpartum"))
women <- filter(data_final, studypop == "non-PP")
preg <- filter(data_final, studypop ==  "pregnant")
pp <- filter(data_final, studypop ==  "postpartum")

### run models ----

fit_men_a <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
             family = cumulative("logit"), 
             warmup = 1000, iter = 2000, chains = 1,
             save_pars = save_pars(all = TRUE),
             data = men)

fit_men_b <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                 family = cumulative("probit"), 
                 warmup = 1000, iter = 2000, chains = 1,
                 save_pars = save_pars(all = TRUE),
                 data = men)

fit_men_a <- add_criterion(fit_men_a, "waic")
fit_men_a <- add_criterion(fit_men_a, "loo")
fit_men_b <- add_criterion(fit_men_b, "waic")
fit_men_b <- add_criterion(fit_men_b, "loo")

fit_wom_a <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                 family = cumulative("logit"), 
                 warmup = 1000, iter = 2000, chains = 4,
                 data = women)

fit_wom_b <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                 family = cumulative("probit"), 
                 warmup = 1000, iter = 2000, chains = 4,
                 data = women)

fit_wom_a <- add_criterion(fit_wom_a, "waic")
fit_wom_a <- add_criterion(fit_wom_a, "loo")
fit_wom_b <- add_criterion(fit_wom_b, "waic")
fit_wom_b <- add_criterion(fit_wom_b, "loo")

fit_preg_a <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                 family = cumulative("logit"), 
                 warmup = 1000, iter = 2000, chains = 4,
                 data = preg)

fit_preg_b <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                 family = cumulative("probit"), 
                 warmup = 1000, iter = 2000, chains = 4,
                 data = preg)

fit_preg_a <- add_criterion(fit_preg_a, "waic")
fit_preg_a <- add_criterion(fit_preg_a, "loo")
fit_preg_b <- add_criterion(fit_preg_b, "waic")
fit_preg_b <- add_criterion(fit_preg_b, "loo")

fit_pp_a <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                  family = cumulative("logit"), 
                  warmup = 1000, iter = 2000, chains = 4,
                  data = pp)

fit_pp_b <- brm(dep_cat2 | weights(sample_wts) ~ log(vitD_lab) + diet_pc1 + diet_pc2, 
                  family = cumulative("probit"), 
                  warmup = 1000, iter = 2000, chains = 4,
                  data = pp)

fit_pp_a <- add_criterion(fit_pp_a, "waic")
fit_pp_a <- add_criterion(fit_pp_a, "loo")
fit_pp_b <- add_criterion(fit_pp_b, "waic")
fit_pp_b <- add_criterion(fit_pp_b, "loo")

fit_preg_p_a <- brm(formula = bf(dep_cat2 | weights(sample_wts) ~ log(vitD_lab):studypop + diet_pc1 + diet_pc2) +
                    lf(disc ~ 0 + studypop, cmc = FALSE), 
                family = cumulative("logit"), 
                warmup = 1000, iter = 2000, chains = 4,
                data = preg_pp)


fit_preg_p_b <- brm(formula = bf(dep_cat2 | weights(sample_wts) ~ log(vitD_lab):studypop + diet_pc1 + diet_pc2) +
                      lf(disc ~ 0 + studypop, cmc = FALSE), 
                    family = cumulative("probit"), 
                    warmup = 1000, iter = 2000, chains = 4,
                    data = preg_pp) 

fit_preg_p_a <- add_criterion(fit_preg_p_a, "waic")
fit_preg_p_a <- add_criterion(fit_preg_p_a, "loo")
fit_preg_p_b <- add_criterion(fit_preg_p_b, "waic")
fit_preg_p_b <- add_criterion(fit_preg_p_b, "loo")

# save to file!----

all_ <- c(ls(pattern = "fit"), "men", "pp", "preg", "preg_pp", "women")

save(list = all_, file = "outputs/bayesian_objects.RData")



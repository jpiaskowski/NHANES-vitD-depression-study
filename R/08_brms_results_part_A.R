
### main analysis ----


library(brms)
library(dplyr); library(tidyr)
library(ggplot2); library(tidybayes)


load(file = "outputs/bayesian_objects.RData")

### model fit check ----


waic(fit_men_a, fit_men_b) 
loo(fit_men_a, fit_men_b)  


waic(fit_wom_a, fit_wom_b) 
loo(fit_wom_a, fit_wom_b) 


waic(fit_preg_a, fit_preg_b) 
loo(fit_preg_a, fit_preg_b) 


waic(fit_pp_a, fit_pp_b) 
loo(fit_pp_a, fit_pp_b) 


waic(fit_preg_p_a, fit_preg_p_b) 
loo(fit_preg_p_a, fit_preg_p_b) 


# more diagnostics ----

# men
rhat(fit_men_b) 
neff_ratio(fit_men_b) 

plot(fit_men_b) # check convergence
# looks okay

# women
rhat(fit_wom_b) 
neff_ratio(fit_wom_b) 

plot(fit_wom_b) # check convergence
# look fine

# preg
rhat(fit_preg_b) 
neff_ratio(fit_preg_b) 

plot(fit_preg_b) # check convergence

# pp
rhat(fit_pp_b) 
neff_ratio(fit_preg_b) 
  
plot(fit_pp_b) # check convergence


# preg/pp
rhat(fit_preg_p_b) 
neff_ratio(fit_preg_p_b) 

plot(fit_preg_p_b) # check convergence

# fit b is universally better


### Extract results ----

coef_draws_long1 <- fit_men_b |> 
  gather_draws(`b_[a-z].*`, regex = TRUE) %>%
  mutate(cohort = "men") %>% 
  mutate(.variable = gsub("^b_", "", .variable)) %>%
  mutate(variable = factor(.variable,
                           levels = c("logvitD_lab", "diet_pc1", "diet_pc2"),
                           labels = c("log vitamin D", "diet pattern 1", "diet pattern 2")))

coef_draws_long2 <- fit_wom_b |> 
  gather_draws(`b_[a-z].*`, regex = TRUE) %>%
  mutate(cohort = "women") %>% 
  mutate(.variable = gsub("^b_", "", .variable)) %>%
  mutate(variable = factor(.variable,
                           levels = c("logvitD_lab", "diet_pc1", "diet_pc2"),
                           labels = c("log vitamin D", "diet pattern 1", "diet pattern 2")))

coef_draws_long3 <- fit_preg_b |> 
  gather_draws(`b_[a-z].*`, regex = TRUE) %>%
  mutate(cohort = "pregnant") %>% 
  mutate(.variable = gsub("^b_", "", .variable)) %>%
  mutate(variable = factor(.variable,
                           levels = c("logvitD_lab", "diet_pc1", "diet_pc2"),
                           labels = c("log vitamin D", "diet pattern 1", "diet pattern 2")))

coef_draws_long4 <- fit_pp_b |> 
  gather_draws(`b_[a-z].*`, regex = TRUE) %>%
  mutate(cohort = "postpartum") %>% 
  mutate(.variable = gsub("^b_", "", .variable)) %>%
  mutate(variable = factor(.variable,
                           levels = c("logvitD_lab", "diet_pc1", "diet_pc2"),
                           labels = c("log vitamin D", "diet pattern 1", "diet pattern 2")))



coef_draws_long5 <- fit_preg_p_b |> 
  gather_draws(`b_[a-z].*`, regex = TRUE) %>%
  mutate(cohort = "pregnant/postpartum") %>% 
  mutate(.variable = gsub("^b_", "", .variable)) %>%
  mutate(variable = factor(.variable,
                           levels = c("logvitD_lab", "diet_pc1", "diet_pc2"),
                           labels = c("log vitamin D", "diet pattern 1", "diet pattern 2")))

# repeat for all cohorts, then combine into one file

coef_draws_sum1 <- coef_draws_long1 |> 
  median_hdi(.value, .width = c(0.68, 0.95)) 

coef_draws_sum2 <- coef_draws_long2 |> 
  median_hdi(.value, .width = c(0.68, 0.95)) 

coef_draws_sum3 <- coef_draws_long3 |> 
  median_hdi(.value, .width = c(0.68, 0.95)) 

coef_draws_sum4 <- coef_draws_long4 |> 
  median_hdi(.value, .width = c(0.68, 0.95)) 

coef_draws_sum5 <- coef_draws_long5 |> 
  median_hdi(.value, .width = c(0.68, 0.95)) 

all_sum <- bind_rows(coef_draws_long1, coef_draws_long2, coef_draws_long3, coef_draws_long4) %>% 
  mutate(cohort = factor(cohort, levels = c("pregnant", "postpartum", "women", "men"))) %>% 
  group_by(cohort, variable) %>% 
  median_hdi(.value, .width = c(0.68, 0.95)) %>% ungroup


##### Plotting ----

ggplot(all_sum, aes(x = .value, y = reorder(variable, desc(variable)), color = cohort, fill = cohort)) +
  geom_pointinterval(aes(x = .value, xmin = .lower, xmax = .upper), shape = 18, size = 20, alpha = 0.7) +
 # facet_wrap(vars(variable), ncol = 1, nrow = 3, scales = "free") + 
  xlab("slope") + 
  geom_vline(xintercept = 0, color = "gray30") + 
  scale_color_brewer(palette="Dark2") + 
  scale_fill_brewer(palette="Dark2") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        #axis.text.y = element_blank(), 
        legend.title = element_blank())

# not run
# ggsave("outputs/all_estimates.png", width = 7.7, height = 4.2)


(hyp1 <- hypothesis(fit_men_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 < 0"), alpha = 0.001))
(hyp2 <- hypothesis(fit_wom_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 < 0"), alpha = 0.001))
(hyp3 <- hypothesis(fit_preg_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 > 0"), alpha = 0.001))
(hyp4 <- hypothesis(fit_pp_b, c("logvitD_lab < 0", "diet_pc1 > 0", "diet_pc2 < 0"), alpha = 0.001))

CE_preg <- conditional_effects(fit_preg_b, "vitD_lab", categorical = TRUE, 
                              prob = 0.99, method = "posterior_epred")

# plot prediction curves

est_preg <- conditional_effects(fit_preg_b, "vitD_lab", categorical = TRUE, 
                                prob = 0.99, method = "posterior_epred")[[1]] |> mutate(cohort = "pregnant")

est_pp <- conditional_effects(fit_pp_b, "vitD_lab", categorical = TRUE, 
                                prob = 0.99, method = "posterior_epred")[[1]] |> mutate(cohort = "postpartum")

est_women <- conditional_effects(fit_wom_b, "vitD_lab", categorical = TRUE, 
                                prob = 0.99, method = "posterior_epred")[[1]] |> mutate(cohort = "women")

est_men <- conditional_effects(fit_men_b, "vitD_lab", categorical = TRUE, 
                                 prob = 0.99, method = "posterior_epred")[[1]] |> mutate(cohort = "men")


bind_rows(est_preg, est_pp, est_women, est_men) |> 
  mutate(cohort = factor(cohort, levels = c("pregnant", "postpartum", "women", "men"))) |> 
  ggplot(aes(x = effect1__, y = estimate__, color = effect2__, fill = effect2__, ymin = lower__, ymax = upper__)) +
  geom_smooth(se = TRUE, linewidth = 1) +
  xlab("Vitamin D") + ylab("Probability") + 
  facet_wrap(vars(cohort), nrow = 2, ncol = 2) +  
  theme_bw(base_size = 14) + 
  scale_color_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# not run
# ggsave("outputs/prediction_curves.png", width = 5, height = 5.5)


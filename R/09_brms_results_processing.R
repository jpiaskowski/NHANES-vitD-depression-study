
### main analysis ----


library(brms)
library(dplyr); library(tidyr)
library(ggplot2); library(tidybayes)


load(file = "outputs/bayesian_objects.RData")


# summary(fit_men_b)
# summary(fit_wom_b)
# summary(fit_preg_b)
# summary(fit_pp_b)
# summary(fit_preg_p_b)

### model fit check ----


waic(fit_men_a, fit_men_b) # lower is better
loo(fit_men_a, fit_men_b) # lower is better # 
# b is better

waic(fit_wom_a, fit_wom_b) # lower is better
loo(fit_wom_a, fit_wom_b) # lower is better
# b is better

waic(fit_preg_a, fit_preg_b) # lower is better
loo(fit_preg_a, fit_preg_b) # lower is better
# b is better

waic(fit_pp_a, fit_pp_b) # lower is better
loo(fit_pp_a, fit_pp_b) # lower is better
# b is better

waic(fit_preg_p_a, fit_preg_p_b) # lower is better
loo(fit_preg_p_a, fit_preg_p_b) # lower is better
# b is better!

# more diagnostics ----

# men
rhat(fit_men_b) # should be close to zero
neff_ratio(fit_men_b) #https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html
# okay if between 0.5 and 1, best is 0.1 to 0.5
plot(fit_men_b) # check convergence
# looks okay

# women
rhat(fit_wom_b) # should be close to zero
neff_ratio(fit_wom_b) #https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html
# okay if between 0.5 and 1, best is 0.1 to 0.5
plot(fit_wom_b) # check convergence
# look fine

# preg
rhat(fit_preg_b) # should be close to zero
neff_ratio(fit_preg_b) #https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html
# okay if between 0.5 and 1, best is 0.1 to 0.5
plot(fit_preg_b) # check convergence

# pp
rhat(fit_pp_b) # should be close to zero
neff_ratio(fit_preg_b) #https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html
  # okay if between 0.5 and 1, best is 0.1 to 0.5
plot(fit_pp_b) # check convergence
#posterior_summary(fit_preg_b) # table of results

# preg/pp
rhat(fit_preg_p_b) # should be close to zero
neff_ratio(fit_preg_p_b) #https://mc-stan.org/bayesplot/reference/MCMC-diagnostics.html
# okay if between 0.5 and 1, best is 0.1 to 0.5
plot(fit_preg_p_b) # check convergence


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

# repeat for all cohorts, then combine into one file#

#all_coef_draws <-  bind_rows(coef_draws_long1, coef_draws_long2, coef_draws_long3, coef_draws_long4) 
 

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

dep_cols <- c("#4B2991", "#952EA0","#D44292")
cohort_cols <- c("#A6CEE3", "#1F78B4", "#D53E4F", "#FB9A99")
bfeed_cols <- c("#ABDDA4", "#66C2A5")



half_eye_plot <- function(df) {
  ggplot(df, aes(x = .value , color = variable, fill = variable)) +
    stat_halfeye(n = 1000, slab_alpha = 0.5, interval_alpha = 0.8) +
    facet_wrap(vars(variable), scales = "free", nrow = 3, ncol = 1) + 
    scale_color_brewer(palette="Set1") + 
    scale_fill_brewer(palette="Set1") + 
    theme_minimal(base_size = 15) +
    theme(axis.title.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_blank())
}

half_eye_plot(coef_draws_long1); ggsave("outputs/men_halfeye.png", width = 4, height = 6.5)
half_eye_plot(coef_draws_long2); ggsave("outputs/women_halfeye.png", width = 4, height = 6.5)
half_eye_plot(coef_draws_long3); ggsave("outputs/pregnant_halfeye.png", width = 4, height = 6.5)
half_eye_plot(coef_draws_long4); ggsave("outputs/postpartum_halfeye.png", width = 4, height = 6.5)





ggplot(all_sum, aes(x = .value, y = reorder(variable, desc(variable)), color = cohort, fill = cohort)) +
  geom_pointinterval(aes(x = .value, xmin = .lower, xmax = .upper), shape = 18, size = 20, alpha = 0.7) +
 # facet_wrap(vars(variable), ncol = 1, nrow = 3, scales = "free") + 
  xlab("slope") + 
  geom_vline(xintercept = 0, color = "gray30") + 
  #scale_color_manual(values = cohort_cols) + 
  #scale_fill_manual(values = cohort_cols) +
  scale_color_brewer(palette="Dark2") + 
  scale_fill_brewer(palette="Dark2") + 
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        #axis.text.y = element_blank(), 
        legend.title = element_blank())

ggsave("outputs/all_estimates.png", width = 7.7, height = 4.2)


# ggplot(coef_draws_long1, aes(x = .value , color = variable, fill = variable)) +
#     stat_halfeye(n = 1000, slab_alpha = 0.5, interval_alpha = 0.7) +
#     facet_wrap(vars(variable), scales = "free", nrow = 3, ncol = 1) + 
#     #xlab("coefficient") + 
#     scale_color_brewer(palette="Set1") + 
#     scale_fill_brewer(palette="Set1") + 
#     theme_minimal(base_size = 15) +
#     theme(axis.title.y = element_blank(),
#           legend.position = "none",
#           axis.title.x = element_blank())

(hyp1 <- hypothesis(fit_men_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 < 0"), alpha = 0.001))
(hyp2 <- hypothesis(fit_wom_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 < 0"), alpha = 0.001))
(hyp3 <- hypothesis(fit_preg_b, c("logvitD_lab < 0", "diet_pc1 < 0", "diet_pc2 > 0"), alpha = 0.001))
(hyp4 <- hypothesis(fit_pp_b, c("logvitD_lab < 0", "diet_pc1 > 0", "diet_pc2 < 0"), alpha = 0.001))

CE_preg <- conditional_effects(fit_preg_b, "vitD_lab", categorical = TRUE, 
                              prob = 0.99, method = "posterior_epred")

names(CE_preg) # for investigation

# some testing
CE_preg_plot <- plot(CE_preg)[[1]] 

# this is plotting vitamin D versus the prediction for each diet PC and applying a smoothing function
CE_preg_plot +
  xlab("Vitamin D") + 
  ggtitle("pregnant women") + 
  theme_bw(base_size = 14) + 
  scale_color_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha = 0.5) +
  #scale_color_manual(values = dep_cols) + 
  #scale_fill_manual(values = dep_cols) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


# this replicates the plot above

ggplot(CE_preg[[1]], aes(x = effect1__, y = estimate__, color = effect2__, fill = effect2__, ymin = lower__, ymax = upper__)) +
  geom_smooth(se = TRUE, linewidth = 0.75) +
  xlab("Vitamin D") + 
  ggtitle("pregnant women") + 
  theme_bw(base_size = 14) + 
  scale_color_manual(values = c("red2", "dodgerblue2", "gold2")) + 
  scale_fill_manual(values = c("red2", "dodgerblue2", "gold2")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# now, some actual plotting

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

ggsave("outputs/prediction_curves.png", width = 5, height = 5.5)

bind_rows(est_preg, est_pp, est_women, est_men) |> 
  mutate(cohort = factor(cohort, levels = c("pregnant", "postpartum", "women", "men"))) |> 
  ggplot(aes(x = effect1__, y = estimate__, color = effect2__, fill = effect2__, ymin = lower__, ymax = upper__)) +
  geom_smooth(se = TRUE, linewidth = 0.75) +
  xlab("Vitamin D") + ylab("Probability") + 
  facet_wrap(vars(cohort), nrow = 2, ncol = 2, scales = "free_x") +  
  theme_bw(base_size = 14) + 
  scale_color_manual(values = c("red2", "dodgerblue2", "gold2")) + 
  scale_fill_manual(values = c("red2", "dodgerblue2", "gold2")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave("outputs/prediction_curves_freescales.png", width = 5, height = 5.5)

## need posterior estimates
# I don't like any of this (below)

# create prediction grid over Vit D, setting latent variables at zero

vit_d_range =  range(data_final$vitD_lab, na.rm = TRUE)

pred_grid <- data.frame(vitD_lab = seq(vit_d_range[1], vit_d_range[2], by = 0.2), 
                        diet_pc1 = 0,
                        diet_pc2 = 0) |> mutate(ID = 1:n())

  
preg_new = posterior_epred(fit_preg_b, newdata = pred_grid)  ## this is it!!

preg_new_test <- rbind(as.data.frame(t(preg_new[, , 1])), as.data.frame(t(preg_new[, , 2])), as.data.frame(t(preg_new[, , 3]))) |> 
  mutate(ID = rep(1:nrow(pred_grid), 3), depression = rep(c("no depression", "mild", "moderate to severe"), each = nrow(pred_grid))) |> relocate(ID, depression) |> 
  pivot_longer(col = 3:4001,  names_to = "draw") |> left_join(pred_grid, by = "ID") |> select(-diet_pc1, -diet_pc2) |> 
  group_by(vitD_lab, depression) |> median_hdi(value, .width = c(0.68, 0.95)) |> ungroup() 

table(preg_new_test$depression)
apply(preg_new_test, 2, function(x) sum(is.na(x)))
sum(is.na(preg_new_test$vitD_lab))

ggplot(preg_new_test, aes(x = log(vitD_lab), y = value, color = depression)) +
  geom_line()

#### breastfeeding subgroup analysis ----

library(brms)
library(dplyr); library(tidyr)
library(ggplot2); library(tidybayes)
library(ggplot2); library(viridis)

load(file = "outputs/bayesian_objects_breastfeed.RData")


waic(fit_bfeed_a, fit_bfeed_b)
loo(fit_bfeed_a, fit_bfeed_b)

# fit b is better

rhat(fit_bfeed_b)
neff_ratio(fit_bfeed_b)

# https://discourse.mc-stan.org/t/plot-interactions-for-family-categorical/13095

conditions <- data.frame(breastfeed = c("yes", "no"))

bfeed_CE <- conditional_effects(
  fit_bfeed_a, 
  effects  = "vitD_lab",
  conditions = conditions,
  categorical = TRUE, prob = 0.99)[[1]] |> 
  mutate(breastfeed = factor(breastfeed, levels = c("yes", "no"), 
                             labels = c("breastfeeding", "non-breastfeeding")))

fixef(fit_bfeed_b)

(hyp5 <- hypothesis(fit_bfeed_b, c("logvitD_lab:breastfeedno < 0", "logvitD_lab:breastfeedyes < 0",
                                   "logvitD_lab:breastfeedno = logvitD_lab:breastfeedyes", 
                                   "logvitD_lab:breastfeedno - logvitD_lab:breastfeedyes < 0", 
                                   "diet_pc1 = 0", "diet_pc2 < 0"), alpha = 0.001))

ggplot(bfeed_CE, aes(x = effect1__, y = estimate__, color = effect2__, fill = effect2__, ymin = lower__, ymax = upper__)) +
  geom_smooth(se = TRUE, linewidth = 1) +
  xlab("Vitamin D") + ylab("Probability") + 
  facet_wrap(vars(breastfeed), nrow = 2, ncol = 2) +  
  theme_bw(base_size = 14) + 
  scale_color_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave("outputs/breastfeed_prediction_curves.png", width = 5, height = 3.5)





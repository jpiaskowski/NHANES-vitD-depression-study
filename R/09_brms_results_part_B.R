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
                                   "logvitD_lab:breastfeedno - logvitD_lab:breastfeedyes > 0", 
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

# not run
# ggsave("outputs/breastfeed_prediction_curves.png", width = 5, height = 3.5)



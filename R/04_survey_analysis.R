
#### read in data and prepare for analysis ----

library(dplyr); library(tidyr)
library(survey)
library(ggplot2); library(viridis)

mydata_full <- read.csv("data/NHANES_2007-2018_recoded.csv") %>% 
  rename(strata_var = "strata") %>%  # rename variable to unique name so it doesn't trip us later
  filter(!(is.na(sample_wt) | is.na(strata_var) | is.na(var_unit))) %>%  # can't build survey object if any of these are missing
  mutate(depression_cat = factor(depression_cat, 
                                 levels = c("no depression", "mild",
                                            "moderate", "moderately severe to severe"))) %>% 
  mutate(dep_cat2 = forcats::fct_collapse(depression_cat, 
                                          `moderate to severe` = c("moderate", "moderately severe to severe"))) |> 
  filter(!is.na(depression_cat), age_screening > 19, age_screening < 45) # indicator variable if depression category is missing



# create survey object (this will be used for all downstream analyses)

nhanes_svy <- svydesign(data = mydata_full,
                     ids = ~ var_unit, 
                     weights = ~ sample_wt,
                     strata = ~ strata_var, 
                     nest = TRUE)

## weighted cross tabulations & chi-square tests 

# chi-square test
svychisq(~ depression_cat + studypop, nhanes_svy)
svychisq(~ depression_cat + breastfeed, nhanes_svy)


xtab_cohort <- prop.table(svytable(~ dep_cat2 + studypop, nhanes_svy), margin = 2) |> 
  as.data.frame() |> 
  mutate(percent = Freq*100,
         studypop = factor(studypop, levels = c("pregnant", "postpartum", "non-PP", "men"), 
                           labels = c("pregnant", "postpartum", "women", "men")))
  
###### Plot Data

ggplot(xtab_cohort, aes(x = studypop, y = percent, fill = dep_cat2)) +
  geom_col(width = 0.6) +
  #scale_fill_manual(values = dep_cols) + 
  scale_fill_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  theme_bw(base_size = 15) + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("outputs/depression_cohort_percent.png", width = 5, height = 3.75)


xtab_bfeed <- prop.table(svytable(~ dep_cat2 + breastfeed, nhanes_svy), margin = 2) |> 
  as.data.frame() |> 
  mutate(percent = Freq*100,
         breastfeed = factor(breastfeed, levels = c("yes", "no"), 
                           labels = c("breastfeeding", "non-breastfeeding")))

ggplot(xtab_bfeed, aes(x = breastfeed, y = percent, fill = dep_cat2)) +
  geom_col(width = 0.4) +
  #scale_fill_manual(values = dep_cols) + 
  scale_fill_viridis(option = "D", discrete = TRUE, alpha = 0.8) +
  theme_bw(base_size = 14) + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("outputs/depression_cohort_percent_bfeed.png", width = 4.6, height = 3.75)

write.csv(xtab_cohort, "outputs/cohort_percentages.csv", row.names = FALSE)
write.csv(xtab_bfeed, "outputs/breastfeed_percentages.csv", row.names = FALSE)

## Check cohort totals:

load("outputs/prepped_data.RData")

pop_counts <- data_final |> drop_na(vitD_lab) |> 
  count(studypop, dep_cat2) |> pivot_wider(names_from = studypop, values_from = n)

rowSums(pop_counts[,-1])
colSums(pop_counts[,-1])
sum(rowSums(pop_counts[,-1]))


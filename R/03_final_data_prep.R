
library(dplyr); library(tidyr)
library(forcats)
library(DataExplorer)

mydata <- read.csv("data/NHANES_2007-2018_cleaned.csv") 

codebook <- read.csv("data/codebook.csv") %>% select(variable, new_ID) %>% 
  filter(variable %in% colnames(mydata))

mydata_recode <- mydata %>% #select(-indfmpir) %>% 
  rename_at(vars(codebook$variable), ~ codebook$new_ID) %>% 
  mutate(depression_cat = cut(dep_score, breaks = c(0, 4, 9, 14, 28),
                              include.lowest = TRUE, ordered_result = TRUE, 
                              labels = c("no depression", "mild", "moderate", 
                                         "moderately severe to severe"))) %>%
  mutate(studypop = fct_recode(as.factor(studypop), pregnant = "1", postpartum = "2", 
                               `non-PP` = "3", men = "4"),
         study_year = fct_recode(as.factor(study_year), `2007/2008` = "5",`2009/2010` = "6",
                                 `2011/2012` = "7", `2013/2014` = "8", `2015/2016` = "9", 
                                 `2017/2018` = "10"),
         sex = fct_recode(as.factor(sex), female = "2", male = "1"),
         race_ethnic = fct_recode(as.factor(race_ethnic), Hispanic = "1",
                                  White = "3", Black = "4", `Multi-racial` = "5"),
         pregnant = fct_recode(as.factor(pregnant), pregnant = "1", 
                               `not pregnant` = "2", `unknown` = "3"),
         ed_level = fct_recode(as.factor(ed_level), `No High School Diploma` = "1",
                               `High School Diploma/GED` = "3", `Some College or AA` = "4",
                               `College Degree` = "5"),
         marital_status = fct_recode(as.factor(marital_status), `Married/partnered` = "1",
                                     `Widowed/divorced/separated` = "2", `Never married` = "5"),
         season_collect = fct_recode(as.factor(season_collect), winter = "1", 
                                     summer = "2"),
         food_secur = fct_recode(as.factor(food_secur), secure = "1", insecure = "2"),
         wic = fct_recode(as.factor(wic), yes = "1", no = "2"),
         breastfeed = fct_recode(as.factor(breastfeed), yes = "1", no = "2"),
         supplements = fct_recode(as.factor(supplements), yes = "1", no = "2")
         ) %>% 
  mutate(breastfeed_pp = case_when(
    studypop == "postpartum" &  breastfeed == "yes" ~ "yes",
    studypop == "postpartum" &  breastfeed == "no" ~ "no",
    TRUE ~ NA_character_)) %>% 
  relocate(studypop, .after = id) %>% 
  relocate(depression_cat, .after = dep_score) %>% 
  mutate(filter_var = (age_screening > 19 & age_screening < 45 &
                         kcal >= 500 & kcal <= 8000 &
                         protein >= 20 & protein <= 300 & 
                         carbs <= 800 &
                         fats >= 10 & fats <= 300 & 
                         bmi > 15))

mydata_recode_age <- mydata_recode %>% 
  filter(age_screening > 19) %>% # removes 25k obs
  filter(age_screening < 45) # removes 20k
  
missing_studypop <- filter(mydata_recode_age, is.na(studypop)) %>% 
  select(studypop, sex, pregnant, breastfeed, months_postp, breastfeed_pp)
  
# check on factor levels: 
mydata_factor <- select_if(mydata_recode, is.factor)
apply(mydata_factor, 2, table)

# check on indicator variable:
table(mydata_recode$filter_var)
table(mydata_recode$breastfeed_pp)

# write out file:

write.csv(mydata_recode, "data/NHANES_2007-2018_recoded.csv", row.names = FALSE)


# exploratory plots of data

# histograms
mydata_recode %>% select(-id, -sample_wt) %>% 
  DataExplorer::plot_histogram()


# filter based on nutritional principles

hist(mydata_recode$fats)
hist(mydata_recode$kcal)
hist(mydata_recode$protein)
hist(mydata_recode$carbs)

mydata_filter <- mydata_recode %>% 
  filter(age_screening > 19 & age_screening < 45) %>% #Only 20-44 age range (removed 45374)
  filter(kcal >= 500 & kcal <= 8000) %>%  # removed 1742 observation
  filter(protein >= 20 & protein <= 300) %>% # removed 204 observations
  filter(carbs <= 800) %>%  # removed 57 observations
  filter(fats >= 10 & fats <= 300) %>%  # removed 54 observations
  filter(bmi > 15) # removed 70 observations

#nrow(mydata_recode) - nrow(mydata_filter) - 45374 - 1742 - 204 - 57 - 54 - 70


write.csv(mydata_filter, "data/NHANES_2007-2018_filtered.csv", row.names = FALSE)


# filter based on Vitamin D 

# stem(mydata_recode$bmi)
# hist(mydata_recode$vitD_lab) # seems severe
# (150-58)/24
# stem(mydata_recode$extraD)
# stem(mydata_recode$vitD_interview)
# 
# mydata_vitD <- mydata_recode %>%  
#   filter(vitD_lab <= 150) %>%  # removed 1168 observations
#   filter(vitD3_lab <= 150) %>%  # no additional observations removed
#   filter(vitD2_lab <= 10) %>%  # removed 339 observations
#   #filter(! extraD >= 50) %>% # removed 10556 observations %>% 
#   filter(vitD_interview < 50) # no additional observations removed
# 
# 
# nrow(mydata_recode) - nrow(mydata_vitD) - 663 - 1168 - 339 - 10556
# 
# both_samples <- intersect(mydata_filter$id, mydata_vitD$id)
# 
# all <- mydata_recode %>% filter(id %in% both_samples)
# 
# table(all$studypop, all$depression_cat)
# 

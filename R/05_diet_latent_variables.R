
#### read in data and prepare for analysis ----

# load libraries
library(dplyr); library(tidyr)
library(ggplot2)


# read in data and do some basic processing

mydata_full <- read.csv("data/NHANES_2007-2018_recoded.csv") 

diet_vars <- c("kcal", "protein", "carbs", "sugars", "fats", "sat_fats", "monosat_fats")

mydata_diet <- mydata_full |> 
  filter(age_screening > 19 & age_screening < 45) |> 
  select(id, studypop, all_of(diet_vars))  |> 
  mutate(studypop = as.factor(studypop))

# filter out missing diet data
mydata_diet_final <- mydata_diet |> drop_na() # 12,839 data points

#### PCA ----

diet_pca <- prcomp(mydata_diet_final[,-(1:2)], center = TRUE, scale. = TRUE)

# plot PCA loadings

labs <- c("carbohydrates", "fats", "kcal", "monounsaturated",    
          "protein", "saturated fats", "sugars")

scores <- as.data.frame(diet_pca$rotation) |> select(PC1, PC2) |> 
  rename(`Principal Component 1 (74%)` = "PC1", `Principal component 2 (15%)` = "PC2") |> 
  tibble::rownames_to_column("diet_vars") |> 
  pivot_longer(cols = 2:3)

ggplot(scores, aes(y = diet_vars, x = value)) +
  geom_col() +
  facet_grid(. ~ name) +
  scale_y_discrete(labels = labs) + 
  xlab("") + ylab("") +
  theme_bw(base_size = 12) 

# pc1 ~ overall consumption
# pc2 ~ "paleo" (carbs/sugars vs protein/fats)

# not run
# ggsave("outputs/diet_pca_loadings.png", width = 1750, height = 1400, units = "px")

### extract PCA-derived latent variables and merge to original data set

rotations <- as.data.frame(diet_pca$x[,1:2])

mydata_diet_pc <- cbind(mydata_diet_final, rotations) |> 
  rename(diet_pc1 = "PC1", diet_pc2 = "PC2")

data_combin <- mydata_diet_pc |> select(id, diet_pc1, diet_pc2) |> 
  full_join(mydata_full, by = "id") |> 
  filter(age_screening > 19, age_screening < 45)  |> 
  drop_na(depression_cat) |> 
  mutate(depression_cat = factor(depression_cat, levels = c("no depression", "mild", "moderate", "moderately severe to severe"))) |>
  mutate(dep_cat2 = forcats::fct_collapse(depression_cat, `moderate to severe` = c("moderate", "moderately severe to severe"))) |> 
  # new category with categories "moderate" and "moderately severe to severe" collapsed together
  mutate(log10vitD = log(vitD_lab))


#### write objects to file

write.csv(mydata_diet_pc, "outputs/diet_latent_vars.csv", row.names = F)
save(data_combin, file = "outputs/prepped_data.RData")



# goals: estimate effect of Vitamin D on depression score given information on race and diet


#### read in data and prepare for analysis ----

# load libraries
library(dplyr); library(tidyr)
library(DataExplorer)
library(ggplot2)

# general study info:
# https://wwwn.cdc.gov/nchs/nhanes/Default.aspx

# explanation of weights for diet data for 2003-2004
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DR1IFF_C.htm

# example analysis files:
# https://wwwn.cdc.gov/nchs/data/tutorials/DB303_Fig1_R.R
# https://wwwn.cdc.gov/nchs/data/tutorials/module3_examples_R.r
# need to bring in 'SDMVSTRA' ("Masked variance unit pseudo-stratum variable for variance estimation") from demographics data set

# read in data and do some basic processing

mydata_full <- read.csv("data/NHANES_2007-2018_recoded.csv") 

diet_vars <- c("kcal", "protein", "carbs", "sugars", "fats", "sat_fats", "monosat_fats")
# not included: any vitamin D variable, and wic (which is largely missing)
# table(mydata_full$wic, useNA = "always")

mydata_diet <- mydata_full %>% 
  filter(age_screening > 19 & age_screening < 45) %>% 
  select(id, studypop, all_of(diet_vars))  %>% 
  mutate(studypop = as.factor(studypop))

# filter out missing diet data
mydata_diet_final <- mydata_diet %>% drop_na() # 12,839 data points

#### some checks ----
plot_missing(mydata_diet)

# dig more into missing data for diet vars
mydata_diet_na <- mydata_diet %>% mutate_all(is.na) # turns data into TRUE/FALSE for missing or not
cor(mydata_diet_na[,-1])
table(mydata_diet_na$kcal, mydata_diet_na$protein)
# they are all missing diet data at the same observations

# quick data viz
plot_histogram(mydata_diet_final, ncol = 2)
# there are many low values for diet
ggsave("outputs/diet_histograms.png")

lapply(mydata_diet_final, range, na.rm = TRUE)
cor(mydata_diet_final[,-(1:2)])
plot_boxplot(mydata_diet_final, by = "studypop")

# Statistical methods for estimating usual intake of nutrients and foods: a review of the theory
# https://www.sciencedirect.com/science/article/abs/pii/S0002822306017056
#. https://epi.grants.cancer.gov/dhq/dietcalc/index.html

#### correlation plots ----
library(psych)
data_cp <- mydata_diet_final %>% select(studypop, kcal, carbs, sugars, fats, sat_fats, monosat_fats, protein)
colnames(data_cp) <- c("studypop", "kcal", "carbohydrates", "sugars", "fats",   
                       "saturated fats", "monounsaturated", "protein")

col1 <- rgb(0/255, 153/255, 76/255, 0.5) # green (men)
col2 <- rgb(255/255, 0/255, 127/255, 0.6) # magenta (non-PP)
col3 <- rgb(102/255, 102/255, 255/255, 0.6) # periwinkle (postpartum)
col4 <- rgb(255/255, 128/255, 0/255, 0.6) # orange (pregnant)

levels(data_cp$studypop)

# practice plot
data(iris)
pairs.panels(iris[1:4], 
             fg = "magenta", bg = "khaki", col = "blue", pch = 21, 
             col.axis = "green2", col.lab = "red", col.main = "purple", col.sub = "darkcyan",
             hist.col = "white", 
             #bg=c(col1, col2, col3)[iris$Species], pch = 21, 
             main="Fisher Iris data by Species", cex.cor = 1, cex = 3) 

# final plot
png("outputs/pairs_plot.png", 
    width = 2000, height = 2000, bg = "transparent")
pairs.panels(data_cp[,-1], hist.col = "gray80", cex.axis = 3.5,
             cex.cor = 0.5, cex = 2.5, main = NA, cex.labels = 4,
             smooth = FALSE, density = FALSE, ellipses = FALSE, pch = 21, 
             bg = c(col1, col2, col3, col4)[mydata_diet_final$studypop])
dev.off()

# library(GGally)
# ggpairs(mydata_diet_final, 
#         columns = c("kcal", "carbs", "sugars", "fats", "sat_fats", "monosat_fats", "protein"),
#         #lower = list(mapping = aes(alpha = 0.5)), 
#         diag = list(continuous = "histDiag")) +
#   theme_bw()


# not that great of a plot
# diet_year <- select(mydata_full, studypop, study_year, all_of(diet_vars)) %>%
#   pivot_longer(cols = all_of(diet_vars), names_to = "diet_vars") %>% drop_na(value, studypop)
# ggplot(diet_year, aes(x = study_year, y = value)) +
#   geom_boxplot(aes(fill = studypop, color = studypop), alpha = 0.6) +
#   facet_wrap(vars(diet_vars), scales = "free_y") +
#   theme_classic()
# 
# ggsave("outputs/diet_boxplots.png")

# they all have highly positive correlations
round(cor(mydata_diet_final), 2)

#### PCA ----

diet_pca <- prcomp(mydata_diet_final[,-(1:2)], center = TRUE, scale. = TRUE)

plot(diet_pca)
summary(diet_pca) #75% of variation summarized in first principal component

labs <- c("carbohydrates", "fats", "kcal", "monounsaturated",    
          "protein", "saturated fats", "sugars")

scores <- as.data.frame(diet_pca$rotation) %>% select(PC1, PC2) %>% 
  rename(`Principal Component 1 (74%)` = "PC1", `Principal component 2 (15%)` = "PC2") %>% 
  tibble::rownames_to_column("diet_vars") %>% 
  pivot_longer(cols = 2:3)

ggplot(scores, aes(y = diet_vars, x = value)) +
  geom_col() +
  facet_grid(. ~ name) +
  scale_y_discrete(labels = labs) + 
  xlab("") + ylab("") +
  theme_bw(base_size = 12) 

# pc1 = overall consumption
# pc2 = "paleo" (carbs/sugars vs protein/fats)

ggsave("outputs/diet_pca_loadings.png", width = 1750, height = 1400, units = "px")

rotations <- as.data.frame(diet_pca$x[,1:2])
mydata_diet_pc <- cbind(mydata_diet_final, rotations) %>% 
  rename(diet_pc1 = "PC1", diet_pc2 = "PC2")

write.csv(mydata_diet_pc, "outputs/diet_latent_vars.csv", row.names = F)


library(ggbiplot)
biplot(diet_pca)

ggbiplot(diet_pca,
         alpha = 0.5,
         ellipse = TRUE,
         groups = mydata_diet_final$studypop) +
  theme_bw()

ggbiplot(diet_pca,
         alpha = 0.5,
         ellipse = TRUE,
         groups = mydata_diet_final$) +
  theme_bw()

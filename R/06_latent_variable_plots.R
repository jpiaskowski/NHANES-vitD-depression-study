
library(dplyr); library(tidyr)
library(ggplot2);

mydata_full <- read.csv("data/NHANES_2007-2018_recoded.csv") 
mydata_diet <- read.csv("outputs/diet_latent_vars.csv")

data_combin <- mydata_diet %>% select(id, diet_pc1, diet_pc2) %>% 
  left_join(mydata_full, by = "id") 

ggplot(data_combin, aes(x = diet_pc1, y = diet_pc2)) +
  geom_point(alpha = 0.2, col = "blue") +
  geom_density_2d(color = "black", linewidth = 0.5) +
  theme_classic()

ggplot(data_combin, aes(x = diet_pc1, y = diet_pc2)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(alpha = 0.2, col = "black") +
  geom_density_2d_filled(alpha = 0.5) +
  coord_fixed() +
  theme_classic()

hist(data_combin$diet_pc1)
hist(data_combin$diet_pc2)
cor(data_combin$diet_pc1, data_combin$diet_pc2)

boxplot(diet_pc1 ~ studypop, data = data_combin)
boxplot(diet_pc2 ~ studypop, data = data_combin)

#data_combin %>% select(id, studypop, diet_pc1, diet_pc2) %>% 
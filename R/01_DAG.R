
library(dagitty)

# DAG after input from the literature
g1 <- dagitty( "dag{
      VitD -> Depression <- Ed -> SES -> FoodSecur 
      Depression <- SES -> Diet <- FoodSecur -> Depression
      Depression <- Diet -> VitD
               }")


print(adjustmentSets(g1, exposure = "VitD", outcome = "Depression", 
                     effect = "direct"))
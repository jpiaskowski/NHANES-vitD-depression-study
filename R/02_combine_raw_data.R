#### PART 1: Load Data and Combine -------------------

#Making file of everyone together 2007-2018
#Reproductive health file is excluded because it would automatically kick out men 

require(Hmisc)
library(purrr); library(dplyr); library(tidyr) 
require(psych)
library(forcats)

setwd("data/raw_data") # only needed for import step

# function to remove those dastardly labels
remove_label <- function(obj) {
  write.csv(obj, "temp.csv", row.names = FALSE)
  rm(obj)
  new <- read.csv("temp.csv")
  unlink("temp.csv")
  return(new)
}

raw_files <- dir()

data_files <- map(raw_files, sasxport.get) |> 
  map(., remove_label)

# reset working directory to project directory:
setwd(here::here())

# generate matrix to organize data sets
matrix_index <- matrix(1:48, nrow = 6, ncol = 8, byrow = FALSE)
# use matrix to nest the lists
nested_list <- apply(matrix_index, 2, function(x)
  as.list(data_files[x])
  )
  
# bind rows of data and merge toegther

data_combine <-  map(nested_list,  bind_rows) |> 
  map(mutate, seqn = as.character(seqn)) |> 
  reduce(full_join, by = "seqn")
  
# output file
write.csv(data_combine, "data/combined_data_full.csv", row.names = FALSE)

###### PART II: filter to needed vars -----------------------------------

# if you're starting here and don't need to rerun PART I
# data_combine <- read.csv("data/combined_data_full.csv")

# Variables to keep
keep.vars <-c("seqn", "sddsrvyr", "riagendr", "ridageyr", "ridexprg", "ridreth1", "ridexmon", 
              "dmdeduc2", "dmdmartl", "indfmpir", "dpq010", "dpq020", 
              "dpq030", "dpq040", "dpq050", "dpq060", "dpq070", "dpq080", "dpq090", 
              "fsdad", "fsd660zw", "lbxvidms", "lbxvd2ms", "lbxvd3ms", "dr1tvd", 
              "dr1tkcal", "dr1tprot", "dr1tcarb", "dr1tsugr", "dr1ttfat", 
              "dr1tsfat", "dr1tmfat", "sdmvpsu", "sdmvstra", 
              "dr1tpfat", "wtdrd1.x", "ds1tvd", "ds1ds", "bmxbmi", "rhq197", "rhq200") 

# select needed variables a few others
data_final <- data_combine |> select(all_of(keep.vars)) |> 
  rename(wtdrd1 = "wtdrd1.x") |> 
  mutate(across(matches("dpq0"), ~ na_if(., 7))) |> 
  mutate(across(matches("dpq0"), ~ na_if(., 9))) |> 
  mutate(dpqtotal = dpq010 + dpq020 + dpq030 + dpq040 + 
           dpq050 + dpq060 + dpq070 + dpq080 + dpq090) |> 
  mutate(weight12yr = 1/6 * wtdrd1) |> 
  mutate(studypop = case_when(
    riagendr == 1 ~ 4, # men
    riagendr == 2 & ridexprg == 1 ~ 1, # pregnant
    riagendr == 2 & ridexprg != 1 & (rhq197 > 12 | is.na(rhq197)) ~ 3, # non-postpartum # last argument (is.na) brings these estimates in line with Tori's
    riagendr == 2 & ridexprg != 1 & rhq197 <= 12 ~ 2, # postpartum
  )) 

# some checks
table(data_final$studypop, useNA= "always")
table(data_final$rhq197, data_final$ridexprg, data_final$riagendr, useNA= "always")
table(data_final$riagendr, useNA = "always")
table(data_final$riagendr, data_final$ridexprg, useNA = "always")
table(data_final$ridexprg)

# check of depression scores: 
data_final |> select(starts_with("dpq0")) |> map(., table)
table(data_final$dpqtotal)

#creating new survey weight variable from NHANES calculation instructions. 

describe(data_final$wtdrd1)
describe(data_final$weight12yr)

 # recoding race/ethnicty
table(data_final$ridreth1)
data_final$ridreth1[data_final$ridreth1 == 2] <- 1
table(data_final$ridreth1)


#recoding education 

table(data_final$dmdeduc2)  
data_final$dmdeduc2[data_final$dmdeduc2== 2] <- 1
#table(data_final$dmdeduc2) 
data_final$dmdeduc2[data_final$dmdeduc2== 9] <- NA
#table(data_final$dmdeduc2) 
data_final$dmdeduc2[data_final$dmdeduc2== 7] <- NA
table(data_final$dmdeduc2) 


#redcoding marital status

table(data_final$dmdmartl)
data_final$dmdmartl[data_final$dmdmartl== 6] <- 1
table(data_final$dmdmartl)
data_final$dmdmartl[data_final$dmdmartl== 3] <- 2
table(data_final$dmdmartl)
data_final$dmdmartl[data_final$dmdmartl== 4] <- 2
table(data_final$dmdmartl)
data_final$dmdmartl[data_final$dmdmartl== 99] <- NA
table(data_final$dmdmartl)
data_final$dmdmartl[data_final$dmdmartl== 77] <- NA
table(data_final$dmdmartl)

#recoding food security 

table(data_final$fsdad) 
data_final$fsdad[data_final$fsdad== 3] <- 2
table(data_final$fsdad) 
data_final$fsdad[data_final$fsdad== 4] <- 2
table(data_final$fsdad) 

#recoding WIC status 

table(data_final$fsd660zw)
data_final$fsd660zw[data_final$fsd660zw> 2] <- NA
table(data_final$fsd660zw)

#recoding breastfeeding status 

table(data_final$rhq200)
data_final$rhq200[data_final$rhq200> 2] <- NA
table(data_final$rhq200)


# final checks

table(data_final$ridexprg)
table(data_final$riagendr)
table(data_final$ridexmon)

#creating the combined excel file :)

write.csv(data_final, "data/NHANES_2007-2018_cleaned.csv", row.names = FALSE)


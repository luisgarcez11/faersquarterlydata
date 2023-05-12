


library(usethis)
products_fda <- read.csv("data-raw/products_FDA.txt", sep = "\t") %>% 
  distinct(ApplNo,  DrugName, ActiveIngredient)
usethis::use_data(products_fda)

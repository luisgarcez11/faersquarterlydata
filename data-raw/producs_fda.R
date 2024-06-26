


library(usethis)
products_fda <- read.csv("data-raw/products_FDA.txt", sep = "\t") %>% 
  distinct(ApplNo,  DrugName, ActiveIngredient)
usethis::use_data(products_fda)


als_faers_data$demographics = als_faers_data$demographics[1:1000,]
als_faers_data$drug = als_faers_data$drug[1:1000,]
als_faers_data$indication = als_faers_data$indication[1:1000,]
als_faers_data$outcome = als_faers_data$outcome[1:1000,]
als_faers_data$reaction = als_faers_data$reaction[1:3000,]
als_faers_data$therapy = als_faers_data$therapy[1:1000,]
usethis::use_data(als_faers_data, overwrite = TRUE)

als_faers_data_unified <- unify_tabular_ascii(als_faers_data)
usethis::use_data(als_faers_data_unified, overwrite = TRUE)

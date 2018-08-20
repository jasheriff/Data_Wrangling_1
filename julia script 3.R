
library ("dplyr")
library("tidryr")
library("stringr")
library("tidyselect")

elecdf <- read.csv("refine.csv", header=TRUE)
elecdf <- arrange(elecdf, company)
elecdf$company <- as.character(elecdf$company)
elecdf$company <- toupper(elecdf$company)
#1
elecdf <- elecdf %>% 
  mutate(company = replace(company, startsWith(company, "A"), "akzo")) %>%
  mutate(company = replace(company, str_detect(company, "LIPS") | str_detect(company, "LPS"), "phillips")) %>%
  mutate(company = replace(company, startsWith(company, "V"), "van houten")) %>%
  mutate(company = replace(company, startsWith(company, "U"), "unilever"))

#2
elecdf <- tidyr::separate(elecdf, "Product.code...number", c("product_code", "product_number"), sep = "-")

#3
elecdf <- elecdf %>% mutate(product_category = 
                              if_else(product_code == "p", "Smpartphone",
                                  if_else(product_code == "x", "Laptop",
                                     if_else(product_code == "v", "TV",
                                          if_else(product_code == "q", "Tablet", NA_character_)))))

#4 
elecdf <- unite(elecdf, full_address, address, city, country, sep = ",", remove=FALSE)

#5 
elecdf <- elecdf %>% 
  mutate("company_phillips" = if_else(company == "phillips", 1, 0)) %>%
  mutate("company_akzo" = if_else(company == "akzo", 1, 0)) %>%
  mutate("company_van houten" = if_else(company == "van houten", 1, 0)) %>%
  mutate("company_unilever" = if_else(company == "unilever", 1, 0)) %>%
  mutate("product_smartphone" = if_else(product_category == "Smartphone", 1, 0)) %>%
  mutate("product_tablet" = if_else(product_category == "Tablet", 1, 0)) %>%
  mutate("product_tv" = if_else(product_category == "TV", 1, 0)) %>%
  mutate("product_laptop" = if_else(product_category == "Laptop", 1, 0))

tbl_df(elecdf)
View(elecdf)

write.csv(elecdf, "refine_clean.csv")
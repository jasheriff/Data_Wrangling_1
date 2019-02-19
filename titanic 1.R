 
library ("dplyr")
library("tidyr")

titanic <- read.csv("titanic.csv", header=TRUE)
titanic <- tbl_df(titanic)

#1 Embarked
#Find the missing values and replace them with S in the "embarked" column 
titanic <- titanic %>%
  mutate(embarked = replace(embarked, embarked == "", "S"))

#2 Age
#Calculate the mean of the Age column and use that value to populate the missing values.
#We could also have used the median age. 
#I would pick the median because this value isn't effected by outliers as much as the mean. 

meanage <- mean(titanic$age, na.rm = TRUE)

titanic <- titanic %>%
  mutate(age = replace(age, is.na(age), meanage))

#3 Boat
#Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

titanic <- titanic %>%
  mutate(boat = replace(boat, boat == "", "NA"))

#4 Cabin
#It does not make sense to fill missing cabin numbers.
#These are nominal values. 
#They could be missing because data was lost, or people didn't have an assigned cabin to begin with.

titanic <- titanic %>%
  mutate("has_cabin_number" = if_else(cabin == "", 0, 1))

View(titanic)
write.csv(titanic, "titanic_clean.csv")

libs <- c("lubridate","plyr", "dplyr","reshape2","reshape", "tidyr")
# the function below first checks if package is installed
# if not, it installs first
invisible(lapply(libs, function(toLoad) {
  check <- require(toLoad, character.only = T)
  if(check == F) {
    print(paste("Loading:", toLoad))
    install.packages(toLoad)
    require(toLoad)
  }
}))


#1. Load VCP data 

setwd("/Users/zsofihesketh/Downloads")
dir <- getwd()

install.packages("haven")
library(haven)

data <- read.csv("2023_VCP_data.csv")

countries_to_keep <- c("Brazil", "Cameroon", "DRC", "Ethiopia", "India", "Indonesia", 
                       "Kenya", "Liberia", "Nigeria", "Pakistan", "Philippines", "Russia", 
                       "Sierra Leone", "South Africa", "Thailand", "Uganda", "Ukraine", "Vietnam")

#####################################################################

#SEX ANALYSIS

#2. Data pre-processing: first for "vaccines are important for all" variable 
#2a. Filter dataset for VaxImp
gender_VaxImp <- data %>%
  select(Country, Gender, VaxImp)

#2b. View the filtered dataset and remove any Don't know or Other values for Gender variable 
head(gender_VaxImp)
table(gender_VaxImp$Gender)
gender_VaxImp <- gender_VaxImp %>%
  filter(!(Gender %in% c("Other", "Don't know/ Prefer not to say")))

#2c. Filter out all other countries than the 18 high TB burden countries in the set
gender_VaxImp <- gender_VaxImp %>%
  filter(Country %in% countries_to_keep)
table(gender_VaxImp$VaxImp)

#2d. Filter out missing responses to the VaxImp statement 
gender_VaxImp <- gender_VaxImp %>%
  filter(VaxImp != "Don't know/ Refused")

#3. VaxImp: Analysis of proportions considered confident in VaxImp by country by sex
proportions <- gender_VaxImp %>%
  filter(Gender %in% c("Male", "Female")) %>%
  group_by(Country, Gender) %>%
  summarise(
    total = n(),
    agree_count = sum(VaxImp %in% c("Tend to Agree", "Strongly agree")),
    proportion_agree = agree_count / total,
    .groups = "drop"
  )

#View the result
print(proportions, n = Inf)

#View neat table of the proportions
proportions %>%
  arrange(Country, Gender) %>%
  knitr::kable(digits = 3, col.names = c("Country", "Gender", "Total", "Agree Count", "Proportion Agree"))


#4.Data preprocessing: repeat steps for "vaccines are safe" variable
#4a. Filter dataset for VaxSaf
gender_VaxSaf <- data %>%
  select(Country, Gender, VaxSaf)

#4b. View the filtered dataset and remove any Don't know or Other values for Gender variable 
head(gender_VaxSaf)
table(gender_VaxSaf$Gender)
gender_VaxSaf <- gender_VaxSaf %>%
  filter(!(Gender %in% c("Other", "Don't know/ Prefer not to say")))

#4c. Filter out all other countries than the 18 high TB burden countries in the set
gender_VaxSaf <- gender_VaxSaf %>%
  filter(Country %in% countries_to_keep)
table(gender_VaxSaf$VaxSaf)

#4d. Filter out missing responses to the VaxSaf statement 
gender_VaxSaf <- gender_VaxSaf %>%
  filter(VaxSaf != "Don't know/ Refused")

#5. VaxSaf: Analysis of proportions considered confident in VaxSaf by country by sex
proportions <- gender_VaxSaf %>%
  filter(Gender %in% c("Male", "Female")) %>%
  group_by(Country, Gender) %>%
  summarise(
    total = n(),
    agree_count = sum(VaxSaf %in% c("Tend to Agree", "Strongly agree")),
    proportion_agree = agree_count / total,
    .groups = "drop"
  )

#View the result
print(proportions, n = Inf)

#View neat table of the proportions
proportions %>%
  arrange(Country, Gender) %>%
  knitr::kable(digits = 3, col.names = c("Country", "Gender", "Total", "Agree Count", "Proportion Agree"))

#6. Data preprocessing: repeat steps for "vaccines are effective" variable
#6a. Filter dataset for VaxEff

gender_VaxEff <- data %>%
  select(Country, Gender, VaxEff)

#6b. View the filtered dataset and remove any Don't know or Other values for Gender variable 
head(gender_VaxEff)
table(gender_VaxEff$Gender)
gender_VaxEff <- gender_VaxEff %>%
  filter(!(Gender %in% c("Other", "Don't know/ Prefer not to say")))

#6c. Filter out all other countries than the 18 high TB burden countries in the set
gender_VaxEff <- gender_VaxEff %>%
  filter(Country %in% countries_to_keep)
table(gender_VaxEff$VaxEff)

#6d. Filter out missing responses to the VaxSaf statement 
gender_VaxEff <- gender_VaxEff %>%
  filter(VaxEff != "Don't know/ Refused")

#7. VaxEff: Analysis of proportions considered confident in VaxEff by country by sex
proportions <- gender_VaxEff %>%
  filter(Gender %in% c("Male", "Female")) %>%
  group_by(Country, Gender) %>%
  summarise(
    total = n(),
    agree_count = sum(VaxEff %in% c("Tend to Agree", "Strongly agree")),
    proportion_agree = agree_count / total,
    .groups = "drop"
  )

#View the result
print(proportions, n = Inf)

#View neat table of the proportions
proportions %>%
  arrange(Country, Gender) %>%
  knitr::kable(digits = 3, col.names = c("Country", "Gender", "Total", "Agree Count", "Proportion Agree"))


#####################################################################

#AGE GROUP ANALYSIS 

#8. Data preprocessing: first for "vaccines are important" variable 
#8a. Filter the dataset for VaxImp 
age_VaxImp <- data %>%
  select(Country, Age_Group, VaxImp)

#8b. View the filtered dataset and remove any Don't know or Other values for Age_Group variable
head(age_VaxImp)
table(age_VaxImp$Age_Group)
age_VaxImp <- age_VaxImp %>%
  filter(!(Age_Group %in% "Don't know/ Prefer not to say"))

#8c. Filter out all other countries than the 18 high TB burden countries in the set
age_VaxImp <- age_VaxImp %>%
  filter(Country %in% countries_to_keep)
table(age_VaxImp$VaxImp)

#8d. Filter out missing responses to the VaxSaf statement 
age_VaxImp <- age_VaxImp %>%
  filter(VaxImp != "Don't know/ Refused")

#9. VaxImp: Analysis of proportions considered confident in VaxImp by country by age group
proportions_ageimp <- age_VaxImp %>%
  group_by(Country, Age_Group) %>%
  summarise(
    total = n(),
    positive = sum(VaxImp %in% c("Strongly agree", "Tend to Agree"))
  ) %>%
  mutate(proportions_ageimp = positive / total) %>%
  ungroup()

#View result and write .csv if desired; or can reproduce neat table as shown in the sex-disaggregated anaysis above if desired
print(proportions_ageimp, n = Inf)
write.csv(proportions_ageimp,"/Users/zsofihesketh/Downloads/proportion_age_vaximp.csv", row.names=TRUE)


#10: Data preprocessing: first for "vaccines are safe" variable 
#10a. Filter the dataset for VaxSaf 
age_VaxSaf <- data %>%
  select(Country, Age_Group, VaxSaf)

#10b. View the filtered dataset and remove any Don't know or Other values for Age_Group variable
head(age_VaxSaf)
table(age_VaxSaf$Age_Group)
age_VaxSaf <- age_VaxSaf %>%
  filter(!(Age_Group %in% "Don't know/ Prefer not to say"))

#10c. Filter out all other countries than the 18 high TB burden countries in the set
age_VaxSaf <- age_VaxSaf %>%
  filter(Country %in% countries_to_keep)
table(age_VaxSaf$VaxSaf)

#10d. Filter out missing responses to the VaxSaf statement 
age_VaxSaf <- age_VaxSaf %>%
  filter(VaxSaf != "Don't know/ Refused")

#11. VaxSaf: Analysis of proportions considered confident in VaxSaf by country by age group
proportions_agesaf <- age_VaxSaf %>%
  group_by(Country, Age_Group) %>%
  summarise(
    total = n(),
    positive = sum(VaxSaf %in% c("Strongly agree", "Tend to Agree"))
  ) %>%
  mutate(proportions_agesaf = positive / total) %>%
  ungroup()

#View result and write .csv if desired; or can reproduce neat table as shown in the sex-disaggregated anaysis above if desired
print(proportions_agesaf, n = Inf)
write.csv(proportions_agesaf,"/Users/zsofihesketh/Downloads/proportion_age_vaxsaf.csv", row.names=TRUE)

#12: Data preprocessing: first for "vaccines are effective" variable 
#12a. Filter the dataset for VaxEff 
age_VaxEff <- data %>%
  select(Country, Age_Group, VaxEff)

#12b. View the filtered dataset and remove any Don't know or Other values for Age_Group variable
head(age_VaxEff)
table(age_VaxEff$Age_Group)
age_VaxEff <- age_VaxEff %>%
  filter(!(Age_Group %in% "Don't know/ Prefer not to say"))

#12c. Filter out all other countries than the 18 high TB burden countries in the set
age_VaxEff <- age_VaxEff %>%
  filter(Country %in% countries_to_keep)
table(age_VaxEff$VaxEff)

#12d. Filter out missing responses to the VaxSaf statement 
age_VaxEff <- age_VaxEff %>%
  filter(VaxEff != "Don't know/ Refused")

#13.VaxEff: Analysis of proportions considered confident in VaxEff by country by age group
proportions_ageeff <- age_VaxEff %>%
  group_by(Country, Age_Group) %>%
  summarise(
    total = n(),
    positive = sum(VaxEff %in% c("Strongly agree", "Tend to Agree"))
  ) %>%
  mutate(proportions_ageeff = positive / total) %>%
  ungroup()

#View result and write .csv if desired; or can reproduce neat table as shown in the sex-disaggregated anaysis above if desired
print(proportions_ageeff, n = Inf)
write.csv(proportions_ageeff,"/Users/zsofihesketh/Downloads/proportion_age_vaxeff.csv", row.names=TRUE)
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

########################################

#1. Load VCP data and convert into required object class 

#1a. Set directory and install haven package
setwd("/Users/zsofihesketh/Downloads")
dir <- getwd()
install.packages("haven")
library(haven)

#1b. Read original .sav file
data <- read_sav("2023_VCP_data.sav")

#1c. Convert .sav data to factor class
data_factor <- as_factor(data)

#1d. Save factor class data as new .csv file
write.csv(data_factor,"/Users/zsofihesketh/Downloads/2023_VCP_data.csv", row.names=TRUE)

#1e. Reopen as new .csv file, now in dataset form 
data <- read.csv("2023_VCP_data.csv")

########################################

#2. Explore the dataset and its variables 

#2a. Explore Country variable
#There are responses from 70 countries, 18 of which are on one of the three WHO high TB burden lists
#These 18 are Brazil, Cameroon, Democratic Republic of the Congo (DRC), Ethiopia, India, Indonesia, Kenya, Liberia, Nigeria, Pakistan, Philippines, Russia, Sierra Leone, South Africa, Thailand, Uganda, Ukraine, Vietnam
length(unique(data$Country)) 
unique(data$Country)

#2b. Explore the survey response categories to VaxImp
#1 == Strongly Agree, 2 == Tend to agree, 3 == Tend to disagree, 4 == Strongly disagree #5 == Don't know/ Refused (No NAs)
unique(data$VaxImp)
sum(is.na(data$VaxImp))

#2c. Explore Age_Group variable: groups are 18-24, 25-34, 35-44, 55+, Don't know/ Prefer not to say (no NAs)#11,323 are 18-24 
#15,553 are 25-35 
#13,555 are 35-44 
#10,963 are 45-54
#19,289 are 55+, making this the most populous age group in this survey overall 
#98 are Don't know/ Prefer not to say 
unique(data$Age_Group)
table(data$Age_Group)

#3. Preprocess data 
#3a. Filter out all entries for Don't know/ Refused for "Vaccines are important for all" (VaxImp), "Vaccines are safe" (VaxSaf) and "Vaccines are effective" (VaxEff)
data_clean_VaxImp <- subset(data, VaxImp %in% c("Strongly agree","Tend to Agree","Tend to Disagree", "Strongly disagree"))
unique(data_clean_VaxImp$VaxImp)

data_clean_VaxSaf <- subset(data, VaxSaf %in% c("Strongly agree","Tend to Agree","Tend to Disagree", "Strongly disagree"))
unique(data_clean_VaxSaf$VaxSaf)

data_clean_VaxEff <- subset(data, VaxEff %in% c("Strongly agree","Tend to Agree","Tend to Disagree", "Strongly disagree"))
unique(data_clean_VaxEff$VaxEff)

#3b. Categorise into 'positive' and 'negative' for easier proportion estimation 
data_clean_VaxImp$VaxImp_grouped <- ifelse(data_clean_VaxImp$VaxImp %in% c("Strongly agree", "Tend to Agree"), "Positive", "Negative")
unique(data_clean_VaxImp$VaxImp_grouped)

data_clean_VaxSaf$VaxSaf_grouped <- ifelse(data_clean_VaxSaf$VaxSaf %in% c("Strongly agree", "Tend to Agree"), "Positive", "Negative")
unique(data_clean_VaxSaf$VaxSaf_grouped)

data_clean_VaxEff$VaxEff_grouped <- ifelse(data_clean_VaxEff$VaxEff %in% c("Strongly agree", "Tend to Agree"), "Positive", "Negative")
unique(data_clean_VaxEff$VaxEff_grouped)

#3c. Filter out all countries that are not those we are interested in
#We are left with data from 18 countries which figure on the high TB burden list 
data_clean_VaxImp <- subset(data_clean_VaxImp, Country %in% c("Brazil", "Cameroon", "DRC", "Ethiopia", "India", "Indonesia", "Kenya", "Liberia", "Nigeria", "Pakistan", "Philippines", "Russia", "Sierra Leone", "South Africa", "Thailand", "Uganda", "Ukraine", "Vietnam"))
data_clean_VaxSaf <- subset(data_clean_VaxSaf, Country %in% c("Brazil", "Cameroon", "DRC", "Ethiopia", "India", "Indonesia", "Kenya", "Liberia", "Nigeria", "Pakistan", "Philippines", "Russia", "Sierra Leone", "South Africa", "Thailand", "Uganda", "Ukraine", "Vietnam"))
data_clean_VaxEff <- subset(data_clean_VaxEff, Country %in% c("Brazil", "Cameroon", "DRC", "Ethiopia", "India", "Indonesia", "Kenya", "Liberia", "Nigeria", "Pakistan", "Philippines", "Russia", "Sierra Leone", "South Africa", "Thailand", "Uganda", "Ukraine", "Vietnam"))


#4. Main analysis: Vaccines are important for all - individual statement confidence score
#4a. Get table of response types in overall 18 country sample, plus positive/negative responses categorised breakdown
#16,321 positive versus 2237 negative in the whole sample = 86.3% positive (confident) across all 18 countries
table(data_clean_VaxImp$VaxImp)
table(data_clean_VaxImp$VaxImp_grouped)

#4b. Get number of positive/negative responses per country, then calculate proportion
#We see that the lowest proportion positive is Cameroon, while the highest is Vietnam 
table(data_clean_VaxImp$VaxImp_grouped, data_clean_VaxImp$Country)

VaxImp_country_scores <- data_clean_VaxImp %>%
  group_by(Country) %>%
  summarise(positive_percent = mean(VaxImp_grouped == "Positive") * 100)

print(VaxImp_country_scores)


#5. Main analysis: Vaccines are safe - individual statement confidence score
#5a. Get table of response types in overall 18 country sample, plus positive/negative responses categorised breakdown
#15,203 positive versus 3121 negative in the whole sample = 79.5% positive (confident) across all 18 countries
table(data_clean_VaxSaf$VaxSaf)
table(data_clean_VaxSaf$VaxSaf_grouped)

#5b.Get number of positive/negative responses per country, then calculate proportion
#We see that the lowest proportion positive is Cameroon, with a lower number than VaxImp, while the highest is still in Vietnam; Ukraine, Russia and Thailan'd scores are markedly lower than for VaxImp
table(data_clean_VaxSaf$VaxSaf_grouped, data_clean_VaxSaf$Country)

VaxSaf_country_scores <- data_clean_VaxSaf %>%
  group_by(Country) %>%
  summarise(positive_percent = mean(VaxSaf_grouped == "Positive") * 100)

print(VaxSaf_country_scores)


#6. Main analysis: Vaccines are effective - individual statement confidence score
#6a. Get table of response types in overall 18 country sample, plus positive/negative responses categorised breakdown
#15,278 positive versus 3,072 negative in the whole sample =  79.9% positive (confident) across all 18 countries
table(data_clean_VaxEff$VaxEff)
table(data_clean_VaxEff$VaxEff_grouped)

#6b.Get number of positive/negative responses per country, then calculate proportion
#We see that the lowest proportion positive is Cameroon, while the highest is Vietnam 
table(data_clean_VaxEff$VaxEff_grouped, data_clean_VaxEff$Country)

VaxEff_country_scores <- data_clean_VaxEff %>%
  group_by(Country) %>%
  summarise(positive_percent = mean(VaxEff_grouped == "Positive") * 100)

print(VaxSaf_country_scores)


#7. Main analysis: generate combined confidence score per country based on three individual statement scores 
#Cameroon has the lowest composite score of 63.0% confident, Vietnam has the highest of 98.5% confident
composite_scores <- dplyr::rename(VaxImp_country_scores, Imp = positive_percent) %>%
  inner_join(dplyr::rename(VaxSaf_country_scores, Saf = positive_percent), by = "Country") %>%
  inner_join(dplyr::rename(VaxEff_country_scores, Eff = positive_percent), by = "Country") %>%
  dplyr::mutate(Composite = (Imp + Saf + Eff) / 3)

print(composite_scores)

########################################
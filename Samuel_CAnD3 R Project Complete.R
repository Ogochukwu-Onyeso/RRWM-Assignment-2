census <- read.csv("G:/RStudio Files/Census_Data.csv") # sets working directory
colnames(census) <- c("PPSOR", "HDGREE", "AGEIMM", "IMMSTAT", "POB", "PR", "SEX", "TOTINC")
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Remove rows with TOTINC of 88,888,888 or 99,999,999, as they count as invalid data here.
census_clean <- census %>%
  filter(!TOTINC %in% c(88888888, 99999999))

# Recoding POB into a binary variable (1 for outside Canada, 0 for born in Canada)
census_clean <- census_clean %>%
  mutate(POB_Binary = ifelse(POB ==1, 0, 1))

# Recoding IMMSTAT into Binary variable (1 for immigrant, 0 for non-immigrant)
census_clean <- census_clean %>%
  mutate(ImmigrationStat = ifelse(IMMSTAT ==1, 0, 1))

# I would like to see the number of non-immigrants born outside Canada, for nothing other than curiosity.
non_immigrants_outside_canada <- census_clean %>%
  filter(POB_Binary == 1 & IMMSTAT == 1) %>%
  count()

print(non_immigrants_outside_canada)

# We can now get into the recoding of income - it is in the census as a continuous variable, but I would like it to be broken down into categories with increments of $20,000.
census_clean <- census_clean %>%
  mutate(TOTINC_category = case_when(
    TOTINC <= 19999 ~ 0,
    TOTINC >= 20000 & TOTINC <= 49999 ~ 1,
    TOTINC >= 40000 & TOTINC <= 59999 ~ 2,
    TOTINC >= 60000 & TOTINC <= 79999 ~ 3,
    TOTINC >= 80000 & TOTINC <= 99999 ~ 4,
    TOTINC >= 1000000 & TOTINC <= 119999 ~ 5,
    TOTINC >= 120000 & TOTINC <= 139999 ~ 6,
    TOTINC >= 140000 ~ 7,
    TRUE ~ NA_real_
  ))
# I would also, for the sake of the logistic regression I will run in a moment, create a binary income variable, representing top earners versus non-top earners. This will be an arbitrary number for the sake of this exercise, though if my memory serves me, it is around the $80,000 mark.
census_clean <- census_clean %>%
  mutate(topearner = ifelse(TOTINC >= 80000, 1, 0))

# I would also like to recode HDGREE into a slightly nicer variable,where 1=no degree, 2=degree below bachelors, 3=bachelors degree or above, 4=masters, 5=phd, and 6=medicine degree, while 99 indicates no response or not applicable.
census_clean <- census_clean %>%
  mutate(DegreeEarned = case_when(
    HDGREE == 1 ~ 1,
    HDGREE %in% 2:8 ~ 2,
    HDGREE %in% 9:10 ~ 3,
    HDGREE == 12 ~ 4,
    HDGREE == 13 ~ 5,
    HDGREE == 11 ~ 6,
    HDGREE %in% c(88, 99) ~ 99,
    TRUE ~ NA_real_
  ))


# Summary table of immigrants vs non-immigrants making above $80,000 to visualize. 
summary_table <- census_clean %>%
  filter(topearner >= 0) %>%
  group_by(POB_Binary, ImmigrationStat) %>%
  summarise(count = n()) %>%
  ungroup()

print(summary_table)

#With everything recoded, we can get to our binary logistic regression to look at the dependent variable (top earner or no) and if and how it is affected by the individuals place of birth, immigration status, gender, degree, province of residence, and age at immigration.
logit_model <- glm(topearner ~ POB_Binary + ImmigrationStat + SEX + DegreeEarned + PR + AGEIMM,
                   data = census_clean, 
                   family = "binomial")

#summary of the regression
summary(logit_model)

nrow(census_clean)

colnames(census) <- c("PPSORT", "HDGREE", "AGEIMM", "IMMSTAT", "POB", "PR", "Sex", "TotInc")

library(tidyverse)
census <- read.csv("Census 2016.csv")

# Remove rows with TOTINC of 88,888,888 or 99,999,999, as they count as invalid data here
census_clean <- census %>%
  filter(!TotInc %in% c(88888888, 99999999))

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

census_clean <- census_clean %>%
  mutate(TotInc_category = case_when(
    TotInc <= 19999 ~ 0,
    TotInc >= 20000 & TotInc <= 49999 ~ 1,
    TotInc >= 40000 & TotInc <= 59999 ~ 2,
    TotInc >= 60000 & TotInc <= 79999 ~ 3,
    TotInc >= 80000 & TotInc <= 99999 ~ 4,
    TotInc >= 1000000 & TotInc <= 119999 ~ 5,
    TotInc >= 120000 & TotInc <= 139999 ~ 6,
    TotInc >= 140000 ~ 7,
    TRUE ~ NA_real_
  ))

# Create a binary variable for income, for logistic regression 
census_clean <- census_clean %>%
  mutate(topearner = ifelse(TotInc >= 80000, 1, 0))

# Recode education to 6 levels
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


nrow(census_clean)
logit_model <- glm(topearner ~ POB_Binary + ImmigrationStat + Sex + DegreeEarned + PR + AGEIMM,
                   data = census_clean, 
                   family = "binomial")

#summary of the regression
summary(logit_model)


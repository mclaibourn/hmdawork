# Read in and examine HMDA data for Charlottesville
# June 12, 2019

# Load libraries
library(tidyverse)

# Read in data
hmda_cvl <- read_csv("hmda_lar_charlottesville_2007_2017.csv")
hmda_alb <- read_csv("hmda_lar_albemarle_2007_2017.csv")

# Combine data
hmda <- bind_rows(list(hmda_cvl, hmda_alb))
# hmda <- hmda %>% select(-c(state_name, msamd_name))

# Data cleaning: make categorical variables factors
# Factor: everything that contains _name
var <- names(hmda) # vector of column names
var <- str_subset(var, "_name") # just column names with "_name" in them

hmda <- hmda %>% 
  mutate_at(var, as.factor)

# Numeric: rate_spread
# hmda_cvl <- hmda_cvl %>% 
#   mutate(rate_spread = as.numeric(rate_spread))
# hmda_alb <- hmda_alb %>% 
#   mutate(rate_spread = as.numeric(rate_spread))

# need indicators for denial and/or originated (not sure what loan purchased represents), 
#   indicators for race of applicant (primary, co-applicant?)
#   use only home purchase; all loans?

# then group by year and census tract to get data frame with
#   number of loans started, loans originated, loans denied
#   number of loans by race (Black, White, Asian, Unknown)
#   median/average loan amount, applicant income

# census tract-year aggregation example: Charlottesville
# TOTALS
# homes purchase loans made
purchase <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name == "Home purchase") %>% 
  group_by(census_tract_number, as_of_year) %>% 
  summarize(purchase = n())

# refinance loans made
refinance <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinance = n())

hmda_cenyear <- full_join(purchase, refinance)

# home purchase loan denials
den_purchase <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_purchase = n())

hmda_cenyear <- full_join(hmda_cenyear, den_purchase)

# refinance loan denials
den_refinance <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_refinance = n())

hmda_cenyear <- full_join(hmda_cenyear, den_refinance)

# purchase loans bought by bank
bank_purchase <- hmda %>% 
  filter(action_taken_name == "Loan purchased by the institution" & loan_purpose_name == "Home purchase") %>% 
  group_by(census_tract_number, as_of_year) %>% 
  summarize(bank_purchase = n())

# BY RACE
# home puchase loans made - white
purchase_white <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(purchase_white = n())

hmda_cenyear <- full_join(hmda_cenyear, purchase_white)

# home puchase loans made - black
purchase_black <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(purchase_black = n())

hmda_cenyear <- full_join(hmda_cenyear, purchase_black)

# refinance loans made - white
refinance_white <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinance_white = n())

hmda_cenyear <- full_join(hmda_cenyear, refinance_white)

# refinance loans made - black
refinance_black <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinance_black = n())

hmda_cenyear <- full_join(hmda_cenyear, refinance_black)

# home purchase loan denials - white
den_purchase_white <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_purchase_white = n())

hmda_cenyear <- full_join(hmda_cenyear, den_purchase_white)

# home purchase loan denials - black
den_purchase_black <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_purchase_black = n())

hmda_cenyear <- full_join(hmda_cenyear, den_purchase_black)

# refinance loan denials - white
den_refinance_white <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_refinance_white = n())

hmda_cenyear <- full_join(hmda_cenyear, den_refinance_white)

# refinance loan denials - black
den_refinance_black <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name == "Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(den_refinance_black = n())

hmda_cenyear <- full_join(hmda_cenyear, den_refinance_black)

# purchase loans bought by bank-white
bank_purchase_white <- hmda %>% 
  filter(action_taken_name == "Loan purchased by the institution" & loan_purpose_name == "Home purchase" & applicant_race_name_1 == "White") %>% 
  group_by(census_tract_number, as_of_year) %>% 
  summarize(bank_purchase_white = n())

hmda_cenyear <- full_join(hmda_cenyear, bank_purchase_white)

# purchase loans bought by bank-black
bank_purchase_black <- hmda %>% 
  filter(action_taken_name == "Loan purchased by the institution" & loan_purpose_name == "Home purchase" & applicant_race_name_1 == "White") %>% 
  group_by(census_tract_number, as_of_year) %>% 
  summarize(bank_purchase_black = n())

hmda_cenyear <- full_join(hmda_cenyear, bank_purchase_black)

# MEDIANS
# median loan amount
med_loan_purchase <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_purchase = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_purchase)

# median refinance amount
med_loan_refinance <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_refinance = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_refinance)

# median income for purchase loan
med_inc_purchase <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_purchase = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_purchase)

# median income for refinance loan
med_inc_refinance <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_refinance = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_refinance)

# MEDIANS BY RACE
# median purchase loan amount-white
med_loan_purchase_white <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_purchase_white = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_purchase_white)

# median purchase loan amount-black
med_loan_purchase_black <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_purchase_black = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_purchase_black)

# median refinancing loan amount-white
med_loan_refinance_white <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_refinance_white = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_refinance_white)

# median refinancing loan amount-black
med_loan_refinance_black <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_loan_refinance_black = median(loan_amount_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_loan_refinance_black)

# median income for purchase loan-white
med_inc_purchase_white <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_purchase_white = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_purchase_white)

# median income for purchase loan-black
med_inc_purchase_black <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_purchase_black = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_purchase_black)

# median income for refinance loan-white
med_inc_refinance_white <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_refinance_white = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_refinance_white)

# median income for refinance loan-black
med_inc_refinance_black <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(med_inc_refinance_black = median(applicant_income_000s, na.rm = TRUE))

hmda_cenyear <- full_join(hmda_cenyear, med_inc_refinance_black)


saveRDS(hmda_cenyear, file = "hmda_cenyear.RDS")
# hmda <- readRDS("hmda_cenyear.RDS")




# 2017
hmda_cen17 <- hmda_cy %>% filter(as_of_year == 2017) %>% filter(!is.na(purchase))
hmda17 <- hmda %>% filter(as_of_year == 2017)

table(hmda17$loan_purpose_name, hmda17$action_taken_name)

hmda17 %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name == "Home purchase") %>% 
  group_by(census_tract_number) %>% 
  summarize(purchase = n())

ggplot(hmda17, aes(x = applicant_income_000s, y = loan_amount_000s)) +
  geom_point() + geom_smooth() +
  coord_cartesian(xlim = c(0,500), ylim = c(0, 1000))

temp <- hmda17 %>% group_by(applicant_race_name_1) %>% count(action_taken_name)

temp <- hmda %>% group_by(census_tract_number, as_of_year) %>% 
  summarize(mean(minority_population))

# model loan amount
hmda_purchase <- hmda %>% filter(loan_purpose_name == "Home purchase")



# model denial
hmda <- hmda %>% 
  mutate(denial = if_else(action_taken_name == "Application denied by financial institution", 1, 0),
         race = fct_recode(applicant_race_name_1, 
                           white = "White", 
                           black = "Black or African American",
                           asian = "Asian",
                           unknown = "Information not provided by applicant in mail, Internet, or telephone application",
                           unknown = "Not applicable",
                           other = "American Indian or Alaska Native",
                           other = "Native Hawaiian or Other Pacific Islander"),
         race = fct_relevel(race, "white", "black", "asian", "other", "unknown"),
         purpose = loan_purpose_name,
         purpose = fct_relevel(purpose, "Home purchase", "Refinancing", "Home improvement"),
         property = property_type_name,
         property = fct_recode(property, 
                               manufactured = "Manufactured housing",
                               multifamily = "Multifamily dwelling",
                               family = "One-to-four family dwelling (other than manufactured housing)"),
         property = fct_relevel(property, "family"),
         ownerocc = owner_occupancy_name,
         ownerocc = fct_recode(ownerocc,
                               yes = "Owner-occupied as a principal dwelling",
                               no = "Not owner-occupied as a principal dwelling",
                               no = "Not applicable"),
         ownerocc = fct_relevel(ownerocc, "no"))

table(hmda$race)

mod_deny <- glm(denial ~ race + purpose + property + ownerocc +
                  loan_amount_000s*applicant_income_000s +
                  minority_population, 
                data = hmda,
                family = "binomial")
summary(mod_deny)


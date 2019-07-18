# Income by tract data 
# American Community Survey 5-year estimates

# ....................................................
# Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)

# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable of interest - Table used:
##  - Mean HouseHold Income (by Tract) - S1902 "MEAN INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)"


# ....................................................
# Pull Cville and Alb Co at the same time (using map functions from purrr)
# https://walkerke.github.io/2017/05/tidycensus-every-tract/

cvl_alb <- c("003", "540") # list of desired counties

tract_meaninc <- map_df(cvl_alb, function(x) {
  get_acs(geography = "tract", 
          variables = c(meanincome ="S1902_C03_001"), 
          state = "VA", county = x, survey = "acs5")
})

# rename estimate, moe to something more desciptive
tract_meaninc <- tract_meaninc %>% 
  rename(avg_inc = estimate, avg_inc_moe = moe) %>% 
  select(-variable)


# ....................................................
# Read in hmda data and join to tract_income
hmda_cvl <- read_csv("hmda_lar_charlottesville_2007_2017.csv")
hmda_alb <- read_csv("hmda_lar_albemarle_2007_2017.csv")
# this may look different for you, depending on whether you've saved the 
# combined (Cville and Albemarle) original HMDA data somewhere

# Combine data and keep only: 
#   2013-2017 (as_of_year)
#   home purchase loans (loan_purpose_name)
#   originated (action_taken_name)
hmda <- bind_rows(list(hmda_cvl, hmda_alb))
hmda <- hmda %>% filter(as_of_year > 2012,
                        loan_purpose_name == "Home purchase",
                        action_taken_name == "Loan originated")

# Join to tract data
hmda <- hmda %>% 
  mutate(tract = gsub("[.]","", census_tract_number)) # create 6 digit tract var

tract_meaninc <- tract_meaninc %>% 
  mutate(tract = str_sub(GEOID, 6,11)) # create 6 digit tract var

hmda_acs <- full_join(tract_meaninc, hmda, by = "tract") # join


# ....................................................
# Scatterplot

# get income in dollars, recode race
hmda_acs <- hmda_acs %>% 
  mutate(appinc = applicant_income_000s*1000,
         race = fct_recode(applicant_race_name_1, 
                           white = "White", 
                           black = "Black or African American",
                           asian = "Asian",
                           unknown = "Information not provided by applicant in mail, Internet, or telephone application",
                           unknown = "Not applicable",
                           other = "American Indian or Alaska Native",
                           other = "Native Hawaiian or Other Pacific Islander"),
         race = fct_relevel(race, "white", "black", "asian", "other", "unknown")) %>% 
  filter(!is.na(race)) # remove single missing observation

# plot all applicant incomes by tract average income
ggplot(hmda_acs, aes(x = avg_inc, y = appinc)) + 
  geom_jitter(width = .25, alpha = 1/5) +  # jitter the points and make them semi-transparent
  geom_abline(intercept = 0, slope = 1) +  # add a reference line
  facet_wrap(~race) # facet by (make a separate panel for) each group


# Other things we could do with this comparison/figure:
# 1. Get the average applicant income by race and tract and graph those against average tract income
# 2. The average income from the census/acs data is adjusted to 2017 dollars; 
#   I wonder if we should adjust them to something like 2015 dollars (the midpoint of 2013-2017)?
#   in case a lot of the appearance of higher than the average is because incomes have risen?
#   Should be just a simple transformation of the avg_inc variable.

# I think something like this could be a nice supplement to the maps!
# The logit models of loan denials is a bit of a separate piece, but I think it contributes
#   to a broader story. In particular,

# The maps address questions like: where are what kinds of people buying/moving (based on race, income)
# Something like this idea in this script is along the lines of: how are these decisions affecting the neighborhoods (in terms of race and income)
# The loan denial model address the obstacles faced by black families (or families of color) in securing a place in a given neighborhood (making displacement easier)
# But I think each address a piece of the same puzzle.

hmda_acs2 <- hmda_acs %>% 
  filter(race %in% c("white", "black"),
         appinc < 3e+06) %>% 
  mutate(race = fct_relevel(race, "black", "white"))

# plot all applicant incomes by tract average income
ggplot(hmda_acs2, aes(x = avg_inc, y = appinc)) + 
  geom_jitter(width = .25, alpha = 1/5) +  # jitter the points and make them semi-transparent
  geom_abline(intercept = 0, slope = 1) +  # add a reference line
  facet_wrap(~race) + # facet by (make a separate panel for) each group
  labs(title = "Applicant Income and Average Tract Income by Race",
       subtitle = "Based on Purchases from 2013-2017 within Census Tracts",
       y = "Applicant Income", x = "Average Tract Income")



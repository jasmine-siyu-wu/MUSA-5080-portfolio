library(tidycensus) # a collecion of packages, include dplyr
library(tidyverse)


pa_income <- get_acs(geography = "county",
                     variables = "B19013_001", #total population
                     state = "PA",
                     year = 2023,
                     survey = "acs5")

dim(pa_income) #first number is row number and second is column
# [1] 67  5
# Pennsylvania has 67 counties - matched with my output
glimpse(pa_income)
head(pa_income)


pa_income$GEOID
as.numeric("01001")


filter(pa_income, estimate > 60000)
# 56 counties

# Counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)
# 15 counties

# Counties where the estimate is under 50000
filter(pa_income, estimate < 50000)
# just one county! Cameron


select(pa_income, NAME, estimate, moe)
# now we have still 67 rows and only 3 columns

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)


mutate(pa_income, moe_pct = moe / estimate * 100)
# now we have a new column called moe_pct
moe_pct
# object "moe_pct" not found
# WE NEED TO SAVE/ASSIGN THE COLUMN TO THE DATAFRAME
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
pa_income


arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct))
# Cameron County has the highest moe_pct


step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)
# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, moe_pct)



pa_income <- mutate(pa_income, reliable = moe_pct < 5)
pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))
# only two groups: because reliable has only two values: TRUE and FALSE



pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low confidence"
  ))
count(pa_income, reliability)
# reliability         n
# <chr>           <int>
# 1 High confidence    26
# 2 Low confidence      7
# 3 Moderate           34



pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA",
  year = 2023,
  survey = "acs5"
)
pa_two
# we read in two variables for each unit in a long format
# so we have 67*2 = 134 rows


pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA",
  year = 2023,
  survey = "acs5",
  output = "wide"
)
pa_wide
# E stands for estimate
# M stands for MOE



pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
pa_wide %>%
  arrange(desc(popE)) %>%
  select(NAME, popE, incomeE) %>%
  head(10)
# the top 10 counties with the highest moe pct (uncertainty)
# contain many less populated counties
# so the answer is yes to "is the margin of error bigger in small counties?"


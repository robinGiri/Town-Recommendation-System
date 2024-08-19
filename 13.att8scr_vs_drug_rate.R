# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the datasets
bristolCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
cornwallCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")
bristolSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/bristol-schools.csv")
cornwallSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/cornwall-schools.csv")

# Load and adjust population data from 2011 to 2023, then estimate for 2022
population_2011 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/Population2011_1656567141570.csv")
population_2023 <- population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Estimate population for 2022
years_between <- 2023 - 2011
annual_growth_rate <- 1.00561255390388033 ^ (1/years_between)
population_2022 <- population_2023 %>% 
  mutate(Population2022 = Population / annual_growth_rate)

# -----------------------------------------------------------------------
# Prepare the crime rate data for Bristol
bristolCrimeRate <- bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Filter for drug offenses in 2022 and calculate drug offense rate per 10,000 people
bs_drugOffenceRate_2022 <- bristolCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs", city == "Bristol, City of") %>% 
  filter(!is.na(Population)) %>%
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(), population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)

# -----------------------------------------------------------------------
# Prepare the crime rate data for Cornwall
cornwallCrimeRate <- cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# Filter for drug offenses in 2022 and calculate drug offense rate per 10,000 people
cw_drugOffenceRate_2022 <- cornwallCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs", city == "Cornwall") %>% 
  filter(!is.na(Population)) %>%
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(), population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)

# Prepare the school data for 2022 and ensure ATT8SCR is numeric
bristolSchools2022 <- bristolSchools %>%
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

cornwallSchools2022 <- cornwallSchools %>%
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Merge and prepare data for Bristol
bristolData <- inner_join(bristolSchools2022, bs_drugOffenceRate_2022, by = "Postcode") %>%
  select(ATT8SCR, DrugOffenseRate)

# Merge and prepare data for Cornwall
cornwallData <- inner_join(cornwallSchools2022, cw_drugOffenceRate_2022, by = "Postcode") %>%
  select(ATT8SCR, DrugOffenseRate)

# Create and summarize the linear model for Bristol
bristolModel <- lm(ATT8SCR ~ DrugOffenseRate, data = bristolData)
summary(bristolModel)

# Create and summarize the linear model for Cornwall
cornwallModel <- lm(ATT8SCR ~ DrugOffenseRate, data = cornwallData)
summary(cornwallModel)

# Plot the linear model with the line of best fit for Bristol
ggplot(bristolData, aes(x = DrugOffenseRate, y = ATT8SCR)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Model: Attainment 8 Score vs Drug Offense Rate in Bristol (2022)",
    x = "Drug Offense Rate per 10,000 people",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

# Plot the linear model with the line of best fit for Cornwall
ggplot(cornwallData, aes(x = DrugOffenseRate, y = ATT8SCR)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Linear Model: Attainment 8 Score vs Drug Offense Rate in Cornwall (2022)",
    x = "Drug Offense Rate per 10,000 people",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

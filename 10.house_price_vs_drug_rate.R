library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------
# Load the cleaned datasets for house pricing and crime rates in Bristol and Cornwall
bristolHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
cornwallHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")

bristolCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
cornwallCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")

# -----------------------------------------------------------------------
# Filter the house price data for transactions in 2022
bristolHousePrice2022 <- bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)
cornwallHousePrice2022 <- cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022)

# -----------------------------------------------------------------------
# Load and prepare the population data
population_2011 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/Population2011_1656567141570.csv")

# Convert the population of 2011 to the population of 2023 using a growth rate
population_2023 <- population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

# Convert the population of 2023 to the population of 2022
years_between <- 2023 - 2011
annual_growth_rate <- 1.00561255390388033 ^ (1 / years_between)
population_2022 <- population_2023 %>% 
  mutate(Population = Population / annual_growth_rate)

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

# -----------------------------------------------------------------------
# LINEAR MODELLING: House Prices vs Drug Offense Rate in Bristol (2022)
# Merge the datasets for Bristol house prices and drug offense rates
bs_housePrice_drugRate_2022 <- inner_join(
  bristolHousePrice2022, bs_drugOffenceRate_2022,
  by = c("Postcode" = "postcode_space")
) %>% 
  select(Price, drug_offense_rate)

# Calculate correlation and build the linear model
# Weak negative relationship (-0.0260)
bs_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Price, drug_offense_rate))

bsModel_housePrice_drugRate <- lm(Price ~ drug_offense_rate, data = bs_housePrice_drugRate_2022)
summary(bsModel_housePrice_drugRate)

# Extract intercept and slope for plotting
bsIntercept_housePrice_drugRate <- coef(bsModel_housePrice_drugRate)[1]
bsSlope_housePrice_drugRate <- coef(bsModel_housePrice_drugRate)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Bristol
ggplot(bs_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_housePrice_drugRate, slope = bsSlope_housePrice_drugRate, color = "red") +      
  labs(
    title = "Impact of Drug Offence on House Price in Bristol (2022)",
    x = "Drug Offence Rate (per 10,000 people)",
    y = "House Price (£)"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------
# LINEAR MODELLING: House Prices vs Drug Offense Rate in Cornwall (2022)
# Merge the datasets for Cornwall house prices and drug offense rates
cw_housePrice_drugRate_2022 <- inner_join(
  cornwallHousePrice2022, cw_drugOffenceRate_2022,
  by = c("Postcode" = "postcode_space")
) %>% 
  select(Price, drug_offense_rate)

# Calculate correlation and build the linear model
# Weak negative relationship (-0.0483)
cw_housePrice_drugRate_2022 %>% 
  summarise(corCoeff = cor(Price, drug_offense_rate))

cwModel_housePrice_drugRate <- lm(Price ~ drug_offense_rate, data = cw_housePrice_drugRate_2022)
summary(cwModel_housePrice_drugRate)

# Extract intercept and slope for plotting
cwIntercept_housePrice_drugRate <- coef(cwModel_housePrice_drugRate)[1]
cwSlope_housePrice_drugRate <- coef(cwModel_housePrice_drugRate)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Cornwall
ggplot(cw_housePrice_drugRate_2022, aes(x = drug_offense_rate, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_housePrice_drugRate, slope = cwSlope_housePrice_drugRate, color = "red") +      
  labs(
    title = "Impact of Drug Offence on House Price in Cornwall (2022)",
    x = "Drug Offence Rate (per 10,000 people)",
    y = "House Price (£)"
  ) +
  theme_minimal()

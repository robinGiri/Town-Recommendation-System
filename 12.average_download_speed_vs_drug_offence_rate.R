library(tidyverse)

# -----------------------------------------------------------------------
# Load the cleaned datasets for broadband speeds and crime rates in Bristol and Cornwall
bristolBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
cornwallBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")

bristolCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
cornwallCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")

# -----------------------------------------------------------------------
# Load and adjust population data from 2011 to 2023, then convert to 2022 estimates
population_2011 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/Population2011_1656567141570.csv")

population_2023 <- population_2011 %>% 
  mutate(Population = Population * 1.00561255390388033)

years_between <- 2023 - 2011
annual_growth_rate <- 1.00561255390388033 ^ (1/years_between)

population_2022 <- population_2023 %>% 
  mutate(Population = Population / annual_growth_rate)

# -----------------------------------------------------------------------
# Prepare the crime rate datasets for Bristol and Cornwall by extracting postcode and adjusting date format
bristolCrimeRate <- bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

cornwallCrimeRate <- cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>% 
  mutate(Year = ymd(paste0(Year, "-01")))

# -----------------------------------------------------------------------
# Calculate the drug offense rate per 10,000 people for Bristol in 2022
bs_drugOffenceRate_2022 <- bristolCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Bristol, City of") %>%
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(), population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)

# -----------------------------------------------------------------------
# Calculate the drug offense rate per 10,000 people for Cornwall in 2022
cw_drugOffenceRate_2022 <- cornwallCrimeRate %>% 
  filter(year(Year) == 2022) %>% 
  left_join(population_2022, by = "Postcode") %>% 
  filter(`Crime type` == "Drugs") %>% 
  filter(city == "Cornwall") %>%
  filter(!is.na(Population)) %>% 
  group_by(postcode_space) %>%
  summarise(drug_offenses = n(), population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)

# -----------------------------------------------------------------------
# LINEAR MODELLING: Average Download Speed vs Drug Offense Rate in Bristol (2022)
bs_drugRate_downloadSpeed_2022 <- inner_join(
  bristolBroadbandSpeed, bs_drugOffenceRate_2022,
  by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)

# Calculate correlation and build the linear model for Bristol
bs_drugRate_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

bsModel_drugRate_downloadSpeed <- lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = bs_drugRate_downloadSpeed_2022)
summary(bsModel_drugRate_downloadSpeed)

# Extract intercept and slope for the line of best fit
bsIntercept_drugRate_downloadSpeed <- coef(bsModel_drugRate_downloadSpeed)[1]
bsSlope_drugRate_downloadSpeed <- coef(bsModel_drugRate_downloadSpeed)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Bristol
ggplot(bs_drugRate_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = drug_offense_rate)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_drugRate_downloadSpeed, slope = bsSlope_drugRate_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Internet Speed on Drug Offense Rates in Bristol (2022)",
    x = "Average Download Speed",
    y = "Drug Offense Rate"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------
# LINEAR MODELLING: Average Download Speed vs Drug Offense Rate in Cornwall (2022)
cw_drugRate_downloadSpeed_2022 <- inner_join(
  cornwallBroadbandSpeed, cw_drugOffenceRate_2022,
  by = "postcode_space") %>% 
  select(`Average download speed (Mbit/s)`, drug_offense_rate)

# Calculate correlation and build the linear model for Cornwall
cw_drugRate_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(drug_offense_rate, `Average download speed (Mbit/s)`))

cwModel_drugRate_downloadSpeed <- lm(drug_offense_rate ~ `Average download speed (Mbit/s)`, data = cw_drugRate_downloadSpeed_2022)
summary(cwModel_drugRate_downloadSpeed)

# Extract intercept and slope for the line of best fit
cwIntercept_drugRate_downloadSpeed <- coef(cwModel_drugRate_downloadSpeed)[1]
cwSlope_drugRate_downloadSpeed <- coef(cwModel_drugRate_downloadSpeed)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Cornwall
ggplot(cw_drugRate_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = drug_offense_rate)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_drugRate_downloadSpeed, slope = cwSlope_drugRate_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Internet Speed on Drug Offense Rates in Cornwall (2022)",
    x = "Average Download Speed",
    y = "Drug Offense Rate"
  ) +
  theme_minimal()

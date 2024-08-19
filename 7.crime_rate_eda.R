library(tidyverse)
library(plotly)
library(lubridate)
library(fmsb)

# Load cleaned crime data for Bristol and Cornwall
bristolCrimeSummary <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-summary.csv")
cornwallCrimeSummary <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-summary.csv")
bristolCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
cornwallCrimeRate <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")

# Load population data and estimate the population for 2022
population_2011 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/Population2011_1656567141570.csv")

# Calculate the population for 2023 based on annual growth rate and then adjust it for 2022
years_between <- 2023 - 2011
annual_growth_rate <- 1.00561255390388033 ^ (1/years_between)
population_2022 <- population_2011 %>%
  mutate(Population = Population * 1.00561255390388033 / annual_growth_rate)

# Prepare the crime rate data by adding Postcode and Year columns
bristolCrimeRate <- bristolCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>%
  mutate(Year = ymd(paste0(Year, "-01")))

cornwallCrimeRate <- cornwallCrimeRate %>%
  mutate(Postcode = str_extract(postcode_space, "^\\S+ \\d")) %>%
  mutate(Year = ymd(paste0(Year, "-01")))

# Combine crime rate data for Bristol and Cornwall
bcCrimeRate <- bind_rows(bristolCrimeRate, cornwallCrimeRate)

# -----------------------------------------------------------------------
# BOX PLOT: Drug Offense Rate in Both Counties in 2022
# Filter the data for drug offenses in 2022
# Merge with population data for accurate rate calculation
# Calculate the drug offense rate per 10,000 people
drugOffenceRate <- bcCrimeRate %>%
  filter(year(Year) == 2022) %>%
  left_join(population_2022, by = "Postcode") %>%
  filter(`Crime type` == "Drugs") %>%
  filter(city == "Bristol, City of" | city == "Cornwall") %>%
  filter(!is.na(Population)) %>%
  group_by(Postcode, city) %>%
  summarise(drug_offenses = n(), population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000)

# Create the box plot for drug offense rates
drugOffenceRateBoxPlot <- drugOffenceRate %>%
  ggplot(aes(x = city, y = drug_offense_rate, fill = city)) +
  geom_boxplot() +
  labs(title = "Drug Offense Rate in Bristol and Cornwall (2022)",
       x = "County",
       y = "Drug Offense Rate per 10,000 people") +
  coord_cartesian(ylim = c(0, 50)) +
  theme_minimal() +
  scale_x_discrete(labels = c("Bristol, City of" = "Bristol", "Cornwall" = "Cornwall")) +
  theme(plot.title = element_text(size = 12))

# Display the box plot
ggplotly(drugOffenceRateBoxPlot)

# ------------------------------------------------------------------------------------
# RADAR CHART: Vehicle Crime Rate per 10,000 People in Bristol (Specific Month 2022)
# Filter the data for vehicle crimes in November 2022 in Bristol
# Merge with population data and calculate the crime rate
bsVehicleCrimeRate <- bristolCrimeRate %>%
  filter(year(Year) == 2022 & month(Year) == 11) %>%
  left_join(population_2022, by = "Postcode") %>%
  filter(`Crime type` == "Vehicle crime") %>%
  filter(city == "Bristol, City of") %>%
  filter(!is.na(Population)) %>%
  group_by(`LSOA name`) %>%
  summarise(total_vehicleCrimes = n(), 
            population = first(Population),
            vehicleCrimeRate_per_10000 = (total_vehicleCrimes / population) * 10000) %>%
  ungroup()

# Create the radar chart data by selecting 20 random LSOAs and preparing for radar chart
bsSampled_lsoas <- bsVehicleCrimeRate %>%
  sample_n(20)

bsRadar_data <- bsSampled_lsoas %>%
  pivot_wider(names_from = `LSOA name`, values_from = vehicleCrimeRate_per_10000, values_fill = list(vehicleCrimeRate_per_10000 = 0)) %>%
  as.data.frame()

bsMax_values <- rep(max(bsRadar_data[-1], na.rm = TRUE), ncol(bsRadar_data))
bsRadar_data <- rbind(rep(0, ncol(bsRadar_data)), bsMax_values, bsRadar_data)

# Set up the radar chart with Min and Max values for scaling
colnames(bsRadar_data) <- c("Min", "Max", paste("Area", 1:(ncol(bsRadar_data) - 2), sep = " "))

# Plot the radar chart
radarchart(bsRadar_data, 
           axistype = 1, 
           pcol = c("blue", "red"), 
           pfcol = c("blue", "red"), 
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           vlcex = 0.8, 
           title = "Vehicle Crime Rate per 10,000 People in Bristol (Nov 2022)",
           cex.main = 1)

# --------------------------------------------------------------------------------
# PIE CHART: Robbery Crime Rate per 10,000 People in Bristol and Cornwall (Specific Month 2022)
# Filter the data for robbery crimes in November 2022 in Bristol and Cornwall
# Merge with population data and calculate the crime rate
robberyCrimeRate <- bcCrimeRate %>%
  filter(year(Year) == 2022 & month(Year) == 11) %>%
  left_join(population_2022, by = "Postcode") %>%
  filter(`Crime type` == "Robbery") %>%
  filter(city == "Bristol, City of" | city == "Cornwall") %>%
  filter(!is.na(Population)) %>%
  group_by(city) %>%
  summarise(total_robberyCrimes = n(), 
            population = first(Population),
            robberyCrimeRate_per_10000 = (total_robberyCrimes / population) * 10000) %>%
  ungroup() %>%
  mutate(percentage = total_robberyCrimes / sum(total_robberyCrimes) * 100,
         label = paste0(round(percentage, 1), "%"))

# Create the pie chart for robbery crime rates
robberyCrimeRate %>%
  ggplot(aes(x = "", y = robberyCrimeRate_per_10000, fill = city)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Robbery Crime Rate per 10,000 People in Bristol and Cornwall (Nov 2022)",
       fill = "County") +
  scale_fill_discrete(labels = c("Bristol, City of" = "Bristol", "Cornwall" = "Cornwall")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(size = 12))

# ---------------------------------------------------------------------------------
# LINE CHART: Drug Offense Rate per 10,000 People in Bristol and Cornwall (2022)
# Filter the data for drug offenses in 2022
# Merge with population data and calculate the crime rate
drugOffenceRate %>%
  mutate(Year = year(Year)) %>%  # Ensure the Year column is numeric and available
  group_by(Year, city) %>%
  summarise(drug_offenses = n(),
            population = first(Population)) %>%
  mutate(drug_offense_rate = drug_offenses / population * 10000) %>%
  ggplot(aes(x = Year, y = drug_offense_rate, color = city, group = city)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Drug Offense Rate per 10,000 People in Bristol and Cornwall (2022)",
       x = "Year",
       y = "Drug Offense Rate per 10,000 People",
       color = "County") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))

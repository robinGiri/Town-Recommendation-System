library(tidyverse)
library(lubridate)

# Load and clean house pricing data for 2020
housePricing20 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/House_Pricing/pp-2020.csv", col_names = FALSE)
names(housePricing20) <- c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                           "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                           "District", "County", "PPD_type", "Record_status")
housePricing20 <- housePricing20 %>%
  mutate(Date_of_transfer = ymd(Date_of_transfer),
         Postcode = replace_na(Postcode, "Unknown"),
         SAON = replace_na(SAON, "Unknown"),
         Street = replace_na(Street, "Unknown"),
         Locality = replace_na(Locality, "Unknown")) %>%
  distinct()

# Load and clean house pricing data for 2021
housePricing21 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/House_Pricing/pp-2021.csv", col_names = FALSE)
names(housePricing21) <- c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                           "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                           "District", "County", "PPD_type", "Record_status")
housePricing21 <- housePricing21 %>%
  mutate(Date_of_transfer = ymd(Date_of_transfer),
         Postcode = replace_na(Postcode, "Unknown"),
         SAON = replace_na(SAON, "Unknown"),
         Street = replace_na(Street, "Unknown"),
         Locality = replace_na(Locality, "Unknown")) %>%
  distinct()

# Load and clean house pricing data for 2022
housePricing22 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/House_Pricing/pp-2022.csv", col_names = FALSE)
names(housePricing22) <- c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                           "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                           "District", "County", "PPD_type", "Record_status")
housePricing22 <- housePricing22 %>%
  mutate(Date_of_transfer = ymd(Date_of_transfer),
         Postcode = replace_na(Postcode, "Unknown"),
         SAON = replace_na(SAON, "Not Applicable"),
         Street = replace_na(Street, "Unknown"),
         Locality = replace_na(Locality, "Not Applicable")) %>%
  distinct()

# Load and clean house pricing data for 2023
housePricing23 <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/House_Pricing/pp-2023.csv", col_names = FALSE)
names(housePricing23) <- c("Txn_ID", "Price", "Date_of_transfer", "Postcode", "Property_type", 
                           "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", 
                           "District", "County", "PPD_type", "Record_status")
housePricing23 <- housePricing23 %>%
  mutate(Date_of_transfer = ymd(Date_of_transfer),
         Postcode = replace_na(Postcode, "Unknown"),
         SAON = replace_na(SAON, "Not Applicable"),
         Street = replace_na(Street, "Unknown"),
         Locality = replace_na(Locality, "Not Applicable")) %>%
  distinct()

# Combine data for each county across all years
bristolHousePriceCleaned <- bind_rows(
  housePricing20 %>% filter(County == "CITY OF BRISTOL"),
  housePricing21 %>% filter(County == "CITY OF BRISTOL"),
  housePricing22 %>% filter(County == "CITY OF BRISTOL"),
  housePricing23 %>% filter(County == "CITY OF BRISTOL")
)

cornwallHousePriceCleaned <- bind_rows(
  housePricing20 %>% filter(County == "CORNWALL"),
  housePricing21 %>% filter(County == "CORNWALL"),
  housePricing22 %>% filter(County == "CORNWALL"),
  housePricing23 %>% filter(County == "CORNWALL")
)

bcHousePriceCleaned <- bind_rows(bristolHousePriceCleaned, cornwallHousePriceCleaned)

# Save the cleaned data to CSV files
write.csv(bristolHousePriceCleaned, "/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv", row.names = FALSE)
write.csv(cornwallHousePriceCleaned, "/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv", row.names = FALSE)
write.csv(bcHousePriceCleaned, "/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-cornwall-house-pricing.csv", row.names = FALSE)

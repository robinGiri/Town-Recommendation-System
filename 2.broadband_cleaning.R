# Load necessary libraries
library(tidyverse)
library(readr)
library(dplyr)

# Load the broadband speed performance dataset
broadbandSpeedPerformance <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/broadband/201805_fixed_pc_performance_r03.csv")

# Check the summary statistics and count missing values in the dataset
summary(broadbandSpeedPerformance)
colSums(is.na(broadbandSpeedPerformance))

# Handle missing values in numeric columns
broadbandSpeedPerformance <- broadbandSpeedPerformance %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), 
                         ifelse(sum(is.na(.)) > 0.9 * n(), 0, median(., na.rm = TRUE)), 
                         .)))

# Recheck for missing values
colSums(is.na(broadbandSpeedPerformance))

# Ensure no duplicate rows exist in the dataset
sum(duplicated(broadbandSpeedPerformance))

# Histogram for Average Data Usage before and after log transformation
ggplot(broadbandSpeedPerformance, aes(x = `Average data usage (GB)`)) +
  geom_histogram() +
  ggtitle("Original Average Data Usage Distribution")

# Apply log transformation to normalize highly skewed data
broadbandSpeedPerformance <- broadbandSpeedPerformance %>%
  mutate(
    `Average data usage (GB)` = log(`Average data usage (GB)` + 1),
    `Median download speed (Mbit/s)` = log(`Median download speed (Mbit/s)` + 1),
    `Average download speed (Mbit/s)` = log(`Average download speed (Mbit/s)` + 1),
    `Minimum download speed (Mbit/s)` = log(`Minimum download speed (Mbit/s)` + 1),
    `Maximum download speed (Mbit/s)` = log(`Maximum download speed (Mbit/s)` + 1),
    `Median upload speed (Mbit/s)` = log(`Median upload speed (Mbit/s)` + 1),
    `Average upload speed (Mbit/s)` = log(`Average upload speed (Mbit/s)` + 1),
    `Minimum upload speed (Mbit/s)` = log(`Minimum upload speed (Mbit/s)` + 1),
    `Maximum upload speed (Mbit/s)` = log(`Maximum upload speed (Mbit/s)` + 1),
    `Number of connections 5<10 Mbit/s (number of lines)` = log(`Number of connections 5<10 Mbit/s (number of lines)` + 1),
    `Number of connections 10<30 Mbit/s (number of lines)` = log(`Number of connections 10<30 Mbit/s (number of lines)` + 1),
    `Number of connections >= 30 Mbit/s (number of lines)` = log(`Number of connections >= 30 Mbit/s (number of lines)` + 1),
    `Average data usage (GB) for lines < 10Mbit/s` = log(`Average data usage (GB) for lines < 10Mbit/s` + 1),
    `Average data usage (GB) for Basic BB lines` = log(`Average data usage (GB) for Basic BB lines` + 1),
    `Average data usage (GB) for SFBB lines` = log(`Average data usage (GB) for SFBB lines` + 1)
  )

# Histogram after log transformation
ggplot(broadbandSpeedPerformance, aes(x = `Average data usage (GB)`)) +
  geom_histogram() +
  ggtitle("Transformed Average Data Usage Distribution")

# Filter data for Bristol and Cornwall regions based on postcode area
bsBroadbandSpeedPerf <- broadbandSpeedPerformance %>% filter(`postcode area` == "BS")
cwBroadbandSpeedPerf <- broadbandSpeedPerformance %>% filter(`postcode area` %in% c("TR", "PL"))
bcBroadbandSpeedPerf <- broadbandSpeedPerformance %>% filter(`postcode area` %in% c("BS", "TR", "PL"))

# Load broadband speed coverage dataset
broadbandSpeedCoverage <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/broadband/201809_fixed_pc_coverage_r01.csv")

# Rename columns to match the performance dataset for easier merging
broadbandSpeedCoverage <- broadbandSpeedCoverage %>% 
  rename(postcode_space = pcds, `postcode area` = pca, `Connected Premises` = `All Matched Premises`)

# Ensure no duplicate rows exist in the dataset
sum(duplicated(broadbandSpeedCoverage))

# Filter data for Bristol and Cornwall regions based on postcode area
bsBroadbandSpeedCov <- broadbandSpeedCoverage %>% filter(`postcode area` == "BS")
cwBroadbandSpeedCov <- broadbandSpeedCoverage %>% filter(`postcode area` %in% c("TR", "PL"))
bcBroadbandSpeedCov <- broadbandSpeedCoverage %>% filter(`postcode area` %in% c("BS", "TR", "PL"))

# Merge performance and coverage data for Bristol, Cornwall, and both regions
bristolBroadbandSpeedCleaned <- inner_join(bsBroadbandSpeedPerf, bsBroadbandSpeedCov, by = c("postcode", "postcode_space", "postcode area"))
cornwallBroadbandSpeedCleaned <- inner_join(cwBroadbandSpeedPerf, cwBroadbandSpeedCov, by = c("postcode", "postcode_space", "postcode area"))
bcBroadbandSpeedCleaned <- inner_join(bcBroadbandSpeedPerf, bcBroadbandSpeedCov, by = c("postcode", "postcode_space", "postcode area"))

# Load postcode to LSOA code mapping data
pscdToLsoa <- read_csv("/Users/bijayagiri/Downloads/raw_source_data/Postcode to LSOA.csv")

# Rename columns for merging
pscdToLsoa <- pscdToLsoa %>% 
  rename(postcode_space = pcds, lsoa_area = lsoa11nm, city = ladnm) %>% 
  select(postcode_space, lsoa_area, city)

# Add LSOA area and city names to cleaned data sets
bristolBroadbandSpeedCleaned <- bristolBroadbandSpeedCleaned %>% left_join(pscdToLsoa, by = "postcode_space")
cornwallBroadbandSpeedCleaned <- cornwallBroadbandSpeedCleaned %>% left_join(pscdToLsoa, by = "postcode_space")
bcBroadbandSpeedCleaned <- bcBroadbandSpeedCleaned %>% left_join(pscdToLsoa, by = "postcode_space")

# Save the cleaned datasets to CSV files
write.csv(bristolBroadbandSpeedCleaned, "/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv", row.names = FALSE)
write.csv(cornwallBroadbandSpeedCleaned, "/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv", row.names = FALSE)
write.csv(bcBroadbandSpeedCleaned, "/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-cornwall-broadband-speed.csv", row.names = FALSE)

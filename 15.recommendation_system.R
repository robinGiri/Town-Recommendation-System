# Load necessary libraries
library(dplyr)   
library(tidyr)   
library(scales)  
library(purrr)   

# Read in the datasets
house_data_bristol = read.csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
house_data_cornwall = read.csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")
broadband_bristol = read.csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
broadband_cornwall = read.csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")
crime_bristol = read.csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
crime_cornwall = read.csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")
schools_bristol = read.csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/bristol-schools.csv")
schools_cornwall = read.csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/cornwall-schools.csv")

# Rename columns to ensure consistency across datasets
crime_bristol = crime_bristol %>%
  rename(Postcode = postcode_space, Town = city)
crime_cornwall = crime_cornwall %>%
  rename(Postcode = postcode_space, Town = city)
house_data_bristol = house_data_bristol %>%
  rename(Postcode = Postcode, Town = Town.City)
house_data_cornwall = house_data_cornwall %>%
  rename(Postcode = Postcode, Town = Town.City)
broadband_bristol = broadband_bristol %>%
  rename(Postcode = postcode, Town = city)
broadband_cornwall = broadband_cornwall %>%
  rename(Postcode = postcode, Town = city)
schools_bristol = schools_bristol %>%
  rename(Postcode = POSTCODE, Town = TOWN)
schools_cornwall = schools_cornwall %>%
  rename(Postcode = POSTCODE, Town = TOWN)

# Combine Bristol datasets
combined_bristol = reduce(list(house_data_bristol, broadband_bristol, crime_bristol, schools_bristol), full_join, by = c("Town", "Postcode"))

# Combine Cornwall datasets
combined_cornwall = reduce(list(house_data_cornwall, broadband_cornwall, crime_cornwall, schools_cornwall), full_join, by = c("Town", "Postcode"))

# Combine Bristol and Cornwall data
combined_data = bind_rows(combined_bristol, combined_cornwall)

# Function to calculate a final score based on crime rate, school quality, and house prices
calculate_final_score = function(data) {
  data %>%
    mutate(
      # Replace NA values with median or zero as needed for scoring
      Price = ifelse(is.na(Price), median(data$Price, na.rm = TRUE), Price),
      crime_rate = ifelse(is.na(Crime.ID), 0, 1), 
      
      # Handle non-numeric values in OFSTEDRATING by converting them to NA first
      OFSTEDRATING = suppressWarnings(as.numeric(OFSTEDRATING)),
      school_quality = ifelse(is.na(OFSTEDRATING), 0, OFSTEDRATING),
      
      # Calculate the scores
      crime_rate_score = 10 * (1 - rescale(crime_rate, to = c(0, 1))),
      school_quality_score = 10 * rescale(school_quality, to = c(0, 1)),
      house_price_score = 10 * (1 - rescale(Price, to = c(0, 1))),
      
      # Calculate the final score with weighted components
      final_score = 0.4 * crime_rate_score + 0.3 * school_quality_score + 0.3 * house_price_score
    )
}

# Apply the function to the combined data
combined_data_with_scores = calculate_final_score(combined_data)

# Filter top recommendations based on the highest final score
top_recommendations = combined_data_with_scores %>%
  arrange(desc(final_score)) %>%
  select(Txn_ID, Postcode, PAON, Street, Town, Locality, County, final_score,) %>%
  distinct(Town, .keep_all = TRUE) %>%
  head(10)

# Save the combined data with scores and top recommendations to CSV files
write.csv(combined_data_with_scores, "/Users/bijayagiri/Downloads/clean_source_data/recommendations/combined_data_with_scores.csv", row.names = FALSE)
write.csv(top_recommendations, "/Users/bijayagiri/Downloads/clean_source_data/recommendations/top_recommendations.csv", row.names = FALSE)

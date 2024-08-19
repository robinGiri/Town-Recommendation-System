# Read in the datasets
# The datasets are read from specified file paths. Each dataset contains information related to different aspects such as housing, broadband, crime, and schools.
house_data_bristol <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
house_data_cornwall <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")
broadband_bristol <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
broadband_cornwall <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")
crime_bristol <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv")
crime_cornwall <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv")
schools_bristol <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/bristol-schools.csv")
schools_cornwall <- read.csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/cornwall-schools.csv")

# Rename columns to ensure consistency across datasets
# This step standardizes column names to facilitate merging and analysis. 
# Renaming is necessary as different datasets use varying column names for similar information.
crime_bristol <- crime_bristol %>%
  rename(Postcode = postcode_space, Town = city)

crime_cornwall <- crime_cornwall %>%
  rename(Postcode = postcode_space, Town = city)

house_data_bristol <- house_data_bristol %>%
  rename(Postcode = Postcode, Town = Town.City)

house_data_cornwall <- house_data_cornwall %>%
  rename(Postcode = Postcode, Town = Town.City)

broadband_bristol <- broadband_bristol %>%
  rename(Postcode = postcode, Town = city)

broadband_cornwall <- broadband_cornwall %>%
  rename(Postcode = postcode, Town = city)

schools_bristol <- schools_bristol %>%
  rename(Postcode = POSTCODE, Town = TOWN)

schools_cornwall <- schools_cornwall %>%
  rename(Postcode = POSTCODE, Town = TOWN)

# Combine Bristol datasets
# Merges multiple datasets for Bristol into a single data frame using a full join on 'Town' and 'Postcode'. 
# This ensures that all records from each dataset are included, even if there are missing values in some datasets.
combined_bristol <- reduce(list(house_data_bristol, broadband_bristol, crime_bristol, schools_bristol), full_join, by = c("Town", "Postcode"))

# Combine Cornwall datasets
# Similarly, merges multiple datasets for Cornwall into a single data frame.
combined_cornwall <- reduce(list(house_data_cornwall, broadband_cornwall, crime_cornwall, schools_cornwall), full_join, by = c("Town", "Postcode"))

# Combine Bristol and Cornwall data
# Combines the datasets from both regions into a single data frame for overall analysis.
combined_data <- bind_rows(combined_bristol, combined_cornwall)

# Define a function to calculate a final score based on crime rate, school quality, and house prices
# This function calculates scores for each record based on crime rate, school quality, and house prices, 
# providing a weighted final score for each location.
calculate_final_score <- function(data) {
  data %>%
    mutate(
      # Replace NA values with median or zero as needed for scoring
      Price = ifelse(is.na(Price), median(data$Price, na.rm = TRUE), Price),
      crime_rate = ifelse(is.na(Crime.ID), 0, 1),  # Example logic, adjust based on actual data
      school_quality = ifelse(is.na(OFSTEDRATING), 0, as.numeric(OFSTEDRATING)),
      crime_rate_score = 10 * (1 - rescale(crime_rate, to = c(0, 1))),
      school_quality_score = 10 * rescale(school_quality, to = c(0, 1)),
      house_price_score = 10 * (1 - rescale(Price, to = c(0, 1))),
      final_score = 0.4 * crime_rate_score + 0.3 * school_quality_score + 0.3 * house_price_score
    )
}

# Apply the function to the combined data
# Applies the scoring function to the combined data, adding new columns with calculated scores.
combined_data_with_scores <- calculate_final_score(combined_data)

# Filter top recommendations based on the highest final score
# Sorts the data by final score in descending order and selects the top 10 recommendations.
top_recommendations <- combined_data_with_scores %>%
  arrange(desc(final_score)) %>%
  select(Postcode, Town, final_score) %>%
  head(10)  # Adjust the number of recommendations as needed

# Save the combined data with scores to a CSV file
# Exports the data with calculated scores to a CSV file for further analysis or reporting.
write.csv(combined_data_with_scores, "/Users/bijayagiri/Downloads/clean_source_data/combined_data_with_scores.csv", row.names = FALSE)

# Save the top recommendations to a CSV file
# Exports the top recommendations to a separate CSV file.
write.csv(top_recommendations, "/Users/bijayagiri/Downloads/clean_source_data/top_recommendations.csv", row.names = FALSE)

# Print top recommendations
# Outputs the top recommendations to the console.
print("Top Recommendations for Bristol and Cornwall:")
print(top_recommendations)


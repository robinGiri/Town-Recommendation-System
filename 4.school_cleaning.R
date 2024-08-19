library(tidyverse)

# Function to clean school data for a given year and region
clean_school_data <- function(ks4_filepath, info_filepath, year) {
  # Load the data
  ks4_data <- read_csv(ks4_filepath) %>%
    mutate(Year = as.character(year)) %>%
    select(PCODE, Year, SCHNAME, ATT8SCR) %>%
    drop_na(SCHNAME, ATT8SCR) %>%
    rename(POSTCODE = PCODE)

  info_data <- read_csv(info_filepath) %>%
    mutate(SCHLEVEL = case_when(
      ISPRIMARY == 1 ~ 'Primary',
      ISSECONDARY == 1 ~ 'Secondary',
      ISPOST16 == 1 ~ 'Post-16')) %>%
    select(URN, SCHNAME, POSTCODE, TOWN, SCHOOLTYPE, SCHLEVEL, OFSTEDRATING)

  # Merge the datasets
  school_data <- inner_join(ks4_data, info_data, by = c("POSTCODE", "SCHNAME"))

  # Handle missing TOWN values
  school_data <- school_data %>%
    mutate(TOWN = replace_na(TOWN, "Bristol"))

  return(school_data)
}

# Clean and merge the data for Bristol for 2019, 2022, and 2023
bristol_2019 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2018-2019/801_ks4final.csv",
                                  "/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2018-2019/801_school_information.csv", 2019)

bristol_2022 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2021-2022/801_ks4final.csv",
                                  "/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2021-2022/801_school_information.csv", 2022)

bristol_2023 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2022-2023/801_ks4final.csv",
                                  "/Users/bijayagiri/Downloads/raw_source_data/schools/bristol/2022-2023/801_school_information.csv", 2023)

# Combine all Bristol data
bristol_schools <- bind_rows(bristol_2019, bristol_2022, bristol_2023)

# Calculate and fill missing OFSTEDRATING with mode
bristol_mode_ofsted <- bristol_schools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

bristol_schools <- bristol_schools %>%
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), bristol_mode_ofsted, OFSTEDRATING),
         COUNTY = "City of Bristol")

# Clean and merge the data for Cornwall for 2019, 2022, and 2023
cornwall_2019 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2018-2019/908_ks4final.csv",
                                   "/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2018-2019/908_school_information.csv", 2019)

cornwall_2022 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2021-2022/908_ks4final.csv",
                                   "/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2021-2022/908_school_information.csv", 2022)

cornwall_2023 <- clean_school_data("/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2022-2023/908_ks4final.csv",
                                   "/Users/bijayagiri/Downloads/raw_source_data/schools/cornwall/2022-2023/908_school_information.csv", 2023)

# Combine all Cornwall data
cornwall_schools <- bind_rows(cornwall_2019, cornwall_2022, cornwall_2023)

# Calculate and fill missing OFSTEDRATING with mode
cornwall_mode_ofsted <- cornwall_schools %>%
  filter(!is.na(OFSTEDRATING)) %>%
  count(OFSTEDRATING) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(OFSTEDRATING)

cornwall_schools <- cornwall_schools %>%
  mutate(OFSTEDRATING = if_else(is.na(OFSTEDRATING), cornwall_mode_ofsted, OFSTEDRATING),
         COUNTY = "Cornwall")

# Save the cleaned data
write.csv(bristol_schools, "/Users/bijayagiri/Downloads/clean_source_data/Schools/bristol-schools.csv", row.names = FALSE)
write.csv(cornwall_schools, "/Users/bijayagiri/Downloads/clean_source_data/Schools/cornwall-schools.csv", row.names = FALSE)

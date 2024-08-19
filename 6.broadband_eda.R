library(tidyverse)
library(plotly)

# Load cleaned broadband data for Bristol and Cornwall
bristolBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
cornwallBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")
bcBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-cornwall-broadband-speed.csv")

options(scipen = 1000)

# ---------------------------------------------------------------------------------
# Create a boxplot to compare Average Download Speeds across both Bristol and Cornwall
# Filter the data for both counties
# Create a boxplot with county names on the x-axis and average download speeds on the y-axis
# Title and labels are added for clarity
bcBroadbandSpeed %>%
  ggplot(aes(x = city, y = `Average download speed (Mbit/s)`, fill = city)) +
  geom_boxplot() +
  labs(
    title = "Average Download Speeds in Bristol and Cornwall",
    x = "County",
    y = "Average Download Speed (Mbit/s)",
    fill = "County"
  ) +
  scale_fill_manual(values = c("Bristol, City of" = "steelblue", "Cornwall" = "forestgreen")) +
  theme_minimal()

# ---------------------------------------------------------------------------------
# Bar Chart 1: Average and Maximum Download Speeds in Bristol
# Filter the bristolBroadbandSpeed data for the city of Bristol
# Group the data by lsoa_area
# Calculate the average and maximum download speeds for each lsoa_area
# Take a random sample of 20 rows from the data
# Reshape the data from wide to long format for easier plotting
# Create a bar plot with lsoa_area on the x-axis, download speed on the y-axis, and speed_type as the fill color
# Set the fill colors for average and maximum speeds
# Add titles and labels to the plot
# Set the theme to minimal and rotate x-axis labels for better readability
bristolBroadbandSpeed %>%
  filter(city == "Bristol, City of") %>%
  group_by(lsoa_area) %>%
  summarise(
    avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
    max_speed = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE)
  ) %>%
  slice_sample(n = 20) %>%
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = lsoa_area, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("avg_speed" = "steelblue", "max_speed" = "darkorange")) +
  labs(
    title = "Average and Maximum Download Speeds across LSOA areas of Bristol city",
    x = "LSOA Area",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ---------------------------------------------------------------------------------
# Bar Chart 2: Average and Maximum Download Speeds in Cornwall
# Filter the cornwallBroadbandSpeed data for the city of Cornwall
# Group the data by lsoa_area
# Calculate the average and maximum download speeds for each lsoa_area
# Take a random sample of 20 rows from the data
# Reshape the data from wide to long format for easier plotting
# Create a bar plot with lsoa_area on the x-axis, download speed on the y-axis, and speed_type as the fill color
# Set the fill colors for average and maximum speeds
# Add titles and labels to the plot
# Set the theme to minimal and rotate x-axis labels for better readability
cornwallBroadbandSpeed %>%
  filter(city == "Cornwall") %>%
  group_by(lsoa_area) %>%
  summarise(
    avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
    max_speed = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE)
  ) %>%
  slice_sample(n = 20) %>%
  pivot_longer(cols = c(avg_speed, max_speed), names_to = "speed_type", values_to = "speed") %>%
  ggplot(aes(x = lsoa_area, y = speed, fill = speed_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("avg_speed" = "forestgreen", "max_speed" = "firebrick")) +
  labs(
    title = "Average and Maximum Download Speeds across LSOA areas of Cornwall city",
    x = "LSOA Area",
    y = "Download Speed (Mbit/s)",
    fill = "Speed Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

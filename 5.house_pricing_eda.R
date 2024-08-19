# Load necessary libraries
library(tidyverse)
library(lubridate)
library(plotly)

# The code loads cleaned data sets for house prices in Bristol and Cornwall, along with a combined dataset.
# These datasets contain information about house prices in various towns/cities within the counties.
bristolHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
cornwallHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")
bcHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-cornwall-house-pricing.csv")

# The code sets an option to avoid scientific notation in the plots, ensuring that numbers are displayed in a more readable format.
options(scipen = 1000)

# The code filters the dataset to include only data from the year 2022 and creates a box plot of house prices for Bristol and Cornwall.
# It visualizes the distribution of house prices within each county, focusing on the range between 150,000 and 800,000.
# The plot is interactive, allowing users to hover over data points to see specific values.
house_price_boxplot <- bcHousePrice %>% 
  filter(year(Date_of_transfer) == 2022) %>%
  ggplot(aes(x = County, y = Price)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(150000, 800000)) + 
  labs(
    title = "House Prices of 2022 in Bristol and Cornwall",
    x = "County",
    y = "House Price"
  ) +
  theme_minimal()

# The ggplotly function makes the box plot interactive, adding tooltips that appear when hovering over the data points.
ggplotly(house_price_boxplot, tooltip = "text")

# The code provides a summary of house prices in 2022 for each county.
# It calculates and prints the minimum, first quartile (Q1), median, third quartile (Q3), and maximum values of house prices.
summary_bc_2022 <- bcHousePrice %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  group_by(County) %>% 
  summarise(
    Min = min(Price),
    Q1 = quantile(Price, 0.25),
    Median = median(Price),
    Q3 = quantile(Price, 0.75),
    Max = max(Price)
  )
print(summary_bc_2022)

# The code defines a vector of labels for different property types, such as Detached, Flat, Other, Semi-Detached, and Terraced.
# These labels are used later in the plots to make the property types more understandable to the users.
property_type_labels <- c(
  "D" = "Detached",
  "F" = "Flat",
  "O" = "Other",
  "S" = "Semi-Detached",
  "T" = "Terraced"
)

# The code defines a function named `plot_avg_house_prices` that generates a bar plot of average house prices by property type for a given dataset.
# It filters the dataset to include only data from 2022, groups the data by town/city and property type, and calculates the average price for each group.
# The resulting plot shows the average prices of different property types in each town/city within the specified county.
# The function takes two arguments: `house_data` (the dataset to be used) and `county_name` (the name of the county).
plot_avg_house_prices <- function(house_data, county_name) {
  house_data %>% 
    filter(year(Date_of_transfer) == 2022) %>%
    group_by(`Town/City`, Property_type) %>%
    summarise(avg_price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = Property_type, y = avg_price, fill = Property_type)) +
    geom_bar(stat = "identity", width = 0.4) +
    facet_wrap(~`Town/City`) +
    scale_fill_discrete(labels = property_type_labels, name = "Property Type") +
    labs(
      title = paste("Average House Prices of 2022 in Towns/Cities of", county_name),
      x = "",
      y = "Average House Price",
      fill = property_type_labels
    ) +
    theme(
      strip.text = element_text(size = 6),  # Adjust size of facet labels
      axis.text.y = element_text(size = 7)  # Adjust size of y-axis labels
    )
}

# The code calls the `plot_avg_house_prices` function to generate and display the bar plot of average house prices by property type for Bristol.
plot_avg_house_prices(bristolHousePrice, "Bristol")

# Similarly, the code calls the `plot_avg_house_prices` function to generate and display the bar plot of average house prices by property type for Cornwall.
plot_avg_house_prices(cornwallHousePrice, "Cornwall")

# The code calculates the average house prices over the years for the counties of Bristol and Cornwall.
# It starts by adding a new column "Year" to the `bcHousePrice` dataset, which extracts the year from the "Date_of_transfer" column.
# Then, it groups the data by "Year" and "County" and calculates the average price using the `mean()` function.
# The resulting dataset is then used to create a line plot using ggplot.
# The x-axis represents the years, the y-axis represents the average house price, and each line represents a county.
# The plot also includes points to show the actual average prices for each year.
# The title, x-axis label, and y-axis label are set using the `labs()` function.
# Finally, the plot is faceted by county, which means separate line plots will be created for each county.
bcHousePrice %>% 
  mutate(Year = year(Date_of_transfer)) %>%
  group_by(Year, County) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = Year, y = avg_price)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Line Plot: Average House Prices from 2020-2023 in Bristol and Cornwall Counties",
    x = "Year",
    y = "Average House Price"
  ) +
  facet_wrap(~County)

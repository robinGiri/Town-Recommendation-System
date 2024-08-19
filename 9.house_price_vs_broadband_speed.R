library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------
# Load the cleaned datasets for house pricing and broadband speeds in Bristol and Cornwall
bristolHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
cornwallHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")

bristolBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
cornwallBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")

# -----------------------------------------------------------------------
# LINEAR MODEL: House Prices vs Download Speed in Bristol (2022)
# Merge the datasets for Bristol house prices and broadband speeds
# Filter for transactions in the year 2022
# Select relevant columns (Price and Average download speed)
bs_housePrice_broadBand_2022 <- inner_join(
  bristolHousePrice, 
  bristolBroadbandSpeed,
  by = c("Postcode" = "postcode_space")
) %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  select(Price, `Average download speed (Mbit/s)`)

# -----------------------------------------------------------------------
# SUMMARY STATISTICS: Correlation and Linear Model for Bristol
# Calculate the correlation coefficient between house prices and download speed
# Build a linear model to assess the relationship
bs_housePrice_broadBand_2022 %>% 
  summarise(corCoeff = cor(Price, `Average download speed (Mbit/s)`))

model_housePrice_broadBand <- lm(Price ~ `Average download speed (Mbit/s)`, data = bs_housePrice_broadBand_2022)
summary(model_housePrice_broadBand)

# Extract the intercept and slope from the model for plotting the line of best fit
intercept_Price_DownloadSpeed <- coef(model_housePrice_broadBand)[1]
slope_Price_DownloadSpeed <- coef(model_housePrice_broadBand)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Bristol
# Create a scatter plot to visualize the relationship
# Add the line of best fit using the calculated intercept and slope
ggplot(bs_housePrice_broadBand_2022, aes(x = `Average download speed (Mbit/s)`, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = intercept_Price_DownloadSpeed, slope = slope_Price_DownloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on House Price in Bristol (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price (£)"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------
# LINEAR MODEL: House Prices vs Download Speed in Cornwall (2022)
# Merge the datasets for Cornwall house prices and broadband speeds
# Filter for transactions in the year 2022
# Select relevant columns (Price and Average download speed)
cw_housePrice_broadBand_2022 <- inner_join(
  cornwallHousePrice, 
  cornwallBroadbandSpeed,
  by = c("Postcode" = "postcode_space")
) %>% 
  filter(year(Date_of_transfer) == 2022) %>% 
  select(Price, `Average download speed (Mbit/s)`)

# -----------------------------------------------------------------------
# SUMMARY STATISTICS: Correlation and Linear Model for Cornwall
# Calculate the correlation coefficient between house prices and download speed
# Build a linear model to assess the relationship
cw_housePrice_broadBand_2022 %>% 
  summarise(corCoeff = cor(Price, `Average download speed (Mbit/s)`))

model_housePrice_broadBand_cw <- lm(Price ~ `Average download speed (Mbit/s)`, data = cw_housePrice_broadBand_2022)
summary(model_housePrice_broadBand_cw)

# Extract the intercept and slope from the model for plotting the line of best fit
intercept_Price_DownloadSpeed_cw <- coef(model_housePrice_broadBand_cw)[1]
slope_Price_DownloadSpeed_cw <- coef(model_housePrice_broadBand_cw)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Cornwall
# Create a scatter plot to visualize the relationship
# Add the line of best fit using the calculated intercept and slope
ggplot(cw_housePrice_broadBand_2022, aes(x = `Average download speed (Mbit/s)`, y = Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = intercept_Price_DownloadSpeed_cw, slope = slope_Price_DownloadSpeed_cw, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on House Price in Cornwall (2022)",
    x = "Average Download Speed (Mbit/s)",
    y = "House Price (£)"
  ) +
  theme_minimal()

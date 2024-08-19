library(tidyverse)
library(lubridate)

# -----------------------------------------------------------------------
# Load the cleaned datasets for house pricing and schools in Bristol and Cornwall
bristolHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/bristol-house-pricing.csv")
cornwallHousePrice <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/house_pricing/cornwall-house-pricing.csv")

bristolSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/bristol-schools.csv")
cornwallSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/cornwall-schools.csv")

# -----------------------------------------------------------------------
# Filter the house price data for transactions in 2022-2023 and calculate the average house price per postcode
bristolHousePrice_2022_2023 <- bristolHousePrice %>% 
  filter(year(Date_of_transfer) == 2022 | year(Date_of_transfer) == 2023) %>% 
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')

cornwallHousePrice_2022_2023 <- cornwallHousePrice %>% 
  filter(year(Date_of_transfer) == 2022 | year(Date_of_transfer) == 2023) %>% 
  group_by(Postcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = 'drop')

# -----------------------------------------------------------------------
# Filter the schools data for the academic years 2021-2022 and 2022-2023 and calculate the average Attainment 8 score per postcode
bristolSchools_2022_2023 <- bristolSchools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')

cornwallSchools_2022_2023 <- cornwallSchools %>% 
  filter(Year == 2022 | Year == 2023) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(POSTCODE) %>%
  summarise(Avg_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')

# -----------------------------------------------------------------------
# LINEAR MODELLING: Attainment 8 Scores vs House Prices in Bristol (2022-2023)
# Merge the datasets for Bristol house prices and Attainment 8 scores
bs_housePrice_att8scr_2022_2023 <- inner_join(bristolHousePrice_2022_2023, bristolSchools_2022_2023, 
                                             by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)

# Calculate correlation and build the linear model
# Weak, negative relationship (-0.291)
bs_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR))

bsModel_housePrice_att8scr_2022_2023 <- lm(Avg_Price ~ Avg_ATT8SCR, data = bs_housePrice_att8scr_2022_2023)
summary(bsModel_housePrice_att8scr_2022_2023)

# Extract intercept and slope for plotting
bsIntercept_housePrice_att8scr_2022_2023 <- coef(bsModel_housePrice_att8scr_2022_2023)[1]
bsSlope_housePrice_att8scr_2022_2023 <- coef(bsModel_housePrice_att8scr_2022_2023)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Bristol
ggplot(bs_housePrice_att8scr_2022_2023, aes(x = Avg_ATT8SCR, y = Avg_Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = bsIntercept_housePrice_att8scr_2022_2023, slope = bsSlope_housePrice_att8scr_2022_2023, color = "red") +      
  labs(
    title = "Influence of Attainment 8 Scores on House Price in Bristol (2022-2023)",
    x = "Attainment 8 Scores (average)",
    y = "House Price (average)"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------
# LINEAR MODELLING: Attainment 8 Scores vs House Prices in Cornwall (2022-2023)
# Merge the datasets for Cornwall house prices and Attainment 8 scores
cw_housePrice_att8scr_2022_2023 <- inner_join(cornwallHousePrice_2022_2023, cornwallSchools_2022_2023, 
                                             by = c("Postcode" = "POSTCODE")) %>% 
  select(Avg_Price, Avg_ATT8SCR)

# Calculate correlation and build the linear model
# Strong, negative relationship (-1)
cw_housePrice_att8scr_2022_2023 %>% 
  summarise(corCoeff = cor(Avg_Price, Avg_ATT8SCR))

cwModel_housePrice_att8scr_2022_2023 <- lm(Avg_Price ~ Avg_ATT8SCR, data = cw_housePrice_att8scr_2022_2023)
summary(cwModel_housePrice_att8scr_2022_2023)

# Extract intercept and slope for plotting
cwIntercept_housePrice_att8scr_2022_2023 <- coef(cwModel_housePrice_att8scr_2022_2023)[1]
cwSlope_housePrice_att8scr_2022_2023 <- coef(cwModel_housePrice_att8scr_2022_2023)[2]

# -----------------------------------------------------------------------
# VISUALIZATION: Scatter Plot with Line of Best Fit for Cornwall
ggplot(cw_housePrice_att8scr_2022_2023, aes(x = Avg_ATT8SCR, y = Avg_Price)) +
  geom_point(color = "blue", size = 2) +   
  geom_abline(intercept = cwIntercept_housePrice_att8scr_2022_2023, slope = cwSlope_housePrice_att8scr_2022_2023, color = "red") +      
  labs(
    title = "Influence of Attainment 8 Scores on House Price in Cornwall (2022-2023)",
    x = "Attainment 8 Scores (average)",
    y = "House Price (average)"
  ) +
  theme_minimal()

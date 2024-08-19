library(tidyverse)

# Load the datasets
bristolBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/bristol-broadband-speed.csv")
cornwallBroadbandSpeed <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/broadband/cornwall-broadband-speed.csv")

bristolSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/bristol-schools.csv")
cornwallSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/schools/cornwall-schools.csv")

# Filter and prepare the data for Bristol schools
bristolSchools2022 <- bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Filter and prepare the data for Cornwall schools
cornwallSchools2022 <- cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Linear Modelling for Schools in Bristol County
bs_att8scr_downloadSpeed_2022 <- inner_join(
  bristolSchools2022, bristolBroadbandSpeed,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)

# Calculate correlation and build the linear model
# Weak negative relationship (-0.284)
bs_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

bsModel_att8scr_downloadSpeed <- lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = bs_att8scr_downloadSpeed_2022)
summary(bsModel_att8scr_downloadSpeed)

# Extract the intercept and slope
bsIntercept_att8scr_downloadSpeed <- coef(bsModel_att8scr_downloadSpeed)[1]
bsSlope_att8scr_downloadSpeed <- coef(bsModel_att8scr_downloadSpeed)[2]

# Plot the relationship with the line of best fit
ggplot(bs_att8scr_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = bsIntercept_att8scr_downloadSpeed, slope = bsSlope_att8scr_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Bristol Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

# Linear Modelling for Schools in Cornwall County
cw_att8scr_downloadSpeed_2022 <- inner_join(
  cornwallSchools2022, cornwallBroadbandSpeed,
  by = c("POSTCODE" = "postcode_space")) %>% 
  select(ATT8SCR, `Average download speed (Mbit/s)`)

# Calculate correlation and build the linear model
# Strong positive relationship (0.858)
cw_att8scr_downloadSpeed_2022 %>% 
  summarise(corCoeff = cor(ATT8SCR, `Average download speed (Mbit/s)`))

cwModel_att8scr_downloadSpeed <- lm(ATT8SCR ~ `Average download speed (Mbit/s)`, data = cw_att8scr_downloadSpeed_2022)
summary(cwModel_att8scr_downloadSpeed)

# Extract the intercept and slope
cwIntercept_att8scr_downloadSpeed <- coef(cwModel_att8scr_downloadSpeed)[1]
cwSlope_att8scr_downloadSpeed <- coef(cwModel_att8scr_downloadSpeed)[2]

# Plot the relationship with the line of best fit
ggplot(cw_att8scr_downloadSpeed_2022, aes(x = `Average download speed (Mbit/s)`, y = ATT8SCR)) +
  geom_point(color = "blue", size = 2) + 
  geom_abline(intercept = cwIntercept_att8scr_downloadSpeed, slope = cwSlope_att8scr_downloadSpeed, color = "red") +      
  labs(
    title = "Impact of Average Download Speed on Attainment 8 Score in Cornwall Schools (2022)",
    x = "Average Download Speed",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

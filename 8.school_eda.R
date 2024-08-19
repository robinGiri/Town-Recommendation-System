library(tidyverse)
library(plotly)

# Load the cleaned datasets for Bristol and Cornwall schools
bristolSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/bristol-schools.csv")
cornwallSchools <- read_csv("/Users/bijayagiri/Downloads/clean_source_data/Schools/cornwall-schools.csv")

# Disable scientific notation for better readability of numbers
options(scipen = 1000)

# -----------------------------------------------------------------------
# BOX PLOT: Average Attainment 8 Score in Both Counties (2021-2022)
# Merge the datasets for Bristol and Cornwall
# Filter the data for the academic year 2021-2022 and remove invalid scores
# Convert Attainment 8 scores to numeric and calculate the average for each school
# Create a box plot to show the distribution of average Attainment 8 scores

att8scr2022BoxPlot <- bind_rows(bristolSchools, cornwallSchools) %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME, COUNTY) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>% 
  ggplot(aes(x = COUNTY, y = avg_ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Attainment 8 Scores in the Academic Session 2021-2022",
       x = "County",
       y = "Attainment 8 Scores") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggplotly(att8scr2022BoxPlot, tooltip = "text")  

# -----------------------------------------------------------------------
# LINE CHART: Bristol Average Attainment 8 Score (2021-2022)
# Filter the data for Bristol schools in the academic year 2021-2022
# Remove invalid scores and convert the data to numeric format
# Calculate the average Attainment 8 score for each school
# Create a line chart to show the average scores across Bristol schools

bristolSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Bristol in 2021-2022",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

# -----------------------------------------------------------------------
# LINE CHART: Cornwall Average Attainment 8 Score (2021-2022)
# Filter the data for Cornwall schools in the academic year 2021-2022
# Remove invalid scores and convert the data to numeric format
# Calculate the average Attainment 8 score for each school
# Create a line chart to show the average scores across Cornwall schools

cornwallSchools %>% 
  filter(Year == 2022 & ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>% 
  group_by(SCHNAME) %>% 
  summarise(avg_ATT8SCR = mean(ATT8SCR)) %>%
  ggplot(aes(x = SCHNAME, y = avg_ATT8SCR, group = 1)) +
  geom_line() +
  geom_point() + 
  labs(title = "Average Attainment 8 Scores across Schools of Cornwall in 2021-2022",
       x = "Schools",
       y = "Average Attainment 8 Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

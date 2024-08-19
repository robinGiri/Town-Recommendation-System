library(tidyverse)
library(readr)
library(dplyr)

# TODO: refactor this code into smaller chunks for better readability and maintainability

# FOR CRIME DATA SET OF BRISTOL IN 2021
# First, the data sets for 2021 are loaded and analyzed by dimensions and the number of missing values for each column
bristolCrimeRate2105 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-05/2021-05-avon-and-somerset-street.csv")
bristolCrimeRate2106 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-06/2021-06-avon-and-somerset-street.csv")
bristolCrimeRate2107 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-07/2021-07-avon-and-somerset-street.csv")
bristolCrimeRate2108 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-08/2021-08-avon-and-somerset-street.csv")
bristolCrimeRate2109 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-09/2021-09-avon-and-somerset-street.csv")
bristolCrimeRate2110 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-10/2021-10-avon-and-somerset-street.csv")
bristolCrimeRate2111 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-11/2021-11-avon-and-somerset-street.csv")
bristolCrimeRate2112 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-12/2021-12-avon-and-somerset-street.csv")

colSums(is.na(bristolCrimeRate2105))
colSums(is.na(bristolCrimeRate2106))
colSums(is.na(bristolCrimeRate2107))
colSums(is.na(bristolCrimeRate2108))
colSums(is.na(bristolCrimeRate2109))
colSums(is.na(bristolCrimeRate2110))
colSums(is.na(bristolCrimeRate2111))
colSums(is.na(bristolCrimeRate2112))

# All the data sets for 2021 show missing Crime ID values, which constitute about 14-20% of the column.
# The Crime ID can be considered as the primary key, as it is a unique identifier for each row.
# If the Crime ID is unknown, the corresponding records may lose their significance, potentially compromising data integrity.
# Moreover, the presence of missing Crime IDs can introduce risks in terms of accuracy and reliability in future analysis,
# as it may lead to duplicated rows and incorrect trends.
# Therefore, it is advisable to remove the rows with missing Crime IDs.
bristolCrimeRate2105 = bristolCrimeRate2105 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bristolCrimeRate2105))

bristolCrimeRate2106 = bristolCrimeRate2106 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2107 = bristolCrimeRate2107 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2108 = bristolCrimeRate2108 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2109 = bristolCrimeRate2109 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2110 = bristolCrimeRate2110 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2111 = bristolCrimeRate2111 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2112 = bristolCrimeRate2112 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bristolCrimeRate2112))

# After removing the rows with missing Crime IDs, bristolCrimeRate2021 is created by merging all the data sets for 2021.
bristolCrimeRate2021 = bind_rows(bristolCrimeRate2105, bristolCrimeRate2106, bristolCrimeRate2107, bristolCrimeRate2108,
                            bristolCrimeRate2109, bristolCrimeRate2110, bristolCrimeRate2111, bristolCrimeRate2112)

colSums(is.na(bristolCrimeRate2021))
# The column "Context" contains missing values that are equal to the number of rows in the dataset,
# indicating that the column is completely empty and serves no purpose.
# Therefore, it is recommended to remove the column.
bristolCrimeRate2021 = bristolCrimeRate2021 %>% 
  select(-Context)
colSums(is.na(bristolCrimeRate2021))

str(bristolCrimeRate2021)
# The summary for the columns "Longitude" and "Latitude" indicates a small difference between the 3rd quartile and the maximum outlier.
# This suggests that there may be some skewness in the data distribution, although it is relatively minor.
# It is important to consider this when analyzing and interpreting the data.
summary(bristolCrimeRate2021)

# Remove duplicated rows that can cause inaccuracies while imputing missing values
sum(duplicated(bristolCrimeRate2021))
duplicated_rows = duplicated(bristolCrimeRate2021)
bristolCrimeRate2021 = bristolCrimeRate2021[!duplicated_rows, ]
colSums(is.na(bristolCrimeRate2021))

#The missing values are filled by the median of the column
#Medians are less sensitive to outliers than mean, so using median lowers the risks
bristolCrimeRate2021$Longitude[is.na(bristolCrimeRate2021$Longitude)] = median(bristolCrimeRate2021$Longitude, na.rm = TRUE)
bristolCrimeRate2021$Latitude[is.na(bristolCrimeRate2021$Latitude)] = median(bristolCrimeRate2021$Latitude, na.rm = TRUE)
colSums(is.na(bristolCrimeRate2021))

#Since columns "LSOA code" and "LSOA name" are categorical variables, mode is used the fill the missing values
#Function get_mode() is defined since R doesn't have built-in mode function
get_mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Calculation of modes of LSOA code and LSOA name
lsoaCodeMode = get_mode(bristolCrimeRate2021$`LSOA code`[!is.na(bristolCrimeRate2021$`LSOA code`)])
lsoaNameMode = get_mode(bristolCrimeRate2021$`LSOA name`[!is.na(bristolCrimeRate2021$`LSOA name`)])
#Imputation of missing values for LSOA code with the mode
bristolCrimeRate2021$`LSOA code`[is.na(bristolCrimeRate2021$`LSOA code`)] = lsoaCodeMode
# Imputation of missing values for LSOA name with the mode
bristolCrimeRate2021$`LSOA name`[is.na(bristolCrimeRate2021$`LSOA name`)] = lsoaNameMode
colSums(is.na(bristolCrimeRate2021))
summary(bristolCrimeRate2021)
sum(duplicated(bristolCrimeRate2021))


#---------------------------------------
#FOR CRIME DATA SET OF BRISTOL IN 2022
#Data sets of crime rate in Bristol in 2022 are loaded and analyzed by dimensions and missing values
bristolCrimeRate2201 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-01/2022-01-avon-and-somerset-street.csv")
bristolCrimeRate2202 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-02/2022-02-avon-and-somerset-street.csv")
bristolCrimeRate2203 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-03/2022-03-avon-and-somerset-street.csv")
bristolCrimeRate2204 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-04/2022-04-avon-and-somerset-street.csv")
bristolCrimeRate2205 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-05/2022-05-avon-and-somerset-street.csv")
bristolCrimeRate2206 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-06/2022-06-avon-and-somerset-street.csv")
bristolCrimeRate2207 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-07/2022-07-avon-and-somerset-street.csv")
bristolCrimeRate2208 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-08/2022-08-avon-and-somerset-street.csv")
bristolCrimeRate2209 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-09/2022-09-avon-and-somerset-street.csv")
bristolCrimeRate2210 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-10/2022-10-avon-and-somerset-street.csv")
bristolCrimeRate2211 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-11/2022-11-avon-and-somerset-street.csv")
bristolCrimeRate2212 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-12/2022-12-avon-and-somerset-street.csv")

colSums(is.na(bristolCrimeRate2201))
colSums(is.na(bristolCrimeRate2202))
colSums(is.na(bristolCrimeRate2203))
colSums(is.na(bristolCrimeRate2204))
colSums(is.na(bristolCrimeRate2205))
colSums(is.na(bristolCrimeRate2206))
colSums(is.na(bristolCrimeRate2207))
colSums(is.na(bristolCrimeRate2208))
colSums(is.na(bristolCrimeRate2209))
colSums(is.na(bristolCrimeRate2210))
colSums(is.na(bristolCrimeRate2211))
colSums(is.na(bristolCrimeRate2212))

#The rows with missing crime ids are removed from each data set
bristolCrimeRate2201 = bristolCrimeRate2201 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bristolCrimeRate2201))

bristolCrimeRate2202 = bristolCrimeRate2202 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2203 = bristolCrimeRate2203 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2204 = bristolCrimeRate2204 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2205 = bristolCrimeRate2205 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2206 = bristolCrimeRate2206 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2207 = bristolCrimeRate2207 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2208 = bristolCrimeRate2208 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2209 = bristolCrimeRate2209 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2210 = bristolCrimeRate2210 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2211 = bristolCrimeRate2211 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2212 = bristolCrimeRate2212 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bristolCrimeRate2212))

#Data set bristolCrimeRate2022 is created by merging data sets of Bristol from 2022
bristolCrimeRate2022 = bind_rows(bristolCrimeRate2201, bristolCrimeRate2202, bristolCrimeRate2203, bristolCrimeRate2204,
                            bristolCrimeRate2205, bristolCrimeRate2206, bristolCrimeRate2207, bristolCrimeRate2208, 
                            bristolCrimeRate2209, bristolCrimeRate2210, bristolCrimeRate2211, bristolCrimeRate2212)

#Column "Context" is removed because it is completely empty
colSums(is.na(bristolCrimeRate2022))
bristolCrimeRate2022 = bristolCrimeRate2022 %>% 
  select(-Context)
colSums(is.na(bristolCrimeRate2022))

#The data set has duplicated rows and they are removed
sum(duplicated(bristolCrimeRate2022))
duplicated_rows22 = duplicated(bristolCrimeRate2022)
bristolCrimeRate2022 = bristolCrimeRate2022[!duplicated_rows22, ]
colSums(is.na(bristolCrimeRate2022))

summary(bristolCrimeRate2022)

#The missing values of Longitude and Latitude are imputed by their median values
bristolCrimeRate2022$Longitude[is.na(bristolCrimeRate2022$Longitude)] = median(bristolCrimeRate2022$Longitude, na.rm = TRUE)
bristolCrimeRate2022$Latitude[is.na(bristolCrimeRate2022$Latitude)] = median(bristolCrimeRate2022$Latitude, na.rm = TRUE)
colSums(is.na(bristolCrimeRate2022))

#Using the user-defined get_mode() function, mode of LSOA code and LSOA name are calculated and imputed
lsoaCodeMode22 = get_mode(bristolCrimeRate2022$`LSOA code`[!is.na(bristolCrimeRate2022$`LSOA code`)])
lsoaNameMode22 = get_mode(bristolCrimeRate2022$`LSOA name`[!is.na(bristolCrimeRate2022$`LSOA name`)])
bristolCrimeRate2022$`LSOA code`[is.na(bristolCrimeRate2022$`LSOA code`)] = lsoaCodeMode22
bristolCrimeRate2022$`LSOA name`[is.na(bristolCrimeRate2022$`LSOA name`)] = lsoaNameMode22
colSums(is.na(bristolCrimeRate2022))
summary(bristolCrimeRate2022)
sum(duplicated(bristolCrimeRate2022))


#---------------------------------------
#FOR CRIME DATA SET OF BRISTOL IN 2023
#Data sets of crime rate in Bristol in 2023 are loaded and analyzed via dimensions and missing values
bristolCrimeRate2301 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-01/2023-01-avon-and-somerset-street.csv")
bristolCrimeRate2302 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-02/2023-02-avon-and-somerset-street.csv")
bristolCrimeRate2303 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-03/2023-03-avon-and-somerset-street.csv")
bristolCrimeRate2304 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-04/2023-04-avon-and-somerset-street.csv")
bristolCrimeRate2305 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-05/2023-05-avon-and-somerset-street.csv")
bristolCrimeRate2306 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-06/2023-06-avon-and-somerset-street.csv")
bristolCrimeRate2307 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-07/2023-07-avon-and-somerset-street.csv")
bristolCrimeRate2308 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-08/2023-08-avon-and-somerset-street.csv")
bristolCrimeRate2309 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-09/2023-09-avon-and-somerset-street.csv")
bristolCrimeRate2310 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-10/2023-10-avon-and-somerset-street.csv")
bristolCrimeRate2311 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-11/2023-11-avon-and-somerset-street.csv")
bristolCrimeRate2312 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-12/2023-12-avon-and-somerset-street.csv")

colSums(is.na(bristolCrimeRate2301))
colSums(is.na(bristolCrimeRate2302))
colSums(is.na(bristolCrimeRate2303))
colSums(is.na(bristolCrimeRate2304))
colSums(is.na(bristolCrimeRate2305))
colSums(is.na(bristolCrimeRate2306))
colSums(is.na(bristolCrimeRate2307))
colSums(is.na(bristolCrimeRate2308))
colSums(is.na(bristolCrimeRate2309))
colSums(is.na(bristolCrimeRate2310))
colSums(is.na(bristolCrimeRate2311))
colSums(is.na(bristolCrimeRate2312))

#Rows with missing crime ids are removed from data sets of 2023
bristolCrimeRate2301 = bristolCrimeRate2301 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(bristolCrimeRate2301))

bristolCrimeRate2302 = bristolCrimeRate2302 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2303 = bristolCrimeRate2303 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2304 = bristolCrimeRate2304 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2305 = bristolCrimeRate2305 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2306 = bristolCrimeRate2306 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2307 = bristolCrimeRate2307 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2308 = bristolCrimeRate2308 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2309 = bristolCrimeRate2309 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2310 = bristolCrimeRate2310 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2311 = bristolCrimeRate2311 %>%
  filter(!is.na(`Crime ID`))
bristolCrimeRate2312 = bristolCrimeRate2312 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(bristolCrimeRate2312))

#Data set bristolCrimeRate2023 is created by merging 2023 data sets of Bristol
bristolCrimeRate2023 = bind_rows(bristolCrimeRate2301, bristolCrimeRate2302, bristolCrimeRate2303, bristolCrimeRate2304,
                            bristolCrimeRate2305, bristolCrimeRate2306, bristolCrimeRate2307, bristolCrimeRate2308, 
                            bristolCrimeRate2309, bristolCrimeRate2310, bristolCrimeRate2311, bristolCrimeRate2312)

#The empty column "Context" is removed
colSums(is.na(bristolCrimeRate2023))
bristolCrimeRate2023 = bristolCrimeRate2023 %>% 
  select(-Context)
colSums(is.na(bristolCrimeRate2023))

#The duplicated rows are removed
sum(duplicated(bristolCrimeRate2023))
duplicated_rows23 = duplicated(bristolCrimeRate2023)
bristolCrimeRate2023 = bristolCrimeRate2023[!duplicated_rows23, ]
colSums(is.na(bristolCrimeRate2023))

summary(bristolCrimeRate2023)

#The missing values of Longitude and Latitude are imputed by their median values
bristolCrimeRate2023$Longitude[is.na(bristolCrimeRate2023$Longitude)] = median(bristolCrimeRate2023$Longitude, na.rm = TRUE)
bristolCrimeRate2023$Latitude[is.na(bristolCrimeRate2023$Latitude)] = median(bristolCrimeRate2023$Latitude, na.rm = TRUE)
colSums(is.na(bristolCrimeRate2023))

#Using get_mode(), the mode of LSOA code and LSOA name are calculated and used for imputation
lsoaCodeMode23 = get_mode(bristolCrimeRate2023$`LSOA code`[!is.na(bristolCrimeRate2023$`LSOA code`)])
lsoaNameMode23 = get_mode(bristolCrimeRate2023$`LSOA name`[!is.na(bristolCrimeRate2023$`LSOA name`)])
bristolCrimeRate2023$`LSOA code`[is.na(bristolCrimeRate2023$`LSOA code`)] = lsoaCodeMode23
bristolCrimeRate2023$`LSOA name`[is.na(bristolCrimeRate2023$`LSOA name`)] = lsoaNameMode23
colSums(is.na(bristolCrimeRate2023))
summary(bristolCrimeRate2023)
sum(duplicated(bristolCrimeRate2023))


#---------------------------------------
#FOR CRIME DATA SET OF CORNWALL IN 2021
#Data sets for 2021 loaded and analyzed
cornwallCrimeRate2105 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-05/2021-05-devon-and-cornwall-street.csv")
cornwallCrimeRate2106 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-06/2021-06-devon-and-cornwall-street.csv")
cornwallCrimeRate2107 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-07/2021-07-devon-and-cornwall-street.csv")
cornwallCrimeRate2108 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-08/2021-08-devon-and-cornwall-street.csv")
cornwallCrimeRate2109 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-09/2021-09-devon-and-cornwall-street.csv")
cornwallCrimeRate2110 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-10/2021-10-devon-and-cornwall-street.csv")
cornwallCrimeRate2111 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-11/2021-11-devon-and-cornwall-street.csv")
cornwallCrimeRate2112 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2021-12/2021-12-devon-and-cornwall-street.csv")

colSums(is.na(cornwallCrimeRate2105))
colSums(is.na(cornwallCrimeRate2106))
colSums(is.na(cornwallCrimeRate2107))
colSums(is.na(cornwallCrimeRate2108))
colSums(is.na(cornwallCrimeRate2109))
colSums(is.na(cornwallCrimeRate2110))
colSums(is.na(cornwallCrimeRate2111))
colSums(is.na(cornwallCrimeRate2112))

#Rows with missing crime id are removed
cornwallCrimeRate2105 = cornwallCrimeRate2105 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cornwallCrimeRate2105))

cornwallCrimeRate2106 = cornwallCrimeRate2106 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2107 = cornwallCrimeRate2107 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2108 = cornwallCrimeRate2108 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2109 = cornwallCrimeRate2109 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2110 = cornwallCrimeRate2110 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2111 = cornwallCrimeRate2111 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2112 = cornwallCrimeRate2112 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cornwallCrimeRate2112))

#Data set cornwallCrimeRate2021 is created by merging all data sets for 2021
cornwallCrimeRate2021 = bind_rows(cornwallCrimeRate2105, cornwallCrimeRate2106, cornwallCrimeRate2107, cornwallCrimeRate2108,
                            cornwallCrimeRate2109, cornwallCrimeRate2110, cornwallCrimeRate2111, cornwallCrimeRate2112)

colSums(is.na(cornwallCrimeRate2021))
#The empty column "Context" is removed
cornwallCrimeRate2021 = cornwallCrimeRate2021 %>% 
  select(-Context)
colSums(is.na(cornwallCrimeRate2021))

str(cornwallCrimeRate2021)
summary(cornwallCrimeRate2021)

#The duplicated rows are removed 
sum(duplicated(cornwallCrimeRate2021))
duplicated_rows_cw21 = duplicated(cornwallCrimeRate2021)
cornwallCrimeRate2021 = cornwallCrimeRate2021[!duplicated_rows_cw21, ]
colSums(is.na(cornwallCrimeRate2021))

#Imputation of missing values: Longitude and Latitude
cornwallCrimeRate2021$Longitude[is.na(cornwallCrimeRate2021$Longitude)] = median(cornwallCrimeRate2021$Longitude, na.rm = TRUE)
cornwallCrimeRate2021$Latitude[is.na(cornwallCrimeRate2021$Latitude)] = median(cornwallCrimeRate2021$Latitude, na.rm = TRUE)
colSums(is.na(cornwallCrimeRate2021))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw21 = get_mode(cornwallCrimeRate2021$`LSOA code`[!is.na(cornwallCrimeRate2021$`LSOA code`)])
lsoaNameMode_cw21 = get_mode(cornwallCrimeRate2021$`LSOA name`[!is.na(cornwallCrimeRate2021$`LSOA name`)])
cornwallCrimeRate2021$`LSOA code`[is.na(cornwallCrimeRate2021$`LSOA code`)] = lsoaCodeMode_cw21
cornwallCrimeRate2021$`LSOA name`[is.na(cornwallCrimeRate2021$`LSOA name`)] = lsoaNameMode_cw21

colSums(is.na(cornwallCrimeRate2021))
summary(cornwallCrimeRate2021)
sum(duplicated(cornwallCrimeRate2021))


#---------------------------------------
#FOR CRIME DATA SET OF CORNWALL IN 2022
#Data sets for 2022 loaded and analyzed
cornwallCrimeRate2201 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-01/2022-01-devon-and-cornwall-street.csv")
cornwallCrimeRate2202 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-02/2022-02-devon-and-cornwall-street.csv")
cornwallCrimeRate2203 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-03/2022-03-devon-and-cornwall-street.csv")
cornwallCrimeRate2204 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-04/2022-04-devon-and-cornwall-street.csv")
cornwallCrimeRate2205 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-05/2022-05-devon-and-cornwall-street.csv")
cornwallCrimeRate2206 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-06/2022-06-devon-and-cornwall-street.csv")
cornwallCrimeRate2207 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-07/2022-07-devon-and-cornwall-street.csv")
cornwallCrimeRate2208 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-08/2022-08-devon-and-cornwall-street.csv")
cornwallCrimeRate2209 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-09/2022-09-devon-and-cornwall-street.csv")
cornwallCrimeRate2210 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-10/2022-10-devon-and-cornwall-street.csv")
cornwallCrimeRate2211 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-11/2022-11-devon-and-cornwall-street.csv")
cornwallCrimeRate2212 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2022-12/2022-12-devon-and-cornwall-street.csv")

colSums(is.na(cornwallCrimeRate2201))
colSums(is.na(cornwallCrimeRate2202))
colSums(is.na(cornwallCrimeRate2203))
colSums(is.na(cornwallCrimeRate2204))
colSums(is.na(cornwallCrimeRate2205))
colSums(is.na(cornwallCrimeRate2206))
colSums(is.na(cornwallCrimeRate2207))
colSums(is.na(cornwallCrimeRate2208))
colSums(is.na(cornwallCrimeRate2209))
colSums(is.na(cornwallCrimeRate2210))
colSums(is.na(cornwallCrimeRate2211))
colSums(is.na(cornwallCrimeRate2212))

#Rows with missing crime id are removed
cornwallCrimeRate2201 = cornwallCrimeRate2201 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cornwallCrimeRate2201))

cornwallCrimeRate2202 = cornwallCrimeRate2202 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2203 = cornwallCrimeRate2203 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2204 = cornwallCrimeRate2204 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2205 = cornwallCrimeRate2205 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2206 = cornwallCrimeRate2206 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2207 = cornwallCrimeRate2207 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2208 = cornwallCrimeRate2208 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2209 = cornwallCrimeRate2209 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2210 = cornwallCrimeRate2210 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2211 = cornwallCrimeRate2211 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2212 = cornwallCrimeRate2212 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cornwallCrimeRate2212))

#Data set cornwallCrimeRate2022 is created by merging all data sets from 2022
cornwallCrimeRate2022 = bind_rows(cornwallCrimeRate2201, cornwallCrimeRate2202, cornwallCrimeRate2203, cornwallCrimeRate2204,
                            cornwallCrimeRate2205, cornwallCrimeRate2206, cornwallCrimeRate2207, cornwallCrimeRate2208,
                            cornwallCrimeRate2209, cornwallCrimeRate2210, cornwallCrimeRate2211, cornwallCrimeRate2212)

colSums(is.na(cornwallCrimeRate2022))
#The empty column "Context" is removed
cornwallCrimeRate2022 = cornwallCrimeRate2022 %>% 
  select(-Context)
colSums(is.na(cornwallCrimeRate2022))

summary(cornwallCrimeRate2022)

#The duplicated rows are removed 
sum(duplicated(cornwallCrimeRate2022))
duplicated_rows_cw22 = duplicated(cornwallCrimeRate2022)
cornwallCrimeRate2022 = cornwallCrimeRate2022[!duplicated_rows_cw22, ]
colSums(is.na(cornwallCrimeRate2022))

#Imputation of missing values: Longitude and Latitude
cornwallCrimeRate2022$Longitude[is.na(cornwallCrimeRate2022$Longitude)] = median(cornwallCrimeRate2022$Longitude, na.rm = TRUE)
cornwallCrimeRate2022$Latitude[is.na(cornwallCrimeRate2022$Latitude)] = median(cornwallCrimeRate2022$Latitude, na.rm = TRUE)
colSums(is.na(cornwallCrimeRate2022))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw22 = get_mode(cornwallCrimeRate2022$`LSOA code`[!is.na(cornwallCrimeRate2022$`LSOA code`)])
lsoaNameMode_cw22 = get_mode(cornwallCrimeRate2022$`LSOA name`[!is.na(cornwallCrimeRate2022$`LSOA name`)])
cornwallCrimeRate2022$`LSOA code`[is.na(cornwallCrimeRate2022$`LSOA code`)] = lsoaCodeMode_cw22
cornwallCrimeRate2022$`LSOA name`[is.na(cornwallCrimeRate2022$`LSOA name`)] = lsoaNameMode_cw22

colSums(is.na(cornwallCrimeRate2022))
summary(cornwallCrimeRate2022)
sum(duplicated(cornwallCrimeRate2022))


#---------------------------------------
#FOR CRIME DATA SET OF CORNWALL IN 2023
#Data sets for 2023 loaded and analyzed
cornwallCrimeRate2301 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-01/2023-01-devon-and-cornwall-street.csv")
cornwallCrimeRate2302 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-02/2023-02-devon-and-cornwall-street.csv")
cornwallCrimeRate2303 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-03/2023-03-devon-and-cornwall-street.csv")
cornwallCrimeRate2304 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-04/2023-04-devon-and-cornwall-street.csv")
cornwallCrimeRate2305 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-05/2023-05-devon-and-cornwall-street.csv")
cornwallCrimeRate2306 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-06/2023-06-devon-and-cornwall-street.csv")
cornwallCrimeRate2307 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-07/2023-07-devon-and-cornwall-street.csv")
cornwallCrimeRate2308 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-08/2023-08-devon-and-cornwall-street.csv")
cornwallCrimeRate2309 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-09/2023-09-devon-and-cornwall-street.csv")
cornwallCrimeRate2310 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-10/2023-10-devon-and-cornwall-street.csv")
cornwallCrimeRate2311 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-11/2023-11-devon-and-cornwall-street.csv")
cornwallCrimeRate2312 = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Crime/2023-12/2023-12-devon-and-cornwall-street.csv")

colSums(is.na(cornwallCrimeRate2301))
colSums(is.na(cornwallCrimeRate2302))
colSums(is.na(cornwallCrimeRate2303))
colSums(is.na(cornwallCrimeRate2304))
colSums(is.na(cornwallCrimeRate2305))
colSums(is.na(cornwallCrimeRate2306))
colSums(is.na(cornwallCrimeRate2307))
colSums(is.na(cornwallCrimeRate2308))
colSums(is.na(cornwallCrimeRate2309))
colSums(is.na(cornwallCrimeRate2310))
colSums(is.na(cornwallCrimeRate2311))
colSums(is.na(cornwallCrimeRate2312))

#Rows with missing crime id are removed
cornwallCrimeRate2301 = cornwallCrimeRate2301 %>%
  filter(!is.na(`Crime ID`))
colSums(is.na(cornwallCrimeRate2301))

cornwallCrimeRate2302 = cornwallCrimeRate2302 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2303 = cornwallCrimeRate2303 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2304 = cornwallCrimeRate2304 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2305 = cornwallCrimeRate2305 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2306 = cornwallCrimeRate2306 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2307 = cornwallCrimeRate2307 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2308 = cornwallCrimeRate2308 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2309 = cornwallCrimeRate2309 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2310 = cornwallCrimeRate2310 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2311 = cornwallCrimeRate2311 %>%
  filter(!is.na(`Crime ID`))
cornwallCrimeRate2312 = cornwallCrimeRate2312 %>%
  filter(!is.na(`Crime ID`))

colSums(is.na(cornwallCrimeRate2312))

#Data set cornwallCrimeRate2023 is created by merging all data sets from 2023
cornwallCrimeRate2023 = bind_rows(cornwallCrimeRate2301, cornwallCrimeRate2302, cornwallCrimeRate2303, cornwallCrimeRate2304,
                            cornwallCrimeRate2305, cornwallCrimeRate2306, cornwallCrimeRate2307, cornwallCrimeRate2308,
                            cornwallCrimeRate2309, cornwallCrimeRate2310, cornwallCrimeRate2311, cornwallCrimeRate2312)

colSums(is.na(cornwallCrimeRate2023))
#The empty column "Context" is removed
cornwallCrimeRate2023 = cornwallCrimeRate2023 %>% 
  select(-Context)
colSums(is.na(cornwallCrimeRate2023))

summary(cornwallCrimeRate2023)

#This data set has no duplicated rows 
sum(duplicated(cornwallCrimeRate2023))

#Imputation of missing values: Longitude and Latitude
cornwallCrimeRate2023$Longitude[is.na(cornwallCrimeRate2023$Longitude)] = median(cornwallCrimeRate2023$Longitude, na.rm = TRUE)
cornwallCrimeRate2023$Latitude[is.na(cornwallCrimeRate2023$Latitude)] = median(cornwallCrimeRate2023$Latitude, na.rm = TRUE)
colSums(is.na(cornwallCrimeRate2023))

#Imputation of missing values: LSOA code and LSOA name
lsoaCodeMode_cw23 = get_mode(cornwallCrimeRate2023$`LSOA code`[!is.na(cornwallCrimeRate2023$`LSOA code`)])
lsoaNameMode_cw23 = get_mode(cornwallCrimeRate2023$`LSOA name`[!is.na(cornwallCrimeRate2023$`LSOA name`)])
cornwallCrimeRate2023$`LSOA code`[is.na(cornwallCrimeRate2023$`LSOA code`)] = lsoaCodeMode_cw23
cornwallCrimeRate2023$`LSOA name`[is.na(cornwallCrimeRate2023$`LSOA name`)] = lsoaNameMode_cw23

colSums(is.na(cornwallCrimeRate2023))
summary(cornwallCrimeRate2023)


#-------------------
#CLEANED DATA SETS
bristolCrimeRateCleaned = bind_rows(bristolCrimeRate2021, bristolCrimeRate2022, bristolCrimeRate2023)
sum(duplicated(bristolCrimeRateCleaned))

cornwallCrimeRateCleaned = bind_rows(cornwallCrimeRate2021, cornwallCrimeRate2022, cornwallCrimeRate2023)

#For adding town/city to the data set
pscdToLsoa = read_csv("/Users/bijayagiri/Downloads/raw_source_data/Postcode to LSOA.csv")

#The data set is narrowed down to necessary columns
pscdToLsoa = pscdToLsoa %>% 
  rename(postcode_space = pcds) %>% 
  rename(`LSOA code` = lsoa11cd) %>% 
  rename(city = ladnm) %>% 
  select(postcode_space, `LSOA code`, city)

# Remove duplicated associations of LSOA code with postcode_space and city
# Retain only the first occurrence of postcode_space and city for each LSOA code
# This makes the dataset more manageable and comprehensible
# Each LSOA code is now associated with a single postcode and city
pscdToLsoa = pscdToLsoa %>% 
  group_by(`LSOA code`) %>% 
  summarize(
    postcode_space = first(postcode_space), 
    city = first(city))

#Inner join done for bristolCrimeRateCleaned and pscdToLsoa, only retaining common rows
#bristolCrimeRateCleaned is narrowed down to required columns 
bristolCrimeRateCleaned = bristolCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")

#Data set bristolCrimeSummary records the number of different crimes 
#that occurred in cities of Bristol, from 2021-2023
bristolCrimeSummary = bristolCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)

#cornwallCrimeRateCleaned inner joined with pscdToLsoa
cornwallCrimeRateCleaned = cornwallCrimeRateCleaned %>% 
  rename(Year = Month) %>% 
  select(`Crime ID`, Year, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  inner_join(pscdToLsoa, by = "LSOA code")

#Number of different crimes that occurred in cities of Cornwall from 2021-2023
cornwallCrimeSummary = cornwallCrimeRateCleaned %>% 
  group_by(city, Year, `Crime type`) %>% 
  summarize(CrimeTypeCount = n(), .groups = 'drop') %>% 
  arrange(Year)

write.csv(bristolCrimeRateCleaned, "/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-rate.csv", row.names = FALSE)
write.csv(cornwallCrimeRateCleaned, "/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-rate.csv", row.names = FALSE)
write.csv(bristolCrimeSummary, "/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/bristol-crime-summary.csv", row.names = FALSE)
write.csv(cornwallCrimeSummary, "/Users/bijayagiri/Downloads/clean_source_data/Crime_Rate/cornwall-crime-summary.csv", row.names = FALSE)

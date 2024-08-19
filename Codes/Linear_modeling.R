
#•	House prices vs Download Speed
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)




cleaned_housing = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv")
cleaned_broadband = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv")

# Merge the datasets on the Postcode or any common column
merged_data <- cleaned_housing %>%
  inner_join(cleaned_broadband, by = "Postcode") %>%
  filter(Year == 2022) %>%
  select(Price, AvgDownloadSpeed)

# Convert Price to numeric if needed
merged_data <- merged_data %>% mutate(Price = as.numeric(Price))

# Remove missing values
merged_data <- merged_data %>% drop_na(Price, AvgDownloadSpeed)

View(merged_data)

# Fit a linear model: House prices vs Download Speed
linear_model <- lm(Price ~ AvgDownloadSpeed, data = merged_data)

# Summary statistics of the linear model
summary(linear_model)

# Visualizing the linear model with a line of best fit
ggplot(merged_data, aes(x = Price, y = AvgDownloadSpeed)) +
  geom_point(color = "blue") +  # Scatter plot of the data
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Line of best fit
  labs(title = "House Prices vs Download Speed",
       x = "House Price",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()



#•	House price vs Drug Rate (2023)


library(tidyverse)
library(ggplot2)

# Load the cleaned housing and crime datasets
housing_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv")
crime_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")

# Trim the housing data postcodes to match the crime data format (first part only, e.g., "PL35")
housing_data <- housing_data %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*")) # Regex to capture the area code and district

# Filter crime data for drug-related offenses in 2022 and summarize by postcode
crime_data <- crime_data %>%
  filter(`Crime type` == 'Drugs', Year == 2023) %>%
  group_by(Postcode) %>%
  summarise(Drug_Rate = n(), .groups = 'drop')

# Merge the datasets by Postcode
merged_data <- housing_data %>%
  inner_join(crime_data, by = "Postcode")

View(merged_data)

# Check the merged data
print(head(merged_data))

# Fit a linear model
linear_model <- lm(Price ~ Drug_Rate, data = merged_data)
summary(linear_model)
# Plotting the relationship
ggplot(merged_data, aes(x = Drug_Rate, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "House Prices vs. Drug Rate (2023)",
       x = "Total Drug Offenses",
       y = "House Price") +
  theme_minimal()


# •	Attainment 8 score vs House Price (2022)


library(tidyverse)
library(ggplot2)

# Load the cleaned school and housing datasets
school_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv", show_col_types = FALSE)
housing_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv", show_col_types = FALSE)

school_data <- school_data %>%
  filter(Academic_Year == "2022-2023") %>%
  mutate(PCODE = str_extract(PCODE, "^[A-Z]+\\d+"))

housing_data <- housing_data %>%
  filter(Year == 2022) %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+"))

# Summarize school data by postcode
school_grouped <- school_data %>%
  group_by(PCODE) %>%
  summarise(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')

# Summarize housing data by postcode
housing_grouped <- housing_data %>%
  group_by(Postcode) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE), .groups = 'drop')

# Merge the grouped datasets on postcode
merged_data <- left_join(housing_grouped, school_grouped, by = c("Postcode" = "PCODE"))

# Check the merged data
print(head(merged_data))

# Fit a linear model to explore the relationship between average Attainment 8 scores and average house prices
linear_model = lm(Average_Price ~ Average_ATT8SCR, data = merged_data)
print(summary(linear_model))

# Visualize the relationship between average Attainment 8 scores and average house prices
ggplot(merged_data, aes(x = Average_ATT8SCR, y = Average_Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "ATT8SCR vs Average House Price (2022)",
       x = "Average Attainment 8 Score",
       y = "Average House Price") +
  theme_minimal()


#Linear Modelling of •	Average Download Speed vs Drug Offense Rate per 10000  people

library(tidyverse)
library(ggplot2)
library(readr)

broadband_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv")
crime_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")

# Prepare broadband data by trimming postcodes
broadband_grouped <- broadband_data %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*")) %>%  # Adjust the regex to match the postcode format
  group_by(Postcode) %>%
  summarise(Average_Download_Speed = mean(AvgDownloadSpeed, na.rm = TRUE), .groups = 'drop')

# Prepare crime data for drug-related offenses and calculate offense rate per 10,000 people
crime_grouped <- crime_data %>%
  filter(`Crime type` == 'Drugs') %>%
  group_by(Postcode) %>%
  summarise(Drug_Offense_Rate = (n() / Population2023) * 10000, .groups = 'drop')

# Merge the grouped data on 'Postcode'
merged_data <- left_join(broadband_grouped, crime_grouped, by = "Postcode")

# Fit a linear model to explore the relationship
linear_model <- lm(Drug_Offense_Rate ~ Average_Download_Speed, data = merged_data)
print(summary(linear_model))

# Visualize the relationship between average download speeds and drug offense rates
ggplot(merged_data, aes(x = Average_Download_Speed, y = Drug_Offense_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average Download Speed vs Drug Offense Rate per 10000  people",
       x = "Average Download Speed (Mbps)",
       y = "Drug Offense Rate") +
  theme_minimal()


# Linear Modelling of •	Attainment 8 score vs Drug Offense Rate (2022)

library(tidyverse)
library(readr)
library(ggplot2)

school_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv")
crime_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")

# Trim the school data postcodes
school_data <- school_data %>%
  mutate(PCODE = str_extract(PCODE, "^[A-Z]+\\d+\\s*\\d*"))  # Adjust the regex to match the postcode format

# Prepare crime data
crime_data <- crime_data %>%
  filter(Year == 2022)  # Filter by year if needed and assume already in correct format

# Group and summarize school data
school_grouped <- school_data %>%
  group_by(PCODE) %>%
  summarise(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')

# Summarize crime data by postcode for drug offenses
crime_grouped <- crime_data %>%
  group_by(Postcode) %>%
  summarise(Drug_Offense_Rate = n(), .groups = 'drop')  # Count the number of drug offenses per postcode

# Merge the grouped data
merged_data <- left_join(school_grouped, crime_grouped, by = c("PCODE" = "Postcode"))

View(merged_data)


# Linear regression to analyze the relationship
linear_model <- lm(Drug_Offense_Rate ~ Average_ATT8SCR, data = merged_data)
summary(linear_model)

# Visualization
ggplot(merged_data, aes(x = Average_ATT8SCR, y = Drug_Offense_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "ATT8SCR vs Drug Offense Rate (2022)",
       x = "Average Attainment 8 Score",
       y = "Drug Offense Rate") +
  theme_minimal()


 # Linear Modelling of •	Average Download Speed vs Attainment 8 score 2022
library(tidyverse)
library(readr)


# Load the datasets
broadband_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv")
school_data <- read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv")  


# Prepare broadband data by summarizing average download speed per postcode
broadband_grouped <- broadband_data %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*")) %>%
  group_by(Postcode) %>%
  summarise(Average_Download_Speed = mean(AvgDownloadSpeed, na.rm = TRUE), .groups = 'drop')

# Prepare school data by filtering for the academic years 2021-2022 and 2022-2023 and summarizing by postcode
school_grouped <- school_data %>%
  filter(Academic_Year == "2022-2023") %>%
  mutate(Postcode = str_extract(PCODE, "^[A-Z]+\\d+\\s*\\d*")) %>%
  group_by(Postcode) %>%
  summarise(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = 'drop')

# Merge the datasets on 'Postcode'
merged_data <- left_join(broadband_grouped, school_grouped, by = "Postcode")

View(merged_data)

# Fit a linear model to explore the relationship
linear_model <- lm(Average_ATT8SCR ~ Average_Download_Speed, data = merged_data)
print(summary(linear_model))

# Visualization
ggplot(merged_data, aes(x = Average_Download_Speed, y = Average_ATT8SCR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = " Average Download Speed and ATT8SCR (2022)",
       x = "Average Download Speed (Mbps)",
       y = "Average Attainment 8 Score") +
  theme_minimal()


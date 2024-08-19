
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)



housing_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv")

broadband_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv")
school_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv")
crime_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")


housing_data = housing_data %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*"))

broadband_data = broadband_data %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*"))

school_data = school_data %>%
  mutate(PCODE = str_extract(PCODE, "^[A-Z]+\\d+\\s*\\d*"))

# Filtering for the year 2023
housing_data = housing_data %>% filter(Year == 2023)
crime_data = crime_data %>% filter(Year == 2023)


# Grouping housing by postcode
housing_grouped = housing_data %>%
  group_by(Postcode) %>%
  summarise(Average_Price = mean(as.numeric(Price), na.rm = TRUE))

# Grouping broadband data by postcode
broadband_grouped = broadband_data %>%
  group_by(Postcode) %>%
  summarise(Average_Download_Speed = mean(AvgDownloadSpeed, na.rm = TRUE))

# Grouping school data by postcode
school_grouped = school_data %>%
  group_by(PCODE) %>%
  summarise(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))

# Grouping crime data by postcode
crime_grouped = crime_data %>%
  group_by(Postcode) %>%
  summarise(Total_Crimes = n())

# Merging all datasets by Postcode
merged_data = housing_grouped %>%
  inner_join(broadband_grouped, by = "Postcode") %>%
  inner_join(school_grouped, by = c("Postcode" = "PCODE")) %>%
  inner_join(crime_grouped, by = "Postcode")

View(merged_data)

#Normalizing
merged_data = merged_data %>%
  mutate(
    Normalized_House_Price = (Average_Price - min(Average_Price)) / (max(Average_Price) - min(Average_Price)),
    Normalized_Broadband_Speed = (Average_Download_Speed - min(Average_Download_Speed)) / (max(Average_Download_Speed) - min(Average_Download_Speed)),
    Normalized_School_Grade = (Average_ATT8SCR - min(Average_ATT8SCR)) / (max(Average_ATT8SCR) - min(Average_ATT8SCR)),
    Normalized_Crime_Rate = (Total_Crimes - min(Total_Crimes)) / (max(Total_Crimes) - min(Total_Crimes))
  )

# Defining weights
weights = c(HousePrice = 0.25, BroadbandSpeed = 0.25, SchoolGrade = 0.25, CrimeRate = 0.25)

# Calculating the composite score using the weights
merged_data = merged_data %>%
  mutate(
    Composite_Score = (Normalized_House_Price * weights['HousePrice'] +
                         Normalized_Broadband_Speed * weights['BroadbandSpeed'] +
                         Normalized_School_Grade * weights['SchoolGrade'] +
                         (1 - Normalized_Crime_Rate) * weights['CrimeRate'])
  )


recommended_houses = merged_data %>%
  arrange(desc(Composite_Score))


top_recommendations = recommended_houses %>%
  select(Postcode, Average_Price, Average_Download_Speed, Average_ATT8SCR, Total_Crimes, Composite_Score) %>%
  head(5)

print(top_recommendations)


top_recommendations_with_town = top_recommendations %>%
  left_join(housing_data %>% select(Postcode, `Town/City`), by = "Postcode")

View(top_recommendations_with_town)


print(top_recommendations_with_town)

# Visualizing the top recommendations with a bar chart 
ggplot(top_recommendations_with_town, aes(x = reorder(`Town/City`, -Composite_Score), y = Composite_Score, fill = `Town/City`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Recommended Towns/City by Composite Score",
       x = "Town/City",
       y = "Composite Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
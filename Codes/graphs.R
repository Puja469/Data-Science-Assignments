# Visualization of Housing

library(tidyverse)
library(ggplot2)

cleaned_housing = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv")

# Filtering
housing_2022 = cleaned_housing %>%
  filter(Year == 2022 & County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(Price = as.numeric(Price))

housing_2022 = housing_2022 %>% drop_na(Price)

# Generating the boxplot
ggplot(housing_2022, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Average House Price in 2022 (Boxplot)", x = "County", y = "House Price") +
  theme_minimal() +
  scale_fill_manual(values = c("CORNWALL" = "#FF9999", "CITY OF BRISTOL" = "#9999FF"))

# calculating
average_price_by_town = housing_2022 %>%
  group_by(`Town/City`) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

# Generating the bar chart
ggplot(average_price_by_town, aes(x = reorder(`Town/City`, -Average_Price), y = Average_Price, fill = `Town/City`)) +
  geom_bar(stat = "identity") +
  labs(title = "Average House Price in 2022 by Town/City", x = "Town/City", y = "Average House Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filtering
housing_2020_2023 = cleaned_housing %>%
  filter(Year %in% c(2020, 2021, 2022, 2023) & County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(Price = as.numeric(Price))

#Calculating
average_price_by_year = housing_2020_2023 %>%
  group_by(Year, County) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

# Generating the line chart
ggplot(average_price_by_year, aes(x = Year, y = Average_Price, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Average House Price from 2020 to 2023", x = "Year", y = "Average House Price") +
  theme_minimal()




#Visualization of BroadbandSpeed

library(tidyverse)
library(ggplot2)

cleaned_data_broadband = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv")

# Filtering
filtered_broadband = cleaned_data_broadband %>%
  filter(County %in% c('Cornwall', 'Bristol, City of'))


filtered_broadband$AvgDownloadSpeed = as.numeric(filtered_broadband$AvgDownloadSpeed)
filtered_broadband$MaxDownloadSpeed <- as.numeric(filtered_broadband$MaxDownloadSpeed)

# Generating the boxplot 
ggplot(filtered_broadband, aes(x = County, y = AvgDownloadSpeed, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Download Speeds in Bristol and Cornwall",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))




  
  
  
  bristol_broadband = cleaned_data_broadband %>%
  filter(County == 'Bristol, City of')

# Calculating
bristol_speeds = bristol_broadband %>%
  summarise(
    Average_Download = mean(AvgDownloadSpeed, na.rm = TRUE),
    Maximum_Download = mean(MaxDownloadSpeed, na.rm = TRUE)
  )

# Bar chart 
bristol_speeds_long <- bristol_speeds %>%
  pivot_longer(cols = c(Average_Download, Maximum_Download), 
               names_to = "Speed_Type", 
               values_to = "Speed")

ggplot(bristol_speeds_long, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average and Maximum Download Speeds/ in Bristol",
       x = "Speed Type", 
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Average_Download" = "#F8766D", "Maximum_Download" = "#00BFC4"))

# Filtering
cornwall_broadband = cleaned_data_broadband %>%
  filter(County == 'Cornwall')

# Calculate the average and maximum download speeds for Cornwall
cornwall_speeds = cornwall_broadband %>%
  summarise(
    Average_Download = mean(AvgDownloadSpeed, na.rm = TRUE),
    Maximum_Download = mean(MaxDownloadSpeed, na.rm = TRUE)
  )

# Bar chart 
cornwall_speeds_long = cornwall_speeds %>%
  pivot_longer(cols = c(Average_Download, Maximum_Download), 
               names_to = "Speed_Type", 
               values_to = "Speed")

ggplot(cornwall_speeds_long, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average and Maximum Download Speeds in Cornwall",
       x = "Speed Type", 
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Average_Download" = "#F8766D", "Maximum_Download" = "#00BFC4"))



#Visualization of crimedataset

library(tidyverse)
library(ggplot2)

cleaned_crime_data = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")

# Filtering
drug_offences_2022 = cleaned_crime_data %>%
  filter(`Crime type` == "Drugs" & Year == "2022")


drug_offence_rate = drug_offences_2022 %>%
  group_by(County, Street) %>%
  summarise(Total_Offences = n())

# Generating the boxplot 
ggplot(drug_offence_rate, aes(x = County, y = Total_Offences, fill = County)) +
  geom_boxplot() +
  labs(title = "Drug Offence Rate in Towns/Districts for Cornwall and Bristol (2022)",
       x = "County",
       y = "Total Drug Offences") +
  theme_minimal() +
  scale_fill_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))



library(fmsb)
library(tidyverse)

# Choose a specific month (e.g., April)
selected_month <- "04"  # April

# Filter the data for vehicle crimes in the selected month of 2022
vehicle_crimes <- cleaned_crime_data %>%
  filter(`Crime type` == "Vehicle crime" & Month == selected_month)

# Group by Year to calculate total vehicle crimes and population for each year
vehicle_crime_rate <- vehicle_crimes %>%
  group_by(Year) %>%
  summarise(
    Total_Vehicle_Crimes = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Crime_Rate_Per_10000 = (Total_Vehicle_Crimes / Population) * 10000)

# Prepare the data for radar chart
radar_data <- vehicle_crime_rate %>%
  select(Year, Crime_Rate_Per_10000) %>%
  spread(Year, Crime_Rate_Per_10000)

# Add minimum and maximum values for radar chart scaling
radar_data <- rbind(rep(100, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

# Plot the radar chart
par(mfrow = c(1, 1))
radarchart(radar_data, axistype = 1,
           pcol = c("#F8766D", "#00BFC4"),
           pfcol = c(scales::alpha("#F8766D", 0.5), scales::alpha("#00BFC4", 0.5)),
           plwd = 2,
           title = paste("Vehicle Crime Rate per 10000 People in", selected_month, "2022 by Year"))



selected_month = "04"  

# Filtering
robbery_crimes = cleaned_crime_data %>%
  filter(`Crime type` == "Robbery" & Year == "2022" & Month == selected_month)


robbery_crime_rate = robbery_crimes %>%
  group_by(County) %>%
  summarise(
    Total_Robbery_Crimes = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Crime_Rate_Per_10000 = (Total_Robbery_Crimes / Population) * 10000)


pie_data = robbery_crime_rate %>%
  select(County, Crime_Rate_Per_10000)

# Generating the pie chart 
ggplot(pie_data, aes(x = "", y = Crime_Rate_Per_10000, fill = County)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Robbery Crime Rate per 10000 People in", selected_month, "2022")) +
  theme_void() +
  scale_fill_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))


# Filtering
drug_offenses = cleaned_crime_data %>%
  filter(`Crime type` == "Drugs" & Year == "2022")



drug_offense_rate = drug_offenses %>%
  group_by(County, Month) %>%
  summarise(
    Total_Drug_Offenses = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Drug_Offense_Rate_Per_10000 = (Total_Drug_Offenses / Population) * 10000)

# Generating the line chart 
ggplot(drug_offense_rate, aes(x = Month, y = Drug_Offense_Rate_Per_10000, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Drug Offense Rate per 10000 People in Both Counties (2022)",
       x = "Month", 
       y = "Drug Offense Rate per 10000 People") +
  theme_minimal() +
  scale_color_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))



#Visualization of school


library(tidyverse)
library(ggplot2)


cleaned_schooldata = read_csv("/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv")




# Filtering
data_2021_2022 = cleaned_schooldata %>% 
  filter(Academic_Year == "2021-2022")

# Calculating
average_att8scr = data_2021_2022 %>%
  group_by(County) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(average_att8scr)

# Creating a boxplot 
ggplot(data_2021_2022, aes(x = County, y = ATT8SCR, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot of Attainment 8 Scores (2021-2022)",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()


# Filtering
bristol_2021_2022 = cleaned_schooldata %>% 
  filter(County == "Bristol" & Academic_Year == "2021-2022")

# Calculating
bristol_avg_att8scr = bristol_2021_2022 %>%
  group_by(SCHNAME) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(bristol_avg_att8scr)

# Create a line chart for Bristol schools' average Attainment 8 scores
ggplot(bristol_avg_att8scr, aes(x = SCHNAME, y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Attainment 8 Score for Bristol Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate school names for better readability


# Filtering
cornwall_2021_2022 = cleaned_schooldata %>% 
  filter(County == "Cornwall" & Academic_Year == "2021-2022")

# Calculating 
cornwall_avg_att8scr = cornwall_2021_2022 %>%
  group_by(SCHNAME) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(cornwall_avg_att8scr)

# Creating a line chart 
ggplot(cornwall_avg_att8scr, aes(x = SCHNAME, y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Attainment 8 Score for Cornwall Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
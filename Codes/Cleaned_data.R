library(tidyverse)
library(dplyr)
library(readr)



col_n=c("ID", "Price", "Year", "Postcode", "Property Type", "Old/New", "Duration", "PAON", "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD Category Type")

house_prices_2020 = read_csv("/Users/puja/Downloads/Semester4/Obtain/House Sales/pp-2020.csv", col_names = col_n)
house_prices_2021 = read_csv("/Users/puja/Downloads/Semester4/Obtain/House Sales/pp-2021.csv", col_names = col_n)
house_prices_2022 = read_csv("/Users/puja/Downloads/Semester4/Obtain/House Sales/pp-2022.csv", col_names = col_n)
house_prices_2023 = read_csv("/Users/puja/Downloads/Semester4/Obtain/House Sales/pp-2023.csv", col_names = col_n)

#merging
merged_housing = rbind(house_prices_2020, house_prices_2021, house_prices_2022, house_prices_2023)

#cleaning 
clean_housing = merged_housing %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  select(Price, Year, Postcode, `Town/City`, County) %>%
  mutate(Year = str_trim(substring(Year, 1, 4))) %>%
  filter(!is.na(Price) & !is.na(Year) & !is.na(Postcode) & !is.na(`Town/City`) & !is.na(County)) %>%
  distinct()

view(clean_housing)

dim(clean_housing)

write_csv(clean_housing, "/Users/puja/Downloads/Semester4/Cleaned Data/cleanedhousing.csv")



#Cleaning of postcode_lsoa


library(tidyverse)
library(readr)

# Load the dataset
postcode_lsoa_data = read_csv("/Users/puja/Downloads/Semester4/Obtain/Postcode to LSOA 2.csv")


cleaned_data = postcode_lsoa_data %>%
  distinct()
colnames(cleaned_data)


cleaned_data = cleaned_data %>%
  filter(!is.na(pcds) & !is.na(lsoa11cd) & !is.na(lsoa11nm) & !is.na(ladnm))


cleaned_data = cleaned_data %>%
  mutate(pcds = str_trim(pcds))


cleaned_data = cleaned_data %>%
  select(Postcode = pcds, `LSOA Code` = lsoa11cd, `LSOA Name` = lsoa11nm, County = ladnm)


write_csv(cleaned_data, "/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_postcode_lsoa.csv")


View(cleaned_data)





#filtered_LSOA
library(tidyverse)
library(readr)

postcode_lsoa = read_csv("/Users/puja/Downloads/Semester4/Obtain/Postcode to LSOA 2.csv")
View(postcode_lsoa)

colnames(postcode_lsoa)

# Filtering
filtered_postcode_lsoa = postcode_lsoa %>%
  filter(ladnm %in% c("Bristol, City of", "Cornwall")) 

View(filtered_postcode_lsoa)

output_file_path = "/Users/puja/Downloads/Semester4/Cleaned Data/filtered_postcode_lsoa.csv"

write_csv(filtered_postcode_lsoa, output_file_path)

View(filtered_postcode_lsoa)





#Broadband speed Cleaning

library(tidyverse)
library(dplyr)
library(readr)

performance_data = read_csv("/Users/puja/Downloads/Semester4/Obtain/Broadband Speed/201805_fixed_pc_performance_r03.csv")
coverage_data = read_csv("/Users/puja/Downloads/Semester4/Obtain/Broadband Speed/201809_fixed_pc_coverage_r01.csv")


View(performance_data)
View(coverage_data)

# Performing inner join
combined_data_broadband = performance_data %>%
  inner_join(coverage_data, by = c("postcode_space" = "pcds"))

View(combined_data_broadband)

colnames(combined_data_broadband)

# Performing inner join
combined_data_broadband = filtered_postcode_lsoa %>%
  inner_join(performance_data, by = c("pcds" = "postcode_space"))

View(combined_data_broadband)

# Deleting 
cleaned_data = combined_data_broadband %>%
  select(-c(pcd7, pcd8,doterm, usertype, oa11cd, lsoa11cd,msoa11cd,ladcd, lsoa11nm, msoa11nm, ladnmw,dointr, `postcode area` ,
            postcode))
View(cleaned_data)

print(colnames(cleaned_data))

# Renaming specific columns
cleaned_data = cleaned_data %>%
  rename(
    AvgDownloadSpeed = `Average download speed (Mbit/s)`,
    MinDownloadSpeed = `Minimum download speed (Mbit/s)`,
    MaxDownloadSpeed = `Maximum download speed (Mbit/s)`,
    MedDownloadSpeed = `Median download speed (Mbit/s)`,
    MedUploadSpeed = `Median upload speed (Mbit/s)`,
    AvgUploadSpeed = `Average upload speed (Mbit/s)`,
    MinUploadSpeed = `Minimum upload speed (Mbit/s)`,
    MaxUploadSpeed = `Maximum upload speed (Mbit/s)`,
    AvgDataUsage = `Average data usage (GB)`
  )

cleaned_data_broadband <- cleaned_data %>%
  rename(
    Postcode = pcds,
    County = ladnm
  )

View(cleaned_data_broadband)

output_file_path = "/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv"

write_csv(cleaned_data_broadband, output_file_path)

View(cleaned_data_broadband)

colnames(cleaned_data_broadband)



# Selecting renamed columns and delete all others
cleaned_data_broadband = cleaned_data_broadband %>%
  select(Postcode, County, AvgDownloadSpeed, MinDownloadSpeed, MaxDownloadSpeed, MedUploadSpeed, AvgUploadSpeed, MinUploadSpeed, MaxUploadSpeed, AvgDataUsage)
cleaned_data_broadband = cleaned_data_broadband %>%
  filter(is.finite(AvgDownloadSpeed) & 
           is.finite(MaxDownloadSpeed) &
           is.finite(MedUploadSpeed) &
           is.finite(AvgUploadSpeed))

view(cleaned_data_broadband)

output_file_path = "/Users/puja/Downloads/Semester4/Cleaned Data/cleaned_data_broadband.csv"

write_csv(cleaned_data_broadband, output_file_path)

view(cleaned_data_broadband)




#cleaning crime data
library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)

crime_data = "/Users/puja/Downloads/Semester4/Obtain/crime Dataset"

# Listing all subfolders
folders = list.dirs(crime_data, full.names = TRUE, recursive = FALSE)

cleaned_data_list = list()

# Function to clean individual crime data files
clean_crime_data = function(file_path) {
  
  data = read_csv(file_path, col_types = cols(
    `Crime type` = col_character(),
    Month = col_character(),
    Longitude = col_double(),
    Latitude = col_double(),
    Context = col_character()
  ))
  
  # Cleaning the data
  data_clean = data %>%
    filter(!is.na(`Crime type`)) %>%  
    
    mutate(
      Date = as.Date(paste0(Month, "-01"), format = "%Y-%m-%d")  
    ) %>%
    drop_na(Longitude, Latitude)  %>%
    distinct()
  
  # Removing unnecessary columns 
  data_clean = data_clean %>% 
    arrange(Date) %>%
    select(-`Crime ID`, -`Reported by`, -Longitude, -Latitude, -Location, -`Last outcome category`, -Date, -Context)
  
  
  return(data_clean)
}

# Loop through each folder
for (folder in folders) {
  # Get all CSV files in the folder
  files = list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Applying the cleaning function to each file and store in the list
  for (file in files) {
    cleaned_data = clean_crime_data(file)
    cleaned_data_list[[length(cleaned_data_list) + 1]] = cleaned_data
  }
}


all_cleaned_data = bind_rows(cleaned_data_list)


write_csv(all_cleaned_data, file.path(crime_data, "all_cleaned_crime_data.csv"))

summary(all_cleaned_data)

 View(all_cleaned_data)
 print(colnames(all_cleaned_data))
 
 
 postcode_lsoa = read_csv("/Users/puja/Downloads/Semester4/Obtain/Postcode to LSOA 2.csv")
 clean_lsoa = postcode_lsoa %>%
   select(lsoa11cd, lsoa11nm, ladnm, pcds) %>%
   rename(`LSOA code` = lsoa11cd, Street = lsoa11nm, County = ladnm, Postcode = pcds)
 
 population_data = read_csv("/Users/puja/Downloads/Semester4/Obtain/Population2011_1656567141570.csv")
 population_data = population_data %>%
   mutate(Population2023 = 1.00561255390388033 * Population) %>%
   select(Postcode, Population2023) 
 
 
 
 
 # Filtering
 filtered_lsoa = clean_lsoa %>%
   filter(County %in% c("Bristol, City of", "Cornwall")) %>%
   mutate(Postcode = str_trim(substring(Postcode, 1, 6)))
 
 # Checking for duplicates
 any(duplicated(all_cleaned_data$`LSOA code`))
 any(duplicated(filtered_lsoa$`LSOA code`))
 
 # Removing duplicates 
 cleancrime1 = unique(all_cleaned_data, by = "LSOA code")
 filtered_lsoa1 = unique(filtered_lsoa, by = "LSOA code")
 
 # Merging the datasets 
 final_cleaned_crime = cleancrime1 %>%
   left_join(filtered_lsoa1, by = "LSOA code", relationship = "many-to-many") %>%
   mutate(Year = str_trim(substring(Month, 1, 4))) %>%
   mutate(Month = str_trim(substring(Month, 6, 7))) %>%
   left_join(population_data, by = "Postcode") %>%
   filter(!is.na(`Crime type`) & !is.na(`Month`) & !is.na(`Falls within`) & !is.na(`LSOA code`) &
            !is.na(`Street`) & !is.na(`County`) & !is.na(`Population2023`))
 
 
 View(final_cleaned_crime)
 dim(final_cleaned_crime)
 
 write_csv(final_cleaned_crime, "/Users/puja/Downloads/Semester4/Cleaned Data/Cleaned_merged_crimedata.csv")
 
 
 
 
 
 
 
 
 
 # Cleaning of school
 library(dplyr)
 library(readr)
 library(tidyverse)
 
 bristol21=read_csv("/Users/puja/Downloads/Semester4/Obtain/School Info/Bristol School info/2021-2022/801_ks4final.csv")
 bristol22=read_csv("/Users/puja/Downloads/Semester4/Obtain/School Info/Bristol School info/2022-2023/801_ks4final.csv")
 cornwall21=read_csv("/Users/puja/Downloads/Semester4/Obtain/School Info/Cornwall School info/2021-2022/908_ks4final.csv")
 cornwall22=read_csv("/Users/puja/Downloads/Semester4/Obtain/School Info/Cornwall School info/2022-2023/908_ks4final.csv")
 
 
 # cleaning
 clean_data = function(df) {
   df %>%
     select(SCHNAME, PCODE, ATT8SCR, TOWN) %>% 
     mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%  
     filter(!is.na(ATT8SCR)) %>%  
     filter(!is.na(ATT8SCR) & !is.na(PCODE) & !is.na(SCHNAME) & !is.na(TOWN))  %>%
     distinct()
   
    
 }

 # Cleaning all datasets
 bristol21_clean = clean_data(bristol21)
 bristol22_clean = clean_data(bristol22)
 cornwall21_clean = clean_data(cornwall21)
 cornwall22_clean = clean_data(cornwall22)
 
 # Adding academic year and county identifiers
 bristol21_clean = bristol21_clean %>% mutate(Academic_Year = "2021-2022", County = "Bristol")
 bristol22_clean = bristol22_clean %>% mutate(Academic_Year = "2022-2023", County = "Bristol")
 cornwall21_clean = cornwall21_clean %>% mutate(Academic_Year = "2021-2022", County = "Cornwall")
 cornwall22_clean = cornwall22_clean %>% mutate(Academic_Year = "2022-2023", County = "Cornwall")
 
 # Merging
 combined_schooldata = bind_rows(bristol21_clean, bristol22_clean, cornwall21_clean, cornwall22_clean)
 
  View( combined_schooldata)

  write_csv(combined_schooldata, "/Users/puja/Downloads/Semester4/Cleaned Data/cleanedschool.csv")
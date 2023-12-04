# Load needed packages
library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)
# Import migration data
m1112 <- read_xls("Raw/Migration/1112co.xls", sheet = "County Inflow")
m1213 <- read_xls("Raw/Migration/1213co.xls", sheet = "County Inflow")
m1314 <- read_xls("Raw/Migration/1314co.xls", sheet = "County Inflow")
m1415 <- read_xls("Raw/Migration/1415co.xls", sheet = "County Inflow")
m1516 <- read_xls("Raw/Migration/1516co.xls", sheet = "County Inflow")
m1617 <- read_xls("Raw/Migration/1617co.xls", sheet = "County Inflow")
m1718 <- read_xls("Raw/Migration/1718co.xls", sheet = "County Inflow")
m1819 <- read_xls("Raw/Migration/1819co.xls", sheet = "County Inflow")
m1920 <- read_xls("Raw/Migration/1920co.xls", sheet = "County Inflow")
m2021 <- read_xlsx("Raw/Migration/2021co.xlsx", sheet = "County Inflow")
m1011 <- read_xls("Raw/Migration/1011co.xls")
m0910 <- read_xls("Raw/Migration/0910co.xls")
m0809 <- read_xls("Raw/Migration/0809co.xls")
m0708 <- read_xls("Raw/Migration/0708co.xls")
m0607 <- read_xls("Raw/Migration/0607co.xls")
m0506 <- read_xls("Raw/Migration/0506co.xls")
m0405 <- read_xls("Raw/Migration/0405co.xls")
m0304 <- read_xls("Raw/Migration/0304co.xls")
m0203 <- read_xls("Raw/Migration/0203co.xls")
m0102 <- read_xls("Raw/Migration/0102co.xls")
m0001 <- read_xls("Raw/Migration/0001co.xls")
m9900 <- read_xls("Raw/Migration/9900co.xls")
m9899 <- read_xls("Raw/Migration/9899co.xls")
m9798 <- read_xls("Raw/Migration/9798co.xls")
m9697 <- read_xls("Raw/Migration/9697co.xls")
m9596 <- read_xls("Raw/Migration/9596co.xls")

# Define headers
headers <- c("state_code", "county_code", "state_code_origin", "county_code_origin", "state_origin", "county_origin", "returns", "exemptions", "agi")

# Create Function 1
fxn1 <- function(data, headers, x) {
  data <- data[-(1:5),]
  colnames(data) <- headers
  data <- data %>%
    filter(state_code_origin == 96, county_code_origin == 000) %>%
    select(-1:-5, -7, -9) %>%
    rename(county = county_origin, individuals_inflow = exemptions)
  data$county <- str_replace(data$county, " Total Migration-US and Foreign", "")
data$year <- rep(x, nrow(data))
data <- data %>%
  filter(!(county %in% c("Total Migration-US and Foreign")))
data$county <- str_replace(data$county, " County", "")
data$county <- str_replace(data$county, " Count", "")
data$county <- str_replace(data$county, " Countyo", "")
data$county <- str_replace(data$county, " Coun", "")
data$county <- str_replace(data$county, " Cou", "")
  return(data)
}

# Apply fxn1 to applicable dataframes 
m1112 <- fxn1(m1112, headers, "2012")
m1213 <- fxn1(m1213, headers, "2013")
m1314 <- fxn1(m1314, headers, "2014")
m1415 <- fxn1(m1415, headers, "2015")
m1516 <- fxn1(m1516, headers, "2016")
m1617 <- fxn1(m1617, headers, "2017")
m1718 <- fxn1(m1718, headers, "2018")
m1819 <- fxn1(m1819, headers, "2019")
m1920 <- fxn1(m1920, headers, "2020")
m2021 <- fxn1(m2021, headers, "2021")

# Create Function 2
fxn2 <- function(data, headers, x) {
  data <- data[-(1:7),]
  colnames(data) <- headers
  data <- data %>%
    filter(state_code_origin == 96) %>% 
    select(-1:-5, -7, -9) %>%
    rename(county = county_origin, individuals_inflow = exemptions)
  data$county <- str_replace(data$county, " Tot Mig-US & For", "")
  data$year <- rep(x, nrow(data))
  data <- data %>%
    filter(!(county %in% c("Tot Mig-US & For")))
  data$county <- str_replace(data$county, " County", "")
  data$county <- str_replace(data$county, " Count", "")
  data$county <- str_replace(data$county, " Countyo", "")
  data$county <- str_replace(data$county, " Coun", "")
  data$county <- str_replace(data$county, " Cou", "")
  
  return(data)
}

# Apply fxn2 to applicable dataframes 
m0809 <- fxn2(m0809, headers, "2009")
m0708 <- fxn2(m0708, headers, "2008")
m0607 <- fxn2(m0607, headers, "2007")
m0405 <- fxn2(m0405, headers, "2005")
m0304 <- fxn2(m0304, headers, "2004")
m0203 <- fxn2(m0203, headers, "2003")
m0102 <- fxn2(m0102, headers, "2002")
m0001 <- fxn2(m0001, headers, "2001")

# Create Function 3
fxn3 <- function(data, headers, x) {
  data <- data[-(1:7),]
  colnames(data) <- headers
  data <- data %>%
    filter(state_code_origin == 96) %>% 
    select(-1:-5, -7, -9) %>%
    rename(county = county_origin, individuals_inflow = exemptions)
  data$county <- str_replace(data$county, " Tot Mig-US & For", "")
  data$year <- rep(x, nrow(data))
  data <- data %>%
    filter(!(county %in% c("Total Mig - US & For")))
  data$county <- str_replace(data$county, " County", "")
  data$county <- str_replace(data$county, " Count", "")
  data$county <- str_replace(data$county, " Countyo", "")
  data$county <- str_replace(data$county, " Coun", "")
  data$county <- str_replace(data$county, " Cou", "")
  
  return(data)
}

# Apply fxn3 to applicable dataframes 
m9900 <- fxn3(m9900, headers, "2000")
m9899 <- fxn3(m9899, headers, "1999")
m9798 <- fxn3(m9798, headers, "1998")
m9697 <- fxn3(m9697, headers, "1997")
m9596 <- fxn3(m9596, headers, "1996")
m0910 <- fxn3(m0910, headers, "2010")
m1011 <- fxn3(m1011, headers, "2011")

# Clean 0506
m0506 <- m0506[-(1:7),]
colnames(m0506) <- headers
m0506 <- m0506 %>%
  filter(state_code_origin == 96) %>%
  select(-1:-5, -7, -9) %>%
  rename(county = county_origin, individuals_inflow = exemptions)
m0506$county <- str_replace(m0506$county, " Tot Mig-US", "")
m0506$county <- str_replace(m0506$county, " & F", "")
m0506$county <- str_replace(m0506$county, " &", "")
m0506$county <- str_replace(m0506$county, " County", "")
m0506$county <- str_replace(m0506$county, " Count", "")
m0506$county <- str_replace(m0506$county, " Countyo", "")
m0506$county <- str_replace(m0506$county, " Coun", "")
m0506$county <- str_replace(m0506$county, " Cou", "")
m0506$year <- rep("2006", nrow(m0506))
m0506 <- m0506 %>%
  filter(!(county %in% c("Total Mig - USor")))

# Merge
merged_migration <- rbind(m9596, m9697, m9798, m9899, m9900, m0001, m0102, m0203, m0304, m0405, m0506, m0607, m0708, m0910, m1011, m1112, m1213, m1314, m1415, m1516, m1617, m1718, m1819, m1920, m2021)

# Clean Merged
merged_migration <- merged_migration %>% 
  filter(!(county %in% c("Total Mig - US & For")))
merged_migration <- na.omit(merged_migration)
merged_migration$year <- as.numeric(merged_migration$year)
merged_migration$individuals_inflow <- as.numeric(merged_migration$individuals_inflow)
merged_migration$individuals_inflow[merged_migration$individuals_inflow == "d"] <- NA

# Save as clean file
write.csv(merged_migration, file = "Clean/migration_clean.csv")

# Import clean data
usgs_clean <- read_csv("Clean/usgs_clean.csv")

# Merge and save
merged_data <- left_join(usgs_clean, merged_migration, by = c("county", "year"))
merged_data <- merged_data %>% 
  select(-1)
merged_data$individuals_inflow <- as.numeric(merged_data$individuals_inflow)
save(merged_data, file = "Clean/merged_data.RData")





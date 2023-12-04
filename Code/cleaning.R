## -------------------------------------------------
## Script name: Cleaning
## Purpose of script: Clean USGS and Migration Data
## Author: Brooke Banning + Farzana Sarker
## Date Created: 2023-12-02
## --------------------------------------------------

# Load packages
library(httr)
library(tidyverse)
library(rvest)
library(dplyr)
library(knitr)
library(stringr)
library(pander)
library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

#### Clean USGS Data ####
# Read whole page into r
url_water <- "https://waterdata.usgs.gov/co/nwis/water_use?format=html_table&rdb_compression=value&wu_area=County&wu_year=1985%2C1990%2C1995%2C2000%2C2005%2C2010%2C2015&wu_county=ALL&wu_category=TP%2CPS%2CDO%2CLI%2CLS%2CLA%2CIT%2CIC%2CIG&wu_county_nms=--ALL%2BCounties--&wu_category_nms=Total%2BPopulation%252CPublic%2BSupply%252CDomestic%252CLivestock%252CLivestock%2BStock%252CLivestock%2BAnimal%2BSpecialties%252CIrrigation%252C%2BTotal%252CIrrigation%252C%2BCrop%252CIrrigation%252C%2BGolf%2BCourses"
page <- read_html(url_water)

# Read page based off selector
selector <- "#waterUseHTMLOutput"
water_data <- page |> 
  html_elements(selector)

# Convert the table to data frame
water_data = water_data[[1]] |> html_table()

# Remove empty columns
water_data2 <- water_data |>
  select(where(~any(!is.na(.) & . != "")))

# Remove empty rows
water_data3 <- water_data2 |>
  drop_na()

# Remove duplicate data
water_data4 <- distinct(water_data3)

# Remove Columns which has no values rather only "_" this sign
water_data5 <- water_data4 |>
  select(where(~!all(. == "-")))

# Remove Columns which has no values other than 0
water_data6 <- water_data5 |>
  select(where(~!all(. == "0")))

# Remove Columns which are irrelevant for our work
water_data7 <- water_data6 |>
  select(-c(7, 8, 10:15, 17:24, 26, 27, 28:32, 36, 38, 39, 40:42, 44, 45, 47:54, 57, 58:66, 68, 69, 71:76, 78:90, 93:95, 97:105))

# Identify the primary key
water_data8 <- water_data7 |>
  mutate(surrogate_key = row_number())

# Understand the definition, origin, and units 
head(water_data8)
str(water_data8)
summary(water_data8)

# Replace some of the names in my Dataset Correctly
water_data8 <- water_data8 |>
  rename("Total Population of Area, in Thousands" = "Total Population total population of area, in thousands")

water_data8 <- water_data8 |>
  rename("Irrigation, Total self-supplied withdrawals, fresh, in Mgal/d" = "Irrigation, Total total self-supplied withdrawals, fresh, in Mgal/d" )

water_data8 <- water_data8 %>% 
  select(-1:-3, -16:-20, -23)
water_data8 <- water_data8 %>% 
  select(1:5, 7:9, 13)

water_data8[water_data8 == "na"] <- NA
water_data8[water_data8 == "-"] <- NA
water_data8$`County Name` <- str_replace(water_data8$`County Name`, " County", "")
water_data8 <- water_data8 %>% 
  rename(county = `County Name`, year = Year)

# Print the list of column names
column_names <- colnames(water_data8)
print(column_names)

# Replace some of the names in my Dataset Correctly
water_data8 <- water_data8 |>
  rename("population" = "Total Population of Area, in Thousands")

water_data8 <- water_data8 |>
  rename("public_supply_pop" = "Public Supply total population served, in thousands" )

water_data8 <- water_data8 |>
  rename("withdrawals_public" = "Public Supply total self-supplied withdrawals, fresh, in Mgal/d"  )

water_data8 <- water_data8 |>
  rename("withdrawals_domestic" = "Domestic total self-supplied withdrawals, fresh, in Mgal/d")

water_data8 <- water_data8 |>
  rename("pop_domestic" = "Domestic self-supplied population, in thousands"  )

water_data8 <- water_data8 |>
  rename("deliveries_domestic" = "Domestic deliveries from public supply, in Mgal/d" )

water_data8 <- water_data8 |>
  rename("withdrawals_irrigation" = "Irrigation, Crop total self-supplied withdrawals for crops, fresh, in Mgal/d" )

# Save
write.csv(water_data8, "Clean/usgs_clean.csv")


#### Clean Migration Data ####
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

merged_migration <- merged_migration %>% 
  rename("inflow" = "individuals_inflow")

# Save
write.csv(merged_migration, file = "Clean/migration_clean.csv")

#### Calculate 5 Year Sum #### 
# Calculate 5 year migration averages
sum_2000 <- merged_migration %>%
  filter(year >= 1996 & year <= 2000) %>%
  group_by(county) %>%
  summarize(sum_2000 = sum(inflow, na.rm = TRUE))

sum_2005 <- merged_migration %>%
  filter(year >= 2001 & year <= 2005) %>%
  group_by(county) %>%
  summarize(sum_2005 = sum(inflow, na.rm = TRUE))

sum_2010 <- merged_migration %>%
  filter(year >= 2006 & year <= 2010) %>%
  group_by(county) %>%
  summarize(sum_2010 = sum(inflow, na.rm = TRUE))

sum_2015 <- merged_migration %>%
  filter(year >= 2011 & year <= 2015) %>%
  group_by(county) %>%
  summarize(sum_2015 = sum(inflow, na.rm = TRUE))

# Merge averages
migration_sum <- sum_2000 %>% 
  left_join(sum_2005, by = "county") %>%
  left_join(sum_2010, by = "county") %>%
  left_join(sum_2015, by = "county")

# Reshape
migration_sum_long <- pivot_longer(
  migration_sum,
  cols = starts_with("sum_"),
  names_to = "year",
  values_to = "inflow"
)

# Clean
migration_sum_long$year <- str_replace(migration_sum_long$year, "sum_", "")
migration_sum_long$year <- as.numeric(migration_sum_long$year)

# Save
write.csv(migration_sum_long, file = "Clean/migration_clean_summed.csv")

#### Merge USGS & Migration Data ####
#Import clean data
migration_clean_summed <- read_csv("Clean/migration_clean_summed.csv")
usgs_clean <- read_csv("Clean/usgs_clean.csv")

# Merge and save
merged_data <- left_join(usgs_clean, migration_clean_summed, by = c("county", "year"))
merged_data <- merged_data %>% 
  select(-1)
merged_data$inflow <- as.numeric(merged_data$inflow)
save(merged_data, file = "Clean/merged_data.RData")


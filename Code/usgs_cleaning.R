#Necessary Packages 
library(httr)
library(tidyverse)
library(rvest)
library(dplyr)
library(knitr)
library(stringr)
library(pander)

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

# remove duplicate data
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


#Replace some of the names in my Dataset Correctly

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

#save cleaned data
write.csv(water_data8, "Clean/usgs_clean.csv")

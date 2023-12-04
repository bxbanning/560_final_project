# Import data and packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)
library(httr)
library(tidyverse)
library(rvest)
library(dplyr)
library(knitr)
library(stringr)
library(pander)
library(openxlsx)

load("Clean/merged_data.RData")
migration_clean <- read.csv("Clean/migration_clean.csv")

#### Explore IRS #### 
skim(migration_clean)
# migration vs year
ggplot(migration_clean, aes(x = year, y = individuals_inflow)) +
  geom_smooth()
# migration by year for different counties
ggplot(migration_clean, aes(x = year, y = individuals_inflow, color = county)) +
  geom_line(show.legend = FALSE) +
  xlim(2000, NA) +
  labs(title = "Migration by Year for Colorado Counties",
       x = "Year",
       y = "Migration")

#### Explore USGS ####
# pop served by public supply vs year
ggplot(merged_data, aes(y = `Public Supply total population served, in thousands`, x = year)) +
  geom_smooth()
# public supply withdrawals vs year. 
ggplot(merged_data, aes(y = `Public Supply total self-supplied withdrawals, fresh, in Mgal/d`, x = year)) +
  geom_smooth()
# domestic withdrawals vs year. 
ggplot(merged_data, aes(y = `Domestic total self-supplied withdrawals, fresh, in Mgal/d`, x = year)) +
  geom_smooth()
# irrigation withdrawals vs year. 
ggplot(merged_data, aes(y = `Irrigation, Crop total self-supplied withdrawals for crops, fresh, in Mgal/d`, x = year)) +
  geom_smooth()
# all water withdrawal types vs year
ggplot(merged_data, aes(x = year, group =(county))) +
  geom_line()

# In Progress: Currently working on code to combine these all into one graph.
  

#### Explore Merged ####
skim(merged_data)
## Scatter plots
# migration vs public supply withdrawals
ggplot(merged_data, aes(x = individuals_inflow, y = `Public Supply total self-supplied withdrawals, fresh, in Mgal/d`)) + 
  geom_point()

# migration vs irrigation
ggplot(merged_data, aes(x = individuals_inflow, y = `Irrigation, Crop total self-supplied withdrawals for crops, fresh, in Mgal/d`)) + 
  geom_point()

## Geom Smooth
# migration vs irrigation
ggplot(merged_data, aes(x = individuals_inflow, y = `Irrigation, Crop total self-supplied withdrawals for crops, fresh, in Mgal/d`)) +
  geom_smooth()

# migration vs public supply withdrawals 
ggplot(merged_data, aes(x = individuals_inflow, y = `Public Supply total self-supplied withdrawals, fresh, in Mgal/d`)) +
  geom_smooth()

## Log transforms
# Log transformed relationship between migration and irrigation
merged_data = merged_data |>
  mutate(ln_irrigation = log(`Irrigation, Crop total self-supplied withdrawals for crops, fresh, in Mgal/d`),
         ln_inflow = log(individuals_inflow),
         ln_public_supply = log(`Public Supply total self-supplied withdrawals, fresh, in Mgal/d`))
# ln_migration vs ln_irrigation
ggplot(merged_data, aes(x = ln_inflow, y = ln_irrigation)) + 
  geom_smooth()
# ln_migration vs ln_public_supply
ggplot(merged_data, aes(x = ln_inflow, y = ln_public_supply)) + 
  geom_smooth()



ggplot(migration_clean, aes(x = year, y = inflow, color = county)) +
  geom_area()




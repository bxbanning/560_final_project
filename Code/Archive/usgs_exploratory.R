Water_data <- read.csv("/Users/farzanamahin/Library/CloudStorage/OneDrive-MontanaStateUniversity/560 - Final Project/Clean/usgs_clean_data.csv")

#Variables names are misplaced with . rather than space which I would like to correct here. 
new_colnames <- gsub("\\.", " ", colnames(Water_data))
colnames(Water_data) <- new_colnames



#Get to know our data 

install.packages("skimr")
library(skimr)
install.packages("summarytools")
library(summarytools)

head(water_data9)
View(water_data9)
summary(water_data9)


skim(water_data9)

variable_names <- names(data)
print(variable_names)


# If you want the frequency of multiple variables, you can use the apply function:

selected_vars <- c("PUBLIC SUPPLY TOTAL POPULATION SERVED, IN THOUSANDS", "PUBLIC SUPPLY TOTAL SELF-SUPPLIED WITHDRAWALS, TOTAL, IN MGAL/D", "PUBLIC SUPPLY DELIVERIES TO DOMESTIC, IN MGAL/D", "PUBLIC SUPPLY PER CAPITA USE, IN GALLONS/PERSON/DAY", "DOMESTIC TOTAL SELF-SUPPLIED WITHDRAWALS, FRESH, IN MGAL/D")
# Create a function to get the frequency table for each variable
get_frequency <- function(var) {
  freq_table <- table(water_data9[, var])
  return(freq_table)
}

# Apply the function to the selected variables
freq_tables <- lapply(selected_vars, get_frequency)

# You can access the frequency tables by name
freq_public_supply_total_population <- freq_tables$"PUBLIC SUPPLY TOTAL POPULATION SERVED, IN THOUSANDS"


# Print the frequency tables
print(freq_public_supply_total_population)



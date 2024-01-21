#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/15/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/22/23 model_v08: Reducing model size by 80% for easier testing.
## 12/15/23 model_v07: Enhancing reproducibility of results.
## 12/14/23 model_v06: Factorizing dependent variable vector.
## 12/13/23 model_v05: Enhancing missing value logic and algorithm run-time.
## 12/12/23 model_v04: Identifying and handling missing values.
## 12/11/23 model_v03: Removing redundant and duplicate values.
## 12/09/23 model_v02: Importing home loan data to train on a Random Forest model.
## 12/08/23 model_v01: Does discrimination exist in home loans? 

## Import libraries.
library(randomForest) # Machine learning model.
library(caret) # For splitting the train-test data set.
library(readr) # For reading the CSV file.
library(dplyr)
# library(stringr)


## Read and import the raw CSV data into a data frame.
df_wash16_raw <- read_csv("Washington_State_HDMA-2016.csv")

## View the first 5 rows of the data frame.
# head(df_wash16_raw, n=5)

## View the raw data.
# View(df_wash16_raw)

## Since this analysis only concerns the primary market of fully completed loan
## applications where borrowers and lenders are involved, lets remove the rows
## where the action taken is... "Application withdrawn by applicant,"
## "File closed for incompleteness," and "Loan purchased by the institution."
action_A = "Application withdrawn by applicant"
action_B = "File closed for incompleteness" 
action_C = "Loan purchased by the institution"

## The expression below creates a new data frame that includes all rows from
## the original data frame where the values in the action_taken_name column
## (i.e. df_wash16_raw$action_taken_name) are not equal to either action_A,
## action_B, or action_C. The comma after the condition indicates that all
## columns will be included in the new data frame.
df_wash16 <- df_wash16_raw[ (df_wash16_raw$action_taken_name != action_A) & 
                              (df_wash16_raw$action_taken_name != action_B) &
                              (df_wash16_raw$action_taken_name != action_C), ]

## Confirm that the three rows described above were removed from
## the action_taken_name column in the df_wash16 data frame.
## Also confirm that there are 5 row values instead of 8 in the
## updated data frame column after having removed the two rows.
dplyr::count(df_wash16_raw, action_taken_name, sort = TRUE) # Original
dplyr::count(df_wash16, action_taken_name, sort = TRUE)     # Updated

## Simplify the dependent variable into "Approved" and "Not approved" loans.
df_wash16$loan_status <- ifelse(df_wash16$action_taken_name == "Loan originated", "Approved", "Not approved")

df_wash16$loan_status <- factor(df_wash16$loan_status, levels = c("Approved","Not approved"))


## The exact action taken to deny the loan partitions the dependent information
## which is unnecessary for this analysis. 
## Hence, lets remove this redundant column.
df_wash16$action_taken_name <- NULL

## The census_tract_number and msamd_name are redundant, since the county name
## provides the same location info. Hence, lets remove these redundant columns.
df_wash16$census_tract_number <- NULL
df_wash16$msamd_name <- NULL

## The sequence_number is a unique number for each loan application, and
## The respondent_id is a unique number for who responded to the loan application.
## These are not input variables but are rather generated after a loan application,
## is created. Hence, these columns should be removed so not to increase bias.
df_wash16$sequence_number <- NULL
df_wash16$respondent_id <- NULL

## Lets remove another redundant column.
## The agency_name_abbr column already exists in its un-abbreviated form as agency_name.
df_wash16$agency_abbr <- NULL

## Remove columns that repeat a single constant value in each row.
## They provide zero additional information for the machine learning model.
df_wash16 <- select(df_wash16, -c(state_name,
                                  state_abbr,
                                  as_of_year,
                                  application_date_indicator))

# df_wash16$state_name <- NULL
# df_wash16$state_abbr <- NULL
# df_wash16$as_of_year <- NULL
# df_wash16$application_date_indicator <- NULL

## Since this analysis will be done with a Random Forest model, there cannot be 
## any missing or NA data. By carefully removing empty data, rather than filling
## it in with fault-inducing assumptions, it will result in an unbiased, lower
## variance data set for this analysis. Furthermore, it will help prune elements
## of noise and redundancy from vectors with insufficient data while focusing
## on the full collection of information to unveil insightful correlations.
##
## First, remove columns only if it has greater than 80% missing data.
## Then, remove all rows that have any missing data.
## This would be within columns that have less than 80% missing data.
## Threshold for missing data.
threshold <- 0.80

## For each column, calculate the percentage of missing data.
## The function is.na(df_wash16) creates a data frame where each value is 
## either TRUE or FALSE due to being a missing value or not. 
## The function colSums() counts each column, where TRUE equals 1, for how many
## missing values are in the column.
## The function nrow(df_wash16) counts how many rows are in the data frame, 
## which divides the missing value count in each column in order to find
## the percent of missing values in each column.
missing_percentage <- colSums(is.na(df_wash16)) / nrow(df_wash16)

## Remove columns with more than 80% missing data.
df_wash16 <- df_wash16[, !missing_percentage > threshold]

## Now that all remaining columns have less than 80% of empty values,
## lets keep as much of this pertinent data as possible.
## Use the complete.cases function to filter out and remove the
## rows that have at least one missing value in any column.
df_wash16 <- df_wash16 [complete.cases(df_wash16), ]

# Find column names in df_wash16 that are not in df_wash16_raw
unique_to_df16 <- setdiff(names(df_wash16), names(df_wash16_raw))

# Find column names in df_wash16_raw that are not in df_wash16
unique_to_dfraw <- setdiff(names(df_wash16_raw), names(df_wash16))

## Confirm the removed and added columns based on the original data frame.
print(paste("Column(s) removed from the original data frame:", paste(unique_to_dfraw, collapse = ", ")))
print(paste("Column(s) added to the original data frame:", paste(unique_to_df16, collapse = ", ")))

## Confirm that there are zero total missing values in the updated data frame.
total_na <- sum(is.na(df_wash16))
print(paste("Total missing values in the  data frame:", total_na))

## View the first 5 rows of the updated data frame.
# head(df_wash16, n=5)

## View the updated data frame.
# View(df_wash16)

## Create a CSV of the updated data frame.
# write.csv(df_wash16, "df_wash16_v07.csv")


# # Read the CSV file
# fips <- read.csv("all-geocodes-2016.csv", 
#                  stringsAsFactors = FALSE,
#                  check.names = FALSE)
# 
# # Filter for state code for Washington
# fips <- filter(fips, fips$`State Code (FIPS)` == 53)
# 
# View(fips)
# # Create 'county_code' column
# fips$county_code <- str_c(fips$`State Code (FIPS)`, fips$`County Code (FIPS)`)
# 
# 
# # Drop unnecessary columns
# fips <- select(fips, -c(`Summary Level`, `State Code (FIPS)`, `County Code (FIPS)`, 
#                         `County Subdivision Code (FIPS)`, `Place Code (FIPS)`, 
#                         `Consolidtated City Code (FIPS)`))
# View(fips)
# # Rename columns
# colnames(fips) <- c('county_name', 'county_code')
# View(fips)
# # Merge 'fips' into 'df' (assuming 'df' is already defined)
# df_wash16 <- merge(df_wash16, fips, by = "county_name", all.x = TRUE)
# 
# 
# # Load necessary libraries
# library(dplyr)
# library(ggplot2)
# library(sf)
# 
# # Assuming 'df' is your data frame
# county1 <- df %>%
#   count(county_code) %>%
#   rename('number of loans' = n)
# 
# # Convert county codes and number of loans to lists
# fips <- county1$county_code
# values <- county1$`number of loans`
# 
# # Define colorscale
# colorscale <- c("#141d43","#15425a","#0a6671","#26897d","#67a989","#acc5a6","#e0e1d2",
#                 "#f0dbce","#e4ae98","#d47c6f","#bb4f61","#952b5f","#651656","#330d35")
# 
# # Load shapefile for Washington (replace 'path_to_shapefile' with actual path)
# washington <- st_read('USA_States_(Generalized).xml')
# 
# # Merge 'county1' into 'washington'
# washington <- merge(washington, county1, by.x = 'county_code', by.y = 'county_code')
# 
# # Create choropleth map
# ggplot() +
#   geom_sf(data = washington, aes(fill = `number of loans`)) +
#   scale_fill_gradientn(colors = colorscale) +
#   theme_minimal() +
#   labs(fill = 'Number of Loans by county')

# library(dplyr)
# rm(duplicated_rows)
# duplicated_rows <- dplyr::filter(dplyr::group_by(df_wash16, population), dplyr::n() > 1)
# 
# # Print the duplicated rows
# print(duplicated_rows)
# # Assuming 'duplicated_rows' is your data frame
# sorted_rows <- arrange(duplicated_rows, population)
# 
# # Print the sorted data frame
# View(sorted_rows)

# df_wash16_5var <- df_wash16$applicant_income_000s
# df_wash16_5var <- df_wash16$tract_to_msamd_income
# df_wash16_5var <- df_wash16$loan_amount_000s
# df_wash16_5var <- df_wash16$applicant_sex_name
# df_wash16_5var <- df_wash16$applicant_ethnicity_name
# df_wash16_5var <- df_wash16$loan_status

# Specify the columns you want to keep
columns_to_keep <- c('applicant_income_000s', 'tract_to_msamd_income', 'loan_amount_000s', 'applicant_sex_name', 'applicant_ethnicity_name', 'loan_status')

# Create a new data frame with only the specified columns
df_wash16_5var <- df_wash16[, columns_to_keep]


print(df_wash16_5var)

## Set the random seed for reproducibility of results,
## particularly for the createDataPartition function.
set.seed(0)

## Performs stratified split of the data set.
TrainingIndex <- createDataPartition(df_wash16_5var$loan_status, p=0.8, list = FALSE)
TrainingSet <- df_wash16_5var[TrainingIndex,] # Training data
TestingSet <- df_wash16_5var[-TrainingIndex,] # Test data

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

## Convert the dependent variable vector of the character type to a factor,
## while clarifying the factor levels: "Approved" and "Not approved."
## The factor data type resolves the non-numeric argument to a binary operator 
## that is given from character types (i.e. loan_status elements) for the
## Random Forest machine learning model function.
TrainSet$loan_status <- factor(TrainSet$loan_status, levels = c("Approved","Not approved"))

## Building Random forest model.
model <- randomForest(loan_status ~ ., data = TrainSet, ntree = 500, mtry = 5, importance = TRUE)

## Save model to RDS file.
saveRDS(model, "model.rds")


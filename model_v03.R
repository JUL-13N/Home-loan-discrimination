#####################################
## Julien J. Simons                ##
## December 9, 2023                ##
## https://github.com/JulienSimons  ##
#####################################

## Import libraries.
library(randomForest)
library(caret)
library(readr)

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

df_wash16$loan_status <- ifelse(df_wash16$action_taken_name == "Loan originated", "Approved", "Not approved")

## Remove columns that repeat a single constant value in each row. 
df_wash16$state_name <- NULL
df_wash16$state_abbr <- NULL
df_wash16$as_of_year <- NULL
df_wash16$application_date_indicator <- NULL

## Remove a redundant column.
## The agency_name column already exists in its abbreviated form as agency_abbr.
df_wash16$agency_name <- NULL

## Remove the last five rows of the data frame, because they do not include their
## county location and all of its related financial information.
df_wash16 <- head(df_wash16, -4)

## Since this analysis will be done with a Random Forest model, there cannot be 
## any missing or NA data. Of all remaining columns with at least one single point
## of missing data, they are each missing > 66% of data within their respective
## columns. Therefore, any column with missing data can be safely removed rather
## than filling in the data with assumptions.
## Remove any column x that contains at least one NA or empty value.
#df_wash16 <- df_wash16[, !sapply(df_wash16, function(x) any(is.na(x) | x == ""))]

## Remove rows if the column has < 66% mising data.
## Remove columns if the column has > 66% mising data.
# Threshold for missing data


# # Remove rows with missing data in columns with < 66% missing data
# df_wash16 <- df_wash16[!sapply(df_wash16, function(x) {
#   missing_percentage <- sum(is.na(x)) / length(x)
#   if(missing_percentage < threshold) {
#     return(is.na(x))
#   } else {
#     return(rep(FALSE, length(x)))
#   }
# }), ]
# 
# # Remove columns with > 66% missing data
# df_wash16 <- df_wash16[, !sapply(df_wash16, function(x) sum(is.na(x)) / length(x) > threshold)]

## Identify columns that are completely empty (only contain NA).
#empty_columns <- colSums(is.na(df_wash16)) == nrow(df_wash16)

## Print names of empty columns.
#empty_column_names <- names(df_wash16)[empty_columns]
#print(empty_column_names)

## Remove empty columns from the data frame
#df_wash16 <- df_wash16[, !empty_columns]


## Threshold for missing data.
## Remove rows that have missing data only if its column has < 80% missing data.
## Remove columns only if it has > 80% missing data.
threshold <- 0.80

## Calculate the percentage of missing data for each column.
## The function is.na(df_wash16) creates a data frame where each value is 
## either TRUE or FALSE due to being a missing value or not. 
## The function colSums() counts each column, where TRUE equals 1, for how many
## missing values are in the column.
## The function nrow(df_wash16) counts how many rows are in the data frame, 
## which divides the missing value count in each column in order to find
## the percent of missing values in each column.
missing_percentage <- colSums(is.na(df_wash16)) / nrow(df_wash16)

# Identify columns with less than 80% missing data
cols_to_check <- missing_percentage < threshold

# Remove rows with missing data in columns with less than 66% missing data
rows_to_remove <- apply(is.na(df_wash16[, cols_to_check]), 1, any)

# Subset the data frame to remove these rows
df_wash16 <- df_wash16[!rows_to_remove, ]

# Remove columns with more than 66% missing data
df_wash16 <- df_wash16[, !missing_percentage > threshold]
#
df_wash17 <- df_wash16


# Find column names in df1 that are not in df2
unique_to_df16 <- setdiff(names(df_wash16), names(df_wash17))

# Find column names in df2 that are not in df1
unique_to_df17 <- setdiff(names(df_wash17), names(df_wash16))

# Print the unique column names
print(paste("Columns in df16 not in df17:", paste(unique_to_df16, collapse = ", ")))
print(paste("Columns in df17 not in df16:", paste(unique_to_df17, collapse = ", ")))


#df_wash16$state_name <- NULL
#df_wash16$state_abbr <- NULL
#df_wash16$as_of_year <- NULL
#df_wash16$application_date_indicator <- NULL

# Count total NA values in the DataFrame
total_na <- sum(is.na(df_wash16))
print(paste("Total NA values in the DataFrame:", total_na))


write.csv(df_wash16, "df_wash16_v03.csv")

## Performs stratified random split of the data set
TrainingIndex <- createDataPartition(df_wash16$loan_status, p=0.8, list = FALSE)
TrainingSet <- df_wash16[TrainingIndex,] # Training Set
TestingSet <- df_wash16[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training_v03.csv")
write.csv(TestingSet, "testing_v03.csv")

TrainSet <- read.csv("training_v03.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
# TrainSet$loan_status <- factor(TrainSet$loan_status, levels = c("Approved","Not approved"))

## Building Random forest model
model <- randomForest(loan_status ~ ., data = TrainSet, ntree = 500, mtry = 46, importance = TRUE)

## Save model to RDS file
saveRDS(model, "model_v03.rds")


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

## Simplify the dependent variable into "Approved" and "Not approved" loans.
df_wash16$loan_status <- ifelse(df_wash16$action_taken_name == "Loan originated", "Approved", "Not approved")

## The exact action taken to deny the loan partitions the dependent information
## which is unnecessary for this analysis. 
## Hence, lets remove this redundant column.
df_wash16$action_taken_name <- NULL

## Lets remove another redundant column.
## The agency_name column already exists in its abbreviated form as agency_abbr.
df_wash16$agency_name <- NULL

## Remove columns that repeat a single constant value in each row.
## They provide zero additional information for the machine learning model.
df_wash16$state_name <- NULL
df_wash16$state_abbr <- NULL
df_wash16$as_of_year <- NULL
df_wash16$application_date_indicator <- NULL


## Since this analysis will be done with a Random Forest model, there cannot be 
## any missing or NA data. 
## Remove rows that have missing data only if its column has less than 80% missing data.
## Remove columns only if it has greater than 80% missing data.
## This will not only clean the data of noise without filling in with assummption, but it
## will keep the vast majority of useful information.
## Threshold for missing data.
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

## Identify columns with less than 80% missing data.
cols_to_check <- missing_percentage < threshold

## Mark which rows have missing data in the cols_to_check columns.
## Create a logical vector where each element corresponds to a row in df_wash16.
## Each element in rows_to_remove will be TRUE if that row has any missing
## values in the columns specified by cols_to_check, and FALSE otherwise.
## (The 1 indicates that the function should be applied over the rows.
## If it were 2, it would be applied over the columns.)
rows_to_remove <- apply(is.na(df_wash16[, cols_to_check]), 1, any)

## Subset the data frame to remove these rows.
df_wash16 <- df_wash16[!rows_to_remove, ]

## Remove columns with more than 66% missing data.
df_wash16 <- df_wash16[, !missing_percentage > threshold]


# Find column names in df1 that are not in df2
unique_to_df16 <- setdiff(names(df_wash16), names(df_wash16_raw))

# Find column names in df2 that are not in df1
unique_to_dfraw <- setdiff(names(df_wash16_raw), names(df_wash16))

## Print the unique column names.
## df_wash16 
print(paste("Columns removed from the original data frame:", paste(unique_to_dfraw, collapse = ", ")))
print(paste("Column added to the original data frame:", paste(unique_to_df16, collapse = ", ")))


## Confirm that there are zero total missing (NA) values in the data frame.
total_na <- sum(is.na(df_wash16))
print(paste("Total missing values in the  data frame:", total_na))

write.csv(df_wash16, "df_wash16_v04.csv")

## Performs stratified random split of the data set
TrainingIndex <- createDataPartition(df_wash16$loan_status, p=0.8, list = FALSE)
TrainingSet <- df_wash16[TrainingIndex,] # Training Set
TestingSet <- df_wash16[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training_v04.csv")
write.csv(TestingSet, "testing_v04.csv")

TrainSet <- read.csv("training_v04.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
# TrainSet$loan_status <- factor(TrainSet$loan_status, levels = c("Approved","Not approved"))

## Building Random forest model
model <- randomForest(loan_status ~ ., data = TrainSet, ntree = 500, mtry = 46, importance = TRUE)

## Save model to RDS file
saveRDS(model, "model_v04.rds")


#####################################
## Julien J. Simons                ##
## December 8, 2023                ##
## http://github.com/JulienSimons  ##
#####################################
## 12/08/23 model_v01: Does discrimination exist in home loans? 

## Import libraries.
library(randomForest)
library(caret)

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

## Performs stratified random split of the data set
TrainingIndex <- createDataPartition(df_wash16$loan_status, p=0.8, list = FALSE)
TrainingSet <- df_wash16[TrainingIndex,] # Training Set
TestingSet <- df_wash16[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training_v01.csv")
write.csv(TestingSet, "testing_v01.csv")

TrainSet <- read.csv("training_v01.csv", header = TRUE)
TrainSet <- TrainSet[,-1]
TrainSet$loan_status <- factor(TrainSet$loan_status, levels = c("Approved","Not approved"))

## Building Random forest model
model <- randomForest(action_taken_name ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

## Save model to RDS file
saveRDS(model, "model_v01.rds")


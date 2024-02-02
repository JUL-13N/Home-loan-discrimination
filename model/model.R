#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/31/23             ##
## https://github.com/JulienSimons ##
#####################################
## || LOGS ||
## __________
## 01/31/23 model_v18: Partitioning data to 80% to fit memory constraints of a larger website. 
## 01/30/23 model_v17: Partitioning and factoring data to fit memory constraints. 
## 01/30/23 model_v16: Recording various variable and model plots. 
## 01/29/23 model_v15: Fixing categorical and dependent variable factor levels.
## 01/26/23 model_v14: Updating independent vectors (9) and model parameters.
## 01/23/23 model_v13: Updating independent vectors (13) and cost matrix.
## 01/20/24 model_v12: Enhancing model parameters and pruning poor vectors.
## 01/19/23 model_v11: Increasing model parameters and memory required.
## 12/28/23 model_v08: Factored all character vectors.
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

## || INPUT ||
## ___________
## Read and import the raw CSV data into a data frame.
df_wash16_raw <- read_csv("Washington_State_HDMA-2016.csv")

## View the first 5 rows of the data frame.
# head(df_wash16_raw, n=5)

## View the raw data.
# View(df_wash16_raw)

## || DATA CLEANING ||
## ___________________
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

## Simplify the dependent variable into "Approved" and "Denied" loans.
df_wash16$loan_status <- ifelse(df_wash16$action_taken_name == "Loan originated", "Approved", "Denied")

## This following code is unnecessary, since the dependent variable will have its 
## factor taken in a more efficient manner with other categorical column vectors.
#df_wash16$loan_status <- factor(df_wash16$loan_status, levels = c("Approved","Denied"))

## Must make the interactive web application readily available for any given
## person within 1 GB of website memory limit. The input variables should be
## able to be provided by this person as well. So, lets remove variables that
## any given home loan applicant would not readily be able to provide.
## This will also free up memory for the model to maintain its size for greater
## prevision and accuracy.
df_wash16 <- select(df_wash16, -c(population,
                                  minority_population,
                                  number_of_owner_occupied_units,
                                  number_of_1_to_4_family_units,
                                  property_type_name,
                                  tract_to_msamd_income,
                                  hud_median_family_income,
                                  preapproval_name,
                                  co_applicant_sex_name,
                                  co_applicant_race_name_1,
                                  co_applicant_ethnicity_name))

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

## The name of the agency does not correlate to loan acceptance in this data.
## Lets remove two more redundant columns: agency_name and its abbreviated form.
df_wash16$agency_name <- NULL          
df_wash16$agency_abbr <- NULL

# Write the county names to a file for future reference.
writeLines(df_wash16$county_name, "county_names.txt")

## The name of the county does not correlate to loan acceptance in this data.
## However, it could be used to derive other more relevant information like
## hud_median_family_income.
## Lets remove this redundant column.
df_wash16$county_name <- NULL    

## This vector is actually a dependent vector, not input for the model.
## To confirm this, we see an unusual behavior stemming from this vector.
## This vector is an outlier with approximately x5 greater importance 
## on the prediction model (found via Mean Decrease Accuracy) than
## every other vector as measured in the importance() function.
## Removing this vector will reduce the variance and improve overall accuracy.
df_wash16$purchaser_type_name <- NULL

## hoepa_status_name is another outlier as the least important vector for 
## accurately predicting home loans. 
## Lets remove this irrelevant vector.
df_wash16$hoepa_status_name <- NULL                

## Remove columns that repeat a single constant value in each row.
## They provide zero additional information for the machine learning model.
df_wash16 <- select(df_wash16, -c(state_name,
                                  state_abbr,
                                  as_of_year,
                                  application_date_indicator))

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

## Get the names of columns that hold elements of the character data type.
char_cols <- names(df_wash16)[sapply(df_wash16, is.character)]

## Confirm that all independent vectors are not yet factors.
# f_levels <- sapply(df_wash16, levels)
# print(f_levels)
# print("")

## || TRAINING ENTIRE DATA ||
## __________________________
## If planning to use the entire data set for training, without partition,
## then make sure the factor is taken before training the model on the
## df_wash16 data frame. Otherwise skip this section, because the factor
## will need to be taken after reading the partitioned data. 
##
## Convert character columns to factors, so they can work with the random forest.
# df_wash16[char_cols] <- lapply(df_wash16[char_cols], as.factor)
##
## Ensuring the levels of each factor column are sorted in alphabetical order.
## This step is optional, since as.factor sorts factor levels alphabetically by
## default through the following code: sort(unique(x)), where x is the input vector.
# for (column in char_cols){
#   df_wash16[[column]] <- factor(df_wash16[[column]], levels = sort(levels(df_wash16[[column]])) )
# }

## View the factor levels of the categorical columns of the data frame.
# print(str(df_wash16[char_cols]))

## Or could convert all character-type columns to factor this way:
# df_wash16 <- mutate_if(df_wash16, is.character, as.factor)

## Or could use a recursive function to take the factor and define levels.
# column_names <- colnames(df_wash16[sapply(df_wash16,class)=="factor"])
# if (length(column_names)>0){
#   lapply(column_names, function(col) {
#     ## Replace 'test' with df_wash16 or the data frame at hand.
#     test[col] <<- factor(test[col], levels = unique(df_wash16[,col]))
#   })
# }

## Confirm that all vectors are now factors.
# df_levels <- sapply(df_wash16, levels)
# print(df_levels)
# print("")
# print(class(df_wash16$loan_purpose_name))

## Print the size and class of the data frame vectors, 
## including their factor levels and factor-level encoding.
# print(str(df_wash16))

## Save the complete cleaned, un-partitioned data set to a CSV file (57.6 MB).
# write.csv(df_wash16, "dataset.csv")

## Set the random seed for reproducibility of results,
## particularly for the createDataPartition function.
set.seed(0)

## || DATA PARTITION ||
## ____________________
## Performs stratified split of the data set.
TrainingIndex <- createDataPartition(df_wash16$loan_status, p=0.80, list = FALSE)
TrainingSet <- df_wash16[TrainingIndex,] ## Training data: 80% of data.
TestingSet <- df_wash16[-TrainingIndex,] ## Test data: 20% of data.

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)

## Remove the first (default) index column of the TrainSet data.
TrainSet <- TrainSet[,-1] 

## This step is unnecessary, since the loan_status column and its two elements
## were already defined at the start of the code. Furthermore, all data can be
## factored together at once, rather than separately.
##
## Convert the dependent variable vector of the character type to a factor,
## while clarifying the factor levels: "Approved" and "Denied."
## The factor data type resolves the non-numeric argument to a binary operator 
## that is given from character types (i.e. loan_status elements) for the
## Random Forest machine learning model function.
# TrainSet$loan_status <- factor(TrainSet$loan_status, levels = c("Approved","Denied"))


## || FIXING DATA TYPE ||
## ______________________
## This step is crucial, because reading in data from a CSV file removes all
## previously linked data types. Therefore, all character-type columns must be
## converted to a factor data class.
##
## Without all of the categorical data being factored, the Random Forrest
## model will not work correctly. If only the dependent variable is factored in
## the training data set while all of the categorical vector inputs are factored
## after the input is received in the server, then it would generate the following
## error: New factor levels not present in the training data. The reason being, 
## the training data, after being read from the CSV file, was never fully
## factored. The ordering of the factor levels is not an issue for knowing the
## the factored-level encoding, since when the factor is taken, the factored
## levels (string names) are sorted alphabetically and the factor level encoding
## is by default sequential. Hence, the factored-level sequential encoding is
## sorted according to the alphabetical order of the factored level string names.
## Therefore, both the factored levels and their numeric encoding are sorted
## alphabetically (strings of each level, i.e. a, b, c) and sequentially
## (natural whole digit encoding of each level, i.e. 1, 2, 3).
TrainSet <- mutate_if(TrainSet, is.character, as.factor)

## Print the size and class of the data frame vectors, 
## including their factor levels and factor-level encoding.
print(str(TrainSet))

## Define the cost matrix:
## The cost of a true positive (correctly predicting positive) is 0.
## The cost of a false positive (incorrectly predicting positive) is 1.
## The cost of a true negative (correctly predicting negative) is 0.
## The cost of a false negative (incorrectly predicting negative) is 2.
## This cost matrix will make the model slightly more ambitious about approving
## loans, as the cost of false positives is lower than false negatives.
## You can adjust the values according to your specific needs. Remember, the
## higher the cost, the more the model will try to avoid that type of error.
costMatrix <- matrix(c(0, 2, 1, 0), nrow = 2)

## Print the cost matrix.
# print(costMatrix)

## || MACHINE LEARNING MODEL ||
## ____________________________
## Building Random forest model.
## [ Parameters ]
## ntree: number of trees. Higher number decreases the error rate, but risks 
##        over-fitting the data. Choose an odd number, so it can settle ties.
## mtry: the number of variables considered at each split.
##       There are a total of 24 independent variables. Therefore allowing only
##       9, approximately one third, to be considered for a tree node will help
##       reduce the weight from some redundant or irrelevant vectors.
## cutoff: a sample is classified as positive (i.e. Approved) if the
##         predicted probability of the positive class is greater than 50%.
##         This threshold is statically balanced for the model: 1/k, where k
##         is the number of prediction classes: Approved and Denied.
## parms: incorporating costMatrix explicitly via parms parameter in order to minimize the cost function for misclassification.
model <- randomForest(loan_status ~ ., data = TrainSet, ntree = 333, mtry = 3, importance = TRUE,  cutoff = c(0.50, 0.50), parms = list(loss=costMatrix))


## || ANALYZING RESULTS ||
## _______________________
## Get vector importance.
vector_importance <- importance(model)

## Print and save the importance scores to a CSV file.
print(vector_importance)
write.csv(vector_importance, "importance.csv")

## View the importance of variable vectors.
# varImpPlot(model)


## Plot the error rate of the model with respect to the number of trees.
## Higher number of trees decreases the error rate, but risks over-fitting.
# plot(model)


## View the tree size of the model with respect to the ntree=333 parameter index.
# plot(treesize(model))


## Plotting the income of applicants.
# plot(df_wash16$applicant_income_000s, main = "Home Loan Prediction", ylab = "Applicant Income (Thousands)", xlab = "Applicant Number", sub = "Data from HMDA Washington State 2016")


## Plotting gender frequency bar chart.
# plot(df_wash16$applicant_sex_name, main = "Home Loan Prediction", ylab = "Gender", xlab = "Applicant", sub = "Data from HMDA Washington State 2016")


## Save model to RDS file.
saveRDS(model, "model.rds")

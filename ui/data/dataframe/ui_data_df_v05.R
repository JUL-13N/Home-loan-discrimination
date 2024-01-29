#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/24/24             ##
## https://github.com/JulienSimons ##
#####################################
## 01/25/24 ui_data_df_v05: Transposing and cleaning data with only column names.
## 01/25/24 ui_data_df_v04: Improving row and column positions of data frame.
## 01/24/24 ui_data_df_v03: Writing CSV file from the computation outputs.
## 01/24/24 ui_data_df_v02: Revising data frame queries and data manipulation.
## 01/23/24 ui_data_df_v01: Preserving website memory by pre-computing data for UI.

## The ui.R file will read the created ui_data.csv file to generate the normal 
## distribution range and steps of the numeric sliders and selection menus 
## in order to collect the user input for the machine learning model prediction.

library(shiny)
library(stringr) # For using str_to_title function.
library(plyr) # For using round_any function.

## Training set.
TrainSet <- read.csv("training.csv", header = TRUE)

## Remove numeric placeholder column.
TrainSet <- TrainSet[ , -1]

## The last column of the TrainSet data frame is the dependent variable. 
## It requires no input, since its value will be predicted.
## Remove the last column of the data frame using the ncol() function,
## which gives the number of columns in the data frame.
TrainSet <- TrainSet[ , -ncol(TrainSet)]

## Get the column names.
raw_names <- colnames(TrainSet)

## Create data frame sharing the column vectors (raw names) of the TrainSet.
## From each of the seven rows represents a calculated value for the UI data
## representation schema.
ui_dataframe <- data.frame(matrix(ncol = 7, nrow = (length(raw_names)-1)))

## Name the rows in numeric order for the new data frame.
colnames(ui_dataframe) <- c("column_name",
                            "stylized_name",
                            "numeric_slider",
                            "mean_column", 
                            "min_val",
                            "max_val",
                            "step_size")

## Assign the raw names to the columns of the new data frame.
# colnames(ui_dataframe) <- c(raw_names)

i <- 0
while (i < length(TrainSet)){
  i <- (i+1)
  column_name <- names(TrainSet)[i]
  
  ## Assign the column name value to the (first) column "column_name".
  ui_dataframe[i,"column_name"] <- column_name
  
  ## Replace underscores with spaces using gsub().
  ## Capitalize the first letter of each word using str_to_title().
  stylized_name <- str_to_title(gsub("_", " ", column_name))
  
  ## Assign the stylized name value to the column "stylized_name".
  ui_dataframe[i,"stylized_name"] <- stylized_name
  # print(ui_dataframe)
  
  ## If the column is numeric, assign TRUE to the numeric slider column variable.
  if (is.numeric(TrainSet[[column_name]])) {
    ui_dataframe[i,"numeric_slider"] <- TRUE
    
    ## Calculate mean and standard deviation of column.
    ## This will be used to calculate the normal distribution.
    ## The following slider selection will be based on this distribution.
    mean_column <- mean(TrainSet[[column_name]])
    
    ## Append mean_column value to the "mean_column" column for the i column.
    ui_dataframe[i,"mean_column"] <- mean_column
    
    sd_column <- sd(TrainSet[[column_name]])
    sd_column <- pmax(sd_column, 0)
    
    ## Initial loop conditions for creating a normal distribution for 
    ## the range of numeric slider input values.
    left_sd <- 3
    right_sd <- 3
    counter <- 0
    max_iterations <- 100
    
    ## Prevent negative numbers in the normal distribution by reducing
    ## the standard deviation extension, especially on the left side.
    while (left_sd > 0 && counter < max_iterations) {
      if ((mean_column - (left_sd * sd_column)) < 0) {
        ## The left_sd approaches 0 as it reduces, while the right_sd is
        ## reduced at a slower rate in order to keep a broader input range 
        ## that is still viable and within the range of the initial data set.
        left_sd <- left_sd - 0.30
        right_sd <- right_sd - 0.08
      } else {
        break}
      ## Fail-safe checkpoint for the while loop: counter < max_iterations.
      counter <- counter + 1
    }
    
    ## Calculate min and max values based on mean and standard deviation.
    ## Round to the nearest 10 and 100 digit respectively.
    min_val <- round_any(mean_column - (left_sd * sd_column), 10)
    max_val <- round_any(mean_column + (right_sd * sd_column), 100)
    
    ## Append min_val value to the "min_val" column for the i column.
    ui_dataframe[i,"min_val"] <- min_val
    
    ## Append max_val value to the "max_val" column for the i column.
    ui_dataframe[i,"max_val"] <- max_val
    
    
    ## Calculate input step size based on range of values.
    ## Round to the nearest digit.
    step_size <- round(((max_val - min_val) / 100), digit = 0)
    
    ## Append step_size value to the "step_size" row for the i column.
    ui_dataframe[i,"step_size"] <- step_size
  } 
  
  else {
    ## If the column is not numeric, assign FALSE to the numeric slider column variable.
    ui_dataframe[i,"numeric_slider"] <- FALSE
    
    ## Append empty values to the rows that refer to the numeric columns.
    ui_dataframe[i,"mean_column"] <- NA
    ui_dataframe[i,"min_val"] <- NA 
    ui_dataframe[i,"max_val"] <- NA
    ui_dataframe[i,"step_size"] <- NA
  }
}

## Write the data frame to a csv file.
## There are no assigned row names.
## Do not include the default sequential row digits.
write.table(ui_dataframe, file = "ui_data.csv", sep = ",",
            row.names = FALSE,col.names = TRUE, quote = FALSE)

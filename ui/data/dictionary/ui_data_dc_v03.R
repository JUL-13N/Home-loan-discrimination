#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/25/24             ##
## https://github.com/JulienSimons ##
#####################################
## || LOGS ||
## __________
## 01/25/24 ui_data_dc_v03: Utilizing a dictionary for saving unique vectors.
## 01/25/24 ui_data_dc_v02: Fixing the dimensions of the data frame output.
## 01/25/24 ui_data_dc_v01: Computing unique, non-numeric data for UI selectInput.

## || PURPOSE ||
## _____________
## The ui.R file will read the created ui_data_B.csv file to generate
## unique, non-numeric values for the selection menus in order to
## collect the user input for the machine learning model prediction.

library(shiny)
library(stringr) # For using str_to_title and str_replace functions.

## || INPUT ||
## ___________
## Training set.
TrainSet <- read.csv("training.csv", header = TRUE)

## Remove numeric placeholder column.
TrainSet <- TrainSet[ , -1]

## The last column of the TrainSet data frame is the dependent variable. 
## It requires no input, since its value will be predicted.
## Remove the last column of the data frame using the ncol() function,
## which gives the number of columns in the data frame.
TrainSet <- TrainSet[ , -ncol(TrainSet)]

## || CREATE DICTIONARY ||
## ________________________
## Create an empty dictionary using the column vectors (raw names) of the TrainSet.
## Each of the key values represents a unique selector input for the UI schema.
ui_dictionary <- list()

## Original input data frame row counter.
input_col_n <- 0

## New data frame row counter.
new_col_n <- 0

while (input_col_n < length(TrainSet)){
  input_col_n <- (input_col_n+1)
  
  column_name <- names(TrainSet)[input_col_n]

  ## || QUERY NON-NUMERIC DATA ||
  ## ____________________________
  ## If the column is not numeric, continue.
  if (!is.numeric(TrainSet[[column_name]])) {
    new_col_n <- (new_col_n+1)
    
    ## Assign the column_name value to the (first) input_col_n row of the column "column_name".
    #ui_dataframe_B["column_name",new_col_n] <- column_name
    #ui_dictionary$column_name <- c()
    #ui_dictionary$column_name <- append(ui_dictionary$column_name, column_name)
    #ui_dictionary[new_col_n] <- column_name
    
    ## || STYLING ||
    ## _____________
    ## Replace underscores with spaces using gsub().
    ## Capitalize the first letter of each word using str_to_title().
    stylized_name <- str_to_title(gsub("_", " ", column_name))
    
    ## Check if the last character of the stylized name is a digit.
    ## (input_col_n.e. Applicant Race Name 1, Co Applicant Race Name 1)
    is_digit <- grepl("\\d$", stylized_name)
    
    ## Remove the last character and the space before it if it is a digit.
    ## (input_col_n.e. Applicant Race Name, Co Applicant Race Name)
    stylized_name[is_digit] <- substr(stylized_name[is_digit], 1, nchar(stylized_name[is_digit]) - 2)
    
    ## Replace 000s with (Thousands) at the end of the string as denoted by $.
    ## (input_col_n.e. Loan Amount 000s, Applicant Income 000s)
    stylized_name <- str_replace(stylized_name, "000s$", "(Thousands)")
    
    ## Replace "Co " with "Co-" at the start of the string.
    ## (input_col_n.e. Co Applicant Sex Name, Co Applicant Ethnicity Name)
    stylized_name <- str_replace(stylized_name, "^Co ", "Co-")
    
    ## Assign the stylized name value to the input_col_n row of the column "stylized_name".
    #ui_dataframe_B["stylized_name",new_col_n] <- stylized_name
    #ui_dictionary[["stylized_name"]] <- stylized_name
    #ui_dictionary$column_name <- append(ui_dictionary$column_name, stylized_name)
    #ui_dictionary[new_col_n][1] <- stylized_name
    #ui_dictionary[[column_name]] <- append(ui_dictionary[[column_name]], stylized_name)
    #ui_dictionary[[column_name]] <- c(ui_dictionary[[column_name]], stylized_name)
    # print(ui_dataframe)
    
    ## || SAVE UNIQUE DATA ||
    ## ______________________
    ## From every single value from the given column of the data frame,
    ## append each value so that it appears only once.
    menu_choices <- unique(TrainSet[[column_name]])
    
    ## Use rbind() to combine the original data frame with the new vector.
    #ui_dataframe_B <- rbind(ui_dataframe_B, menu_choices)
    #ui_dataframe_B[new_col_n,"menu_choices"] <- list(menu_choices)
    #ui_dictionary$column_name <- append(ui_dictionary$column_name, menu_choices)
    #ui_dictionary[new_col_n][2] <- menu_choices
    #ui_dictionary[[column_name]] <- list(column_name = column_name, stylized_name = stylized_name, menu_choices = menu_choices)
    #ui_dictionary[[column_name]] <- paste(column_name, stylized_name, paste(menu_choices, collapse = ", "), sep = " ; ")
    #ui_dictionary[[stylized_name]] <- paste(menu_choices, collapse = ", ")
    
    ## Assign the menu_choices name value to the given column_name key.
    ui_dictionary[[column_name]] <- paste(menu_choices, collapse = ", ")
    #print(ui_dictionary)
  }
  else {
    ## || IGNORE NUMERIC DATA ||
    ## _________________________
    ## This is skipping a row of data, because it is numerical.
    ## Update new row counter to -1.
    #new_col_n <- (new_col_n-1)
    
    #if (new_col_n != length(ui_dataframe_B)){
      ## The new data frame was created assuming all input row data will be used.
      ## However this skipped row will not be part of the new data frame.
      ## Remove the last column of the new data frame.
      #ui_dataframe_B <- ui_dataframe_B[-nrow(ui_dataframe_B), ]
      #ui_dataframe_B <- ui_dataframe_B[, -ncol(ui_dataframe_B)]
    }
  }

## || OUTPUT ||
## ____________
## Write the dictionary to a CSV file.
## There are no assigned row names.
## Do not include the default sequential row digits.
#print(ui_dictionary)
#write.table(ui_dictionary, "dictionary.csv", sep = ";", row.names = FALSE, col.names = TRUE)
#write.csv(do.call(rbind,ui_dictionary), "dictionary.csv", row.names = FALSE)
cat("\n")
write.csv(ui_dictionary, "ui_data_B3.csv", row.names = FALSE)

#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/19/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/19/23 v02: Define input selectors or sliders based on the given data.
## 12/15/23 v01: Building an interactive web UI with drop-down input selectors.

library(shiny)
library(stringr) # For the str_to_title function.

## Training set.
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[ , -1]

## The last column of the TrainSet data frame is the dependent variable. 
## It requires no input, since its value will be predicted.
## Remove the last column of the data frame using the ncol() function,
## which gives the number of columns in the data frame.
TrainSet <- TrainSet[ , -ncol(TrainSet)]

## Get the column names.
raw_names <- colnames(TrainSet)

## Replace underscores with spaces using gsub().
## Capitalize the first letter of each word using str_to_title().
stylized_names <- str_to_title(gsub("_", " ", raw_names))

## Set the new column names.
colnames(TrainSet) <- stylized_names


## Get the unique values from a specific column.
## Use these values below for the sliderInput.
# unique <- unique(TrainSet$applicant_race_name_1)

## Print the unique values.
# print(unique)

## Need to remove Loan Sequence Number and Respondent ID...
# labels <- c("County Average Income","County Poupulation","County Minority Population",
#             "County Home Units","County Family Units","Loan Amount","County Median Family Income",
#             "Applicant Income","Loan Sequence Number","Respondent ID","Purchaser Type Name",
#             "Property Type Name","")
# 
# [1] "tract_to_msamd_income"          "population"                    
# [3] "minority_population"            "number_of_owner_occupied_units"
# [5] "number_of_1_to_4_family_units"  "loan_amount_000s"              
# [7] "hud_median_family_income"       "applicant_income_000s"         
# [9] "sequence_number"                "respondent_id"                 
# [11] "purchaser_type_name"            "property_type_name"            
# [13] "preapproval_name"               "owner_occupancy_name"          
# [15] "msamd_name"                     "loan_type_name"                
# [17] "loan_purpose_name"              "lien_status_name"              
# [19] "hoepa_status_name"              "county_name"                   
# [21] "co_applicant_sex_name"          "co_applicant_race_name_1"      
# [23] "co_applicant_ethnicity_name"    "census_tract_number"           
# [25] "applicant_sex_name"             "applicant_race_name_1"         
# [27] "applicant_ethnicity_name"       "agency_abbr"                   
# [29] "loan_status"

pageWithSidebar(

  # Page header
  headerPanel('Home Loan Predictor'),

  sidebarPanel(
    ## Create a selectInput or sliderInput for each column by applying function()
    ## to each element of the vector of column names (names(TrainSet) of the data frame.
    lapply(names(TrainSet), function(column_name) {
      
      if (is.numeric(TrainSet[[column_name]])) {
        sliderInput(
          inputId = column_name,
          label = column_name,
          min = min(TrainSet[[column_name]], na.rm = TRUE),
          max = max(TrainSet[[column_name]], na.rm = TRUE),
          value = median(TrainSet[[column_name]], na.rm = TRUE)
        )
      } else {
        selectInput(
          inputId = column_name,
          label = column_name,
          ## Create a drop-down list with every single value from the columns of
          ## the data frame where each value will appear only once in the list.
          choices = unique(TrainSet[[column_name]])
        )
      }
    })
  ),
  mainPanel(
    # Display the selected values
    verbatimTextOutput("selectedValues")
  )
)
    
    
#     ## Create a selectInput for each column by applying function(column_name) to each
#     ## element of the vector of column names (names(TrainSet) of the data frame.
#     lapply(names(TrainSet), function(column_name) {
#       selectInput(
#         inputId = column_name,
#         label = paste("Select from", column_name),
#         ## Create a drop-down list with every single value from the columns of
#         ## the data frame where each value will appear only once in the list.
#         choices = unique(TrainSet[[column_name]])
#       )
#     })
#   ),
#   mainPanel(
#     # Display the selected values
#     verbatimTextOutput("selectedValues")
#   )
# )




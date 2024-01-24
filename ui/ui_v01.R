#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/15/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/15/23 ui_v01: Building interactive web UI with drop-down input selectors.

library(shiny)

## Training set.
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

## Get the unique values from a specific column.
## Use these values below for the sliderInput.
# unique <- unique(TrainSet$applicant_race_name_1)

## Print the unique values.
# print(unique)

pageWithSidebar(

  ## Page header.
  headerPanel('Iris Predictor'),

  ## Input values.
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    selectInput(inputId = "applicant_sex_name", label = "Gender", 
                choices = c(\"Male\", \"Female\", \"Prefer not to say\", \"Information not provided by applicant in mail, Internet, or telephone application\"))
    selectInput(inputId = "applicant_race_name", label = "Race", 
            choices = c(\"White\", \"Black or African American\", \"Asian\", \"Native Hawaiian or Other Pacific Islander\", \"American Indian or Alaska Native\", \"Not applicable\", \"Prefer not to say\", \"Information not provided by applicant in mail, Internet, or telephone application\"))
    selectInput(inputId = "applicant_ethnicity_name", label = "Ethnicity", 
            choices = c(\"Hispanic or Latino\", \"Not Hispanic or Latino\", \"Prefer not to say\", \"Information not provided by applicant in mail, Internet, or telephone application\"))
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),

  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output text box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table

  )
)




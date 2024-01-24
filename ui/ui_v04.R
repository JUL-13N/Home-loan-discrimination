#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/18/24             ##
## https://github.com/JulienSimons ##
#####################################
## 01/18/24 ui_v04: Normalizing distribution of numeric slider selection.
## 12/20/23 ui_v03: Pairing numeric slider labels with their stylized name.
## 12/19/23 ui_v02: Define input selectors or sliders based on the given data.
## 12/15/23 ui_v01: Building an interactive web UI with drop-down input selectors.

library(shiny)
library(stringr) # For using str_to_title function.

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

## Replace underscores with spaces using gsub().
## Capitalize the first letter of each word using str_to_title().
stylized_name_list <- str_to_title(gsub("_", " ", raw_names))

## pagewithSidebar or fluidPage
fluidPage(

  # Page header
  headerPanel('Home Loan Predictor'),

  sidebarPanel(
    ## Create a selectInput or sliderInput for each column by applying function()
    ## to each element of the vector of column names (names(TrainSet) of the data frame.
    lapply(seq_along(TrainSet), function(i) {
      column_name <- names(TrainSet)[i]
      stylized_name <- stylized_name_list[i]
      
      ## If the column is numeric, use the numeric slider input.
      if (is.numeric(TrainSet[[column_name]])) {
        
        ## Calculate mean and standard deviation of column.
        ## This will be used to calculate the normal distribution.
        ## The following slider selection will be based on this distribution.
        mean_column <- mean(TrainSet[[column_name]])
        sd_column <- sd(TrainSet[[column_name]])
        sd_column <- pmax(sd_column, 0)
        
        ## Initial loop conditions.
        left_sd <- 3
        right_sd <- 3
        counter <- 0
        max_iterations <- 100

        ## Prevent negative numbers in the normal distribution by reducing
        ## the standard deviation extention, especially on the left side.
        while (left_sd > 0 && counter < max_iterations) {
          if ((mean_column - (left_sd * sd_column)) < 0) {
            ## The left_sd approaches 0 as it reduces, while the right_sd is
            ## reduced at a slower rate in order to keep a broader input range 
            ## that is still viable and within the range of the initial dataset.
            left_sd <- left_sd - 0.20
            right_sd <- right_sd - 0.03
          } else {
            break}
          ## Fail-safe checkpoint for the while loop: counter < max_iterations.
          counter <- counter + 1
        }

        ## Calculate min and max values based on mean and standard deviation.
        min_val <- round(mean_column - (left_sd * sd_column), digits = 0)
        max_val <- round(mean_column + (right_sd * sd_column), digits = 0)
        
        ## Calculate step size based on range of values.
        step_size <- round((max_val - min_val) / 100, digits = 0)
        
        ## Create slider input with normal distribution of values.
        sliderInput(
          inputId = column_name,
          label = stylized_name,
          min = min_val,
          max = max_val,
          value = mean_column, ## The initial value of the numeric slider.
          step = step_size, ## This is the interval between each value on the slider.
          round = TRUE ## Round all values to the nearest integer.
        )
      } else {
        selectInput(
          inputId = column_name,
          label = stylized_name,
          ## Create a drop-down list with every single value from the columns of
          ## the data frame where each value will appear only once in the list.
          choices = unique(TrainSet[[column_name]])
        )
      }
    }),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

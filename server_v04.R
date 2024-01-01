#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/21/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/21/23 server_v04: Fixing vector ordering of reactive value input.
## 12/20/23 server_v03: Creating reactive input data frame for model prediction.
## 12/19/23 server_v02: Running server functions on input data.
## 12/18/23 server_v01: Building back-end server functions.

library(shiny)
library(data.table)
library(randomForest)

## Read in the Random Forest model.
model <- readRDS("model.rds")

shinyServer(function(input, output, session) {
  #print(input[[column_name]])
  datasetInput <- reactive({
    ##lapply(input, function(column_name) {
      ##input[[column_name]]
    ##})
    # Create a dataframe using the input values
    input <- reactiveValuesToList(input)
    
    ## Remove first two values of the list.
    input <- input[-1]
    
    print(input)
    #lapply(seq_along(input), function(i) {
      #column_name <- input[i]
    df <- data.frame(
      lapply(seq_along(input), function(i) {
        input[i]
        
      })
    )
    View(df)
    loan_status <- 0
    df <- rbind(df, loan_status)
    input <- transpose(df)

    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)

    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)

    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)

  })

  ## Print status/output text box.
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })

  ## Create prediction result table.
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })

})


#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/18/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/18/23 server_v07: Building back-end server functions.

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
    input_list <- reactiveValuesToList(input)
    
    ## Remove first two values of the list.
    input_list <- input_list[-1]
    
    print(input_list)
    
    ordered_input <- c(input_list[-(1:2)], input_list[1:2])
    
    #lapply(seq_along(input), function(i) {
      #column_name <- input[i]
    df_input <- data.frame(
      lapply(seq_along(ordered_input), function(i) {
        ordered_input[c(i)]
        
      })
    )
    
    #loan_status <- 0
    #df <- rbind(df, loan_status)
    #input <- transpose(df)
    #df$loan_status <- as.factor(0)
    
    ## Create an empty row and column for the dependent
    ## variable that the Random Forest model will predict.
    df_status <- data.frame(loan_status = 0)
    
    ## View the empty vector.
    # print(df_status$loan_status)
    
    ## View the data frame.
    # View(df_status)
    
    ## Factorize and define the factor levels for the dependent variable.
    ## The numeric factor of the character strings works with the model.
    ## Add this dependent variable vector to the main input data frame.
    df_input$loan_status <- factor(df_status$loan_status, levels = c("Approved","Not approved"))
    
    ## View the data frame.
    View(df_input)
    #print(df$tract_to_msamd_income)
    #write.table(df,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)

    #test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)

    Output <- data.frame(Prediction=predict(model,df_input), round(predict(model,df_input,type="prob"), 3))
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


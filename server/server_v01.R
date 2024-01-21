#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/18/23             ##
## https://github.com/JulienSimons ##
#####################################
## 12/18/23 server_v01: Building back-end server functions.

library(shiny)
library(data.table)
library(randomForest)

## Read in the Random Forest model.
model <- readRDS("model.rds")

shinyServer(function(input, output, session) {
  
  datasetInput <- reactive({
    
    df <- data.frame(
      Name = c(colnames(input)),
      Value = as.character(c(input$Name)),
      stringsAsFactors = FALSE)
    
    loan_status <- 0
    df <- rbind(df, as.factor(loan_status))
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


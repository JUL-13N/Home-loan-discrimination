#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/20/24             ##
## https://github.com/JulienSimons ##
#####################################
## 01/20/24 server_v07: Comparing change in loan approval rates.
## 01/19/24 server_v06: Accepting dynamic vector quantities.
## 12/22/23 server_v05: Fixing dependent variable data structure.
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
    
    # Create a data frame using the input values
    input_list <- reactiveValuesToList(input)
    
    ## Remove first two values of the list.
    input_list <- input_list[-1]
    
    #print(input_list)
    
    ordered_input <- c(input_list[-(1:12)], input_list[1:12])
    print(ordered_input)
    
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
    # View(df_input)
    #print(df$tract_to_msamd_income)
    #write.table(df,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)

    #test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)

    Output <- data.frame(Prediction=predict(model,df_input), round(predict(model,df_input,type="prob"), 3))
    cat("\n")
    print(Output)
    cat("\n")
    approval_percent <- round(((Output$Approved)*100),2)
    average_percent <- 43.4
    percent_diff <- round(((approval_percent/average_percent)-1)*100,2)
    if (percent_diff > 0) {
      print(paste("Home Loan Application: ", approval_percent,"% chance of approval. You have ", percent_diff,"% greater chance of approval than the average applicant.", sep=""))
    } 
    else if (percent_diff == 0) {
      print(paste("Home Loan Application: ", approval_percent,"% chance of approval. You have the same chance for approval as the average applicant.", sep=""))
    }
    else if (percent_diff < 0) {
      print(paste("Home Loan Application: ", approval_percent,"% chance of approval. You have ", abs(percent_diff),"% less chance of approval than the average applicant.", sep=""))
    }
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
      ## Isolating the datasetInput reactive expression from other reactive
      ## expressions in the app. This ensures that datasetInput is only
      ## evaluated when the submit button is clicked, and not when other inputs
      ## in the app change.
      isolate(datasetInput())
    }
  })

})


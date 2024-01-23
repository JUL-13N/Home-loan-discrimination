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
    input_list <- reactiveValuesToList(input)
    
    ## Remove first two values of the list.
    input_list <- input_list[-1]
    
    print(input_list)
    
    ordered_input <- c(input_list[-(1:18)], input_list[1:18])
    
    #lapply(seq_along(input), function(i) {
      #column_name <- input[i]
    df <- data.frame(
      lapply(seq_along(ordered_input), function(i) {
        ordered_input[c(i)]
        
      })
    )
    
    # df <- data.frame(
    #   Name = c("tract_to_msamd_income",
    #            "population",
    #            "minority_population",
    #            "number_of_owner_occupied_units",
    #            "number_of_1_to_4_family_units",
    #            "loan_amount_000s",
    #            "hud_median_family_income",
    #            "applicant_income_000s",
    #            "sequence_number",
    #            "respondent_id",
    #            "purchaser_type_name",
    #            "property_type_name",
    #            "preapproval_name",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s",
    #            "applicant_income_000s"),
    #   Value = as.character(c(input$tract_to_msamd_income,
    #                          input$population,
    #                          input$minority_population,
    #                          input$number_of_owner_occupied_units,
    #                          input$number_of_1_to_4_family_units,
    #                          input$loan_amount_000s,
    #                          input$hud_median_family_income,
    #                          input$applicant_income_000s,
    #                          input$sequence_number,
    #                          input$respondent_id,
    #                          input$purchaser_type_name,
    #                          input$property_type_name,
    #                          input$preapproval_name,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s,
    #                          input$applicant_income_000s)),
    #   stringsAsFactors = FALSE)
    
    
    #loan_status <- 0
    #df <- rbind(df, loan_status)
    #input <- transpose(df)
    df$loan_status <- as.factor(0)
    View(df)
    print(df$tract_to_msamd_income)
    #write.table(df,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)

    #test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)

    Output <- data.frame(Prediction=predict(model,df), round(predict(model,df,type="prob"), 3))
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


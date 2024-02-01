#####################################
## Julien J. Simons                ##
## 12/08/23 - 01/31/24             ##
## https://github.com/JulienSimons ##
#####################################
## || LOGS ||
## __________
## 01/31/24 13: Analyzing vector importance and algorithmic discrimination.
## 01/30/24 12: Creating a framework of new page input for plotting pre-computed data.
## 01/30/24 11: Fixing input data type and categorical factor options.
## 01/29/24 10: Binding alphabetically ordered vector factors and levels.
## 01/28/24 09: Expanding output print statements and tables.
## 01/23/24 server_v08: Cleaning memory storage of previous input values. 
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
library(ggplot2)
library(dplyr)
library(reshape2)

## || INPUT ||
## ___________
## Read in the dictionary of unique categorical vectors and elements.
ui_dictionary <- read.csv("ui_data_dc.csv", stringsAsFactors = FALSE)

## Read in the Random Forest model.
model <- readRDS("model.rds")

## Read the vector importance file, generated from model.R
vecImp <- read.csv("importance.csv") #, row.names = 1


## || SERVER FRAMEWORK ||
## ______________________
shinyServer(function(input, output, session) {

  ## || INPUT DATA MANIPULATION ||
  ## _____________________________
  datasetInput <- reactive({

    ## Create a data frame using the input values.
    raw_input_list <- reactiveValuesToList(input)
    
    ## Remove the first (default info) value of the list.
    input_list <- raw_input_list[-1]
    
    ## Order to the input list, so that the plot input vector is at the start,
    ## followed by the two numeric inputs, and in order of the training data.
    ordered_list <- c(input_list[-(1:7)], input_list[1:7])
    
    ## Remove the plot input vector. This will be used in a different section.
    ordered_list <- ordered_list[-1]
    # print(ordered_list)
    # print(names(ordered_list))
    
    ## Create a data frame from the ui input.
    ui_input <- data.frame(
      Name = names(ordered_list),
      Value = unlist(ordered_list),
      stringsAsFactors = FALSE)
    
    ## Get the names of columns that hold elements of the character data type.
    # char_cols <- names(ui_input)[sapply(ui_input, is.character)] # <- WORKS
    # print(char_cols)
    
    ## Convert character columns to factors, so they can work with the random forest.
    # ui_input[char_cols] <- lapply(ui_input[char_cols], as.factor) # <- WORKS
    # print(class(ui_input))
    # print("")
    # print(ui_input)
    # print(class(ui_input$loan_purpose_name))
    # print(str(ui_input))
    # ordered_input <- c(input_list[-(1:12)], input_list[1:12])
    # print(ordered_input)
    # print(str(ui_input))
    
    ## Recursive function to create a data frame from alphabetically-ordered input.
    # ui_input <- data.frame(
    #   lapply(seq_along(ordered_input), function(i) {
    #     ordered_input[c(i)]
    #   })
    # )
    
    ## Create an empty row and column for the dependent
    ## variable that the Random Forest model will predict.
    # df_status <- data.frame(loan_status = 0)

    ## Create the dependent variable vector.
    loan_status <- "loan_status"
    
    ## Attach the dependent variable to the input data frame.
    ui_input <- rbind(ui_input,loan_status)
    
    ## Transpose the data frame, so that the column names are the vectors
    ## and the elements below represent their values, like the training data.
    transposed_input <- transpose(ui_input)
    
    ## This step is unnecessary, since the dependent variable: loan_status is
    ## not part of the input data. Its factor levels are already recorded in
    ## the model.rds, so that it will be predicted accordingly.
    # transposed_input$loan_status <- factor(transposed_input$loan_status, levels = c("Approved","Denied"))
    
    ## Write the input data frame to a CSV file for reference, removing
    ## the default placeholder row and column names. These placeholders
    ## are unnecessary for efficiently reading the data.
    write.table(transposed_input,"input.csv",sep=",", quote=TRUE, 
                row.names = FALSE, col.names = FALSE)

    ## Create a data frame from the input.csv file, so it can be used as final
    ## input for the machine learning Random Forest model prediction.
    model_input <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    ## View the empty vector.
    # print(df_status$loan_status)
    
    ## View the data frame.
    # View(df_status)
    
    ## This step is unnecessary, since the dependent variable: loan_status is not
    ## a part of the input data. Its factor levels are already recorded in the
    ## model.rds, so that it will be predicted accordingly.
    ##
    ## Factorize and define the factor levels for the dependent variable.
    ## The numeric factor of the character strings works with the model.
    ## Add this dependent variable vector to the main input data frame.
    # model_input$loan_status <- factor(df_status$loan_status, levels = c("Approved","Denied"))
    
    ## Take all character columns from the input data frame.
    char_cols <- names(model_input)[sapply(model_input, is.character)]
    
    ## This disregards the dependent vector: loan_status, since it is not a
    ## part of the input data. Its factor levels are already recorded in the
    ## model.rds, so that it will be predicted accordingly.
    char_cols <- char_cols[1:7]
    # print(ui_dictionary[char_cols[1]])
    # for(b in ui_dictionary[char_cols[1]]){print(b)}
    col_names <- (colnames(ui_dictionary))

    ## Take the factor of all character columns with the factor levels defined
    ## by their indexed, pre-computed unique options from ui_data_dc_v07.R as
    ## noted in the ui_dictionary.csv file.
    ## 
    ## It is important to note that this clearly defines the factor levels
    ## and its encoded ordering, so to be recorded in the same way as the
    ## training data used to build the Random Forest model.R file.
    i = 0
    for (vector in ui_dictionary[char_cols])
      {
      i <- i+1
      string_choices <- strsplit(vector, "\\| ")[[1]]
      string_choices <- sort(string_choices)
      model_input[[col_names[i]]] <- factor(model_input[[col_names[i]]], 
                                            levels = string_choices)
      }
    
    ## Instead of using a 'for loop', could use a
    ## recursive function to increase efficiency.
    # lapply(seq_along(char_cols), function(i) {
    #   string <- as.character(ui_dictionary[char_cols[i]])
    #   string_choices <- strsplit(string, "\\| ")[[1]]
    #   model_input[[char_cols]] <- factor(model_input[[char_cols]], levels = string_choices)
    # })
    
    ## Could take the factor of each vector individually, but the redundancy
    ## is unnecessary when it is possible to do so in a function for each
    ## vector automatically based on pre-computed and recorded data.
    # model_input$owner_occupancy_name <- factor(model_input$owner_occupancy_name, levels = c("")
    
    ## View the data frame.
    ## Remember to capitalize the V in View!
    # View(ui_input) 
    # print(class(model_input))
    # print(model_input)
    
    ## The factor levels and numeric encoding of the selected input data
    ## frame for the new model prediction can be compared with the data
    ## frame used to train the model.
    ## 
    ## The categorical vectors MUST be factored the same in the training
    ## and input testing data frames in order to share the same string
    ## content, encoding, and ordering of the factor levels.
    ## Refer to model.R for more analysis.
    # print(str(model_input))
    
    ## Clear session memory of unused R objects.
    # gc()
    
    ## Create a data frame from the output prediction of the model.
    df <- data.frame(Prediction=predict(model,model_input),
                     round(predict(model,model_input,type="prob"), 4))
    
    ## || MODEL ANALYSIS ||
    ## ____________________
    output$summary <- renderText({
      
      ## If the user clicks the submit button, making the object equal TRUE.
      if (input$submitbutton>0) {

        ## Use the df data frame within the server logic.
        summary(df)
        print(paste("Prediction: ", round(((df$Approved)*100),2),
                    "% chance of approval.", sep=""))
      }
    })
    
    output$average <- renderText({
      if (input$submitbutton>0) {
        
        ## Use the df data frame within the server logic.
        summary(df)
        approval_percent <- round(((df$Approved)*100),2)
        average_percent <- 51.05
        percent_diff <- round(((approval_percent/average_percent)-1)*100,2)
        gc()
        if (percent_diff > 0) {
          print(paste("You have +",
                      percent_diff,"% chance of loan approval compared to the average applicant.",
                      sep=""))
        }
        else if (percent_diff == 0) {
          print(paste("You have equal chance of loan approval compared to the average applicant.",
                      sep=""))
        }
        else if (percent_diff < 0) {
          print(paste("You have -", abs(percent_diff),
                      "% chance of loan approval compared to the average applicant.", 
                      sep=""))
        }
      }
    })
    
    print(df)
  })

  ## || APPLICATION STATUS ||
  ## ________________________
  ## Print status text box.
  output$contents <- renderPrint({ 
    if (input$submitbutton>0) { 
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })

  ## || MODEL RESULTS TABLE ||
  ## _________________________
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
  
  ## || PLOTTING ||
  ## ______________
  ## Render plot in the new page defined by the UI.
  ## Use UI input to determine which data to plot.
  output$vecImpPlot <- renderPlot({ 
    
    ## Create a data frame using the input values.
    raw_input_list <- reactiveValuesToList(input)
    
    ## Remove the first (default info) value of the list.
    input_list <- raw_input_list[-1]
    
    ## Order to the input list, so that the plot input vector is at the start.
    ordered_list <- c(input_list[-(1:7)], input_list[1:7])
    
    ## Use the plot input vector.
    plot_input <- ordered_list[1]
    # print(plot_input)
    
    ## Sort the data frame by the selected column.
    # vecImp <- vecImp[order(vecImp[[plot_input$column]]), ]
    # vecImp <- order(vecImp[[plot_input$column]])
    
    ## Sort the input data frame.
    ## margin = 2 refers to sorting of the column values.
    # vecImp <- apply(varImp,2, sort)
    # vecImp <- sort(varImp[plot_input$column])
    # vecImp_sorted <- vecImp
    
    ## Iterate through each column and sort the values in ascending order.
    # vecImp_sorted[plot_input$column] <- vecImp[plot_input$column] %>%
    #  mutate(across(everything(), sort))
    
    ## Create a new data frame where the input-selected column is sorted
    ## by its values. plot_input$column represents the column of vecImp.
    ## vecImp[plot_input$column] represents the values of vecImp.
    ## vecImp is the imported data frame of the vector importance,
    ## generated by the model.R file.
    vecImp_sorted <- vecImp %>%
      arrange(plot_input$column, vecImp[plot_input$column])
    
    # row.names(vecImp_sorted) <- vecImp[,1]
    # vecImp_sorted[,1] <- vecImp[,1]
    # row.names(vecImp_sorted) <- vecImp[,1]
    # print(row.names(vecImp_sorted))
    
    # vecImp_sorted[plot_input$column] <- vecImp[plot_input$column] %>%
    #  mutate(across(everything(), sort))
    # print(row.names(vecImp))
    # print(column)
    # vecImp_sorted <- vecImp %>%
    #  arrange(match(plot_input$metric, vecImp[plot_input$column]))
    
    ## Use ggplot2 to create a horizontal bar plot. Requires ggplot2 pacakge.
    ## Although it requires an additional package, it looks better than plot().
    ggplot(vecImp_sorted, 
           ## The line below uses the row names of vecImp_sorted to create
           ## the x-axis sub-label that is tried to their original (x,y)
           ## element value in acending order.
           aes(x = reorder(vecImp_sorted[,1], .data[[plot_input$column]]),
               y = .data[[plot_input$column]])) + 
      geom_col(fill = "#336699") +
      geom_point(color = "black") +
      ## Define the primary x-axis and y-axis labels.
      ## coord_flip flips the x and y-axis, similar to transposing a data frame.
      ## The x-axis therefore is defined by plot_input$column,
      ## which is the input-selected column.
      labs(x = "Feature Vectors", y = plot_input$column) +
      coord_flip() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    
    ## Or could use plot() intead of ggplot2 to create a horizontal bar plot.
    ## This does not require an additional package like ggplot2 does,
    ## so by using the plot() function it can save website space.
    # par(mar = c(5, 10, 4, 2), las = 2) # Adjust margins and orientation
    # plot(varImp[[plot_input$metric]], type = "h", xlab = plot_input$metric, ylab = "Row names")
    # points(varImp[[plot_input$metric]], pch = 16) # Add circles to the bars
    ## Add text labels to the bars.
    # text(x = varImp[[plot_input$metric]], y = 1:nrow(varImp), labels = row.names(varImp), pos = 4)
  })

  output$analysis <- renderText({ 
    
    ## Create a data frame using the input values.
    raw_input_list <- reactiveValuesToList(input)
    
    ## Remove the first (default info) value of the list.
    input_list <- raw_input_list[-1]
    
    ## Order to the input list, so that the plot input vector is at the start.
    ordered_list <- c(input_list[-(1:7)], input_list[1:7])
    
    ## Use the plot input vector.
    plot_input <- ordered_list[1]
    # print(plot_input$column)

    ## Get the total weight (addition) of all vector importance values within 
    ## the input-selected column (i.e. "MeanDecreaseAccuracy").
    ## This will also assign the unique weight value to each of
    ## the indicated column vectors to be used for analysis.
    row_index = 0
    total_weight = 0
    for (col_values in vecImp[plot_input$column]){
      for (weight in col_values){
        row_index <- row_index + 1
        total_weight <- total_weight + weight
        gc()
        if (row_index == 1){
          loan_amount_000s_val <- weight
        }
        else if (row_index == 2){
          applicant_income_000s_val <- weight
        }
        else if (row_index == 5){
          loan_purpose_name_val <- weight
        }
        else if (row_index == 6){
          lien_status_name_val <- weight 
        }
        else if (row_index == 7){
          applicant_sex_name_val <- weight
        }
        else if (row_index == 8){
          applicant_race_name_1_val <- weight
        }
        else if (row_index == 9){
          applicant_ethnicity_name_val <- weight
        }
      }
    }
    # print(total_weight)
    # print(applicant_ethnicity_name_val)
    # print(vecImp[plot_input$column])
    # x = vecImp["applicant_race_name_1","Approved"]
    # y = vecImp[(vecImp[,1])["applicant_race_name_1"],plot_input$column]
    # df[(row.names(df))["c"],df$column_name]
    # print(vecImp[9,vecImp[plot_input$column]])
    
    if ((plot_input$column) == "Approved") { 
      print(paste("Approved values represent the significance of each
      vector in predicting whether a loan is approved or not. Higher values indicate higher importance. 

      For instance, “applicant_income_000s” with a value of ",
      round(applicant_income_000s_val,0)," has the highest importance as it
      represents ", round((100*applicant_income_000s_val/total_weight),2),
      "% of the total postive weight classifaction of all vectors.

      However, it is crucial to consider ethical implications, especially when 
      vectors for races, genders, and ethnicities are included. These vectors
      can lead to algorithmic discrimination, resulting in biased decisions that
      unfairly favor or disadvantage certain grouped identities.
      
      Algorithmic discrimination is a serious issue when
      machine learning models are trained on biased data or when biased
      assumptions are made during the model-building process.

      For example, the race and ethnicity of applicants have a ",
      round((100*applicant_race_name_1_val/total_weight),2),"% and ", 
      round((100*applicant_ethnicity_name_val/total_weight),2),
                    "% effect respectively on the predicted approval of applications.
                    This is an example of discrimination, where the
                    identity of a person is correlated and used by algorithms 
                    like this model to discriminately classify an applicant in an
                    unfair way. Fortunately, the gender of the applicant has the
                    least importance for classifying data within the model with a ",
                    round((100*applicant_sex_name_val/total_weight),2),"% 
                    effect on all approved application predictions.", sep=""))
      }
    
    else if ((plot_input$column) == "Denied") { 
      print(paste("Denied indicates the contribution of each feature vector
      to the model’s ability to distinguish between approved and denied outcomes
      for loan applications. Negative values suggest that as the feature increases,
      the likelihood of not being approved increases, while positive values suggest
      an increased likelihood of approval as the feature increases. 
      Indicates that higher loan amounts are strongly associated with not being approved.
      
      For instance, “loan_amount_000s” with a value of ",
      round(loan_amount_000s_val,0)," indicates that higher loan amounts are 
      strongly associated with not being approved. It has the greatest
      negative effect, representing ",
      round((100*loan_amount_000s_val/total_weight),2),
      "% of the total negative weight classifaction of all vector values.
      
      The feature vector “loan_purpose_name” has the only positive value at ",
      round(loan_purpose_name_val,0), ", which suggests that specific
      purposes for loans will increase the likelihood of approval.

      A shocking example of algorithmic disrimination is shown in the
      feature vectors of race and ethnicity. They have the second
      and third largest effect on all denied application predictions at ",
      round((100*applicant_race_name_1_val/total_weight),2),"%
      and ", round((100*applicant_ethnicity_name_val/total_weight),2),
      "% respectively. To be denied a home loan because of
      one's race and ethnicity is a serious reason why data scientists must
      carefully consider feature vectors and audit 
      the model's performance to protect
      against unjust bias and discriminatory behavior.
      
      It is fortunate however that the gender of the applicant has one
      of the least importance on the model with a ",
      round((100*applicant_sex_name_val/total_weight),2),
      "% effect on all denied application predictions.", sep=""))
      }
    
    else if ((plot_input$column) == "MeanDecreaseAccuracy") {
      print(paste("MeanDecreaseAccuracy values vary significantly. 
      A higher value indicates that excluding the corresponding feature
      would decrease the model’s accuracy by approximately that amount.
      
              For instance, “applicant_income_000s” has high importance with a
              value of ", round(applicant_income_000s_val,0)," and ", 
              round((100*applicant_income_000s_val/total_weight),2), "% of
              the total MeanDecreaseAccuracy score of all vector values. 
              As such, excluding this feature would decrease the model’s
              accuracy by approximately ",
              round((100*applicant_income_000s_val/total_weight),0), "%.
              
      A smaller value, like “lien_status_name” with a value of ",
      round(lien_status_name_val,0), " suggests less contribution to node purity, 
      meaning it is not as significant for classifying data within the model.

              The race and ethnicity of applicants have a ",
              round((100*applicant_race_name_1_val/total_weight),2),"%
              and ", round((100*applicant_ethnicity_name_val/total_weight),2),
              "% effect respectively on the total MeanDecreaseAccuracy score of
              all vectors. This is an example of algorithmic discrimination, 
              where the identity of a person is correlated and thus used by
              algorithms like this model to discriminately classify an applicant
              in an unfair way.
      
              The gender of the applicant is rightfully placed with the least importance
              with a ", round((100*applicant_sex_name_val/total_weight),2),"% 
              effect on the total MeanDecreaseAccuracy score of all vector values.",
              sep=""))
      }
    
    else if ((plot_input$column) == "MeanDecreaseGini") { 
      print(paste("MeanDecreaseGini values vary significantly. 
      A larger value indicates that splitting the data based on that variable
      results in nodes with higher purity (more homogenous).
      For instance, “applicant_income_000s” with a value of ",
      round(applicant_income_000s_val,0)," has high importance as it
      represents ", round((100*applicant_income_000s_val/total_weight),2),
      "% of the total MeanDecreaseGini score of all vector values.
      A smaller value, like “lien_status_name” with a value of ",
      round(lien_status_name_val,0), " suggests less contribution to node purity, 
      meaning it is not as significant for classifying data within the model.

      The race, gender, and ethnicity of applicants have a ",
      round((100*applicant_race_name_1_val/total_weight),2),"%, ",
      round((100*applicant_sex_name_val/total_weight),2),"%,  and ",
      round((100*applicant_ethnicity_name_val/total_weight),2),
      "% effect respectively on the total MeanDecreaseGini score of all vector values.
      Although their effect is relatively small, they should be manually trained
      to be negligible in future experiments, otherwise removed from the model."))
    }
  })
  
  ## After prediction output, clear session memory of unused R objects.
  gc()
})  

  ## || Extra Notes ||
  ## _________________
  # ## Render a normal print statement.
  # output$empty <- renderText({
  #   if (input$submitbutton>0) {
  #     "_________________________________________
  #     "
  #   }})  
  
  # ## Render a normal print statement.
  # output$statement <- renderText({
  #   if (input$submitbutton>0) {
  #   "Public data from the Home Mortgage Disclosure Act (HMDA) in Washingston State."
  # }})
  
  # output$summary <- renderPrint({
  #   if (input$submitbutton>0) {
  #   # Use the df data frame within your server logic
  #       summary(df)
  #     }
  #   })
    # "Hello, this is a normal statement."
    # approval_percent <- round(((df$Approved)*100),2)
    # average_percent <- 49.2
    # percent_diff <- round(((approval_percent/average_percent)-1)*100,2)
    # #gc()
    # if (percent_diff > 0) {
    #   print(paste("Home Loan Prediction: ", approval_percent,"% chance of approval. You have ", percent_diff,"% greater chance of approval than the average applicant.", sep=""))
    # } 
    # else if (percent_diff == 0) {
    #   print(paste("Home Loan Prediction: ", approval_percent,"% chance of approval. You have the same chance for approval as the average applicant.", sep=""))
    # }
    # else if (percent_diff < 0) {
    #   print(paste("Home Loan Prediction: ", approval_percent,"% chance of approval. You have ", abs(percent_diff),"% less chance of approval than the average applicant.", sep=""))
    # }
  # })

#####################################
## JUL-13N                         ##
## 12/08/23 - 02/05/24             ##
## https://github.com/JUL-13N      ##
#####################################
## || LOGS ||
## __________
## 02/05/24 ui_v14: Updating page content and refining style in About section.
## 01/31/24 ui_v13: Curate the footer on each page for mobile devices using JavaScript.
## 01/30 12: Analyzing the plots in new panel section using pre-computed data.
## 01/30 11: Calculating plots in new Plots panel section using pre-computed data.
## 01/29 10: Adding navigation bar and About Me section.
## 01/28 09: Fixing input data type and categorical factor options.
## 01/27 08: Adding JavaScript for mobile device detection and page compatibility.
## 01/27 07: Adding HTML/CSS for styling page elements.
## 01/26/24 ui_v06: Enhancing the automated numeric slider distribution.
## 01/23/24 ui_v05: Enhancing the automated numeric slider distribution.
## 01/18/24 ui_v04: Normalizing distribution of numeric slider selection.
## 12/20/23 ui_v03: Pairing numeric slider labels with their stylized name.
## 12/19/23 ui_v02: Define input selectors or sliders based on the given data.
## 12/15/23 ui_v01: Building an interactive web UI with drop-down input selectors.

## || PURPOSE ||
## _____________
## Read pre-computed data to generate the UI to save website run-time and memory.
## Collect user input for the server to output the machine learning prediction.
##
## This ui.R file reads the created ui_data_df.csv and ui_data_dc.csv files to
## generate the normal distribution range and steps of the numeric sliders and
## the unique, non-numeric values for the selection menus in order to collect
## the user input for the machine learning home loan model prediction.

library(shiny)
library(shinythemes)

## || INPUT ||
## ___________
## Importing pre-computed data, saving website run-time and memory.
##
## Pre-computed numeric data, generated from a data frame.
## This will be used for the sliderInput() function for slider selections.
ui_dataframe <- read.csv("ui_data_df.csv")
##
## Pre-computed categorical data, generated from a dictionary.
## This will be used for the selectInput() function for drop-down selections. 
ui_dictionary <- read.csv("ui_data_dc.csv", stringsAsFactors = FALSE)
# ui_dictionary <- ui_dictionary[-nrow(ui_dictionary), ]
# print(nrow(ui_dictionary))

## Extract all vector names: all elements in the first column of the data frame.
vector_names <- ui_dataframe[,1]
# print(vector_names)
# df_char <- data.frame(vector_names=character())
# print(length(ui_dictionary))
df_char <- data.frame(matrix(ncol = length(ui_dictionary), nrow = 3))
#print(colnames(ui_dictionary))
## Name the rows in numeric order for the new data frame.
colnames(df_char) <- c(colnames(ui_dictionary))
# df_char <- cbind(df_char, ui_dictionary[colnames(ui_dictionary) %in% setdiff(colnames(ui_dictionary), colnames(df_char))])
# df_char <- data.frame(vector_names=character())
# colnames(df_char) <- c(colnames(ui_dictionary))

## Could use pagewithSidebar or fluidPage to define the framework.
fluidPage(theme = shinytheme("flatly"),

## Navigation bar.
navbarPage(
  
  ## Website title.
  title = "JUL-13N",
  
  ## || MAIN PAGE: MODEL ||
  ## ______________________
  tabPanel("Model",
  
  ## Page header.
  #style = "text-align: center; padding-top: 0px; background-color: #f5f5f5;",
  headerPanel("Home Loan Prediction Model"),
  style = "padding-top: 1px; padding-left: 3px; background-color: #f5f5f5;",
  

  ## Left side of the page.
  sidebarPanel(
    HTML("<h3>Feature Vectors</h3>"),
    ## Create a selectInput or sliderInput for each column by applying function()
    ## to each element of the vector of column names (names(TrainSet) of the data frame.
    lapply(seq_along(vector_names), function(i) {
      
      ## Assigning the i element of the imported dictionary keys.
      vector_name <- vector_names[i]
      
      ## Assigning the i row of the 2nd column from the imported data frame.
      stylized_name <- ui_dataframe[i,2] 
      # print(ui_dataframe)
      
      ## If the numeric_slider value is TRUE, use the numeric slider input.
      if (isTRUE(ui_dataframe[i,3])){
        ## Clear session memory of unused R objects.
        # gc()

        ## || NUMERIC UI INPUT ||
        ## ______________________
        ## Create slider input with a normal distribution for numeric vectors.
        ## Must read the pre-computed data in the ui_data_df.csv file. 
        sliderInput(
          
          ## inputID defines the input vector information for the server.
          inputId = vector_name,
          
          ## This stylized label of the vector is what the user sees on the UI.
          label = stylized_name,
          
          ## The mean is the initial value of the numeric slider.
          value = ui_dataframe[i,4],
          
          ## Minimum value based on standard deviation.
          min = ui_dataframe[i,5],
          
          ## Maximum value based on standard deviation.
          max = ui_dataframe[i,6],
          
          ## This is the interval between each value on the slider.
          step = ui_dataframe[i,7]
          #,gc()
          )
        ## Clear session memory of unused R objects.
      } else {
        # gc()
        # unique_choices = ui_dictionary[[vector_name]]
        # k = strsplit(unique_choices, "| ")[[1]]
        # print(unique_choices)
        # print(vector_name)
        # print(ui_dictionary[[vector_name]])
        
        ## strsplit(ui_dictionary[[vector_name]], "\\| ") is a single string
        ## value that is divided by "| ". Because it is a special character,
        ## it requires "\" before it. Because "\" is also a special character,
        ## it requires another "\".
        ##
        ## This creates a list of strings (previously divided by "| ").
        ## Every single string in this list is a unique element from the
        ## categorical vectors of the the original data frame.
        ## Each categorical element appears only once in the list.
        string_choices <- strsplit(ui_dictionary[[vector_name]], "\\| ")[[1]]
        string_choices <- sort(string_choices)
        #factor_choices <- as.factor(string_choices)
        #print(paste("string choices: ",string_choices,sep=""))
        #print(paste("factor_choices: ",factor_choices, sep=""))
        #print(class(factor_choices))
        #print(class(vector_name))
        #vector_name <- as.factor(vector_name)
        #print(str(string_choices))
        #df <- data.frame(vector_names)
        
        #df_char[[vector_name]] <- string_choices
        #df_char[i,i] <- string_choices
        
        #df_char[vector_name] <- lapply(df_char[vector_name], as.factor)
        #print(str(df_char))
        
        ## || CATEGORICAL UI INPUT ||
        ## __________________________
        ## Creates drop-down selections for the categorical vectors.
        ## Must read the pre-computed data in the ui_data_dc.csv file. 
        selectInput(
          
          ## inputID defines the input vector information for the server.
          inputId = vector_name,
          
          ## This stylized label of the vector is what the user sees on the UI.
          label = stylized_name,
          
          ## Create a drop-down list with every single value from the columns of
          ## the data frame where each value will appear only once in the list.
          choices = string_choices #df_char[[vector_name]] #df_char$vector_name
          # ,gc()
          )
        }
    }),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  ## || MODEL OUTPUT FUNCTIONS ||
  ## ____________________________
  mainPanel(
    tags$label(h3("Application Status")), ## Status/Output text box.
    verbatimTextOutput("contents"),
    tableOutput("tabledata"), ## Prediction results table.
    textOutput("average"),
    textOutput("summary"),
    # textOutput("empty"),
    # textOutput("statement")
  ),
  tags$script(src = "mobileDetection.js"),
  ## Include the footer from an external HTML file
  # includeHTML("footer.html"),
  # tags$link(rel = "stylesheet", type = "text/css", href = "footer.css")

## || FOOTERS ||
## _____________
## Add footers.
tags$footer(
  "Copyright © 2024 JUL-13N. ",
  tags$a(href = "https://github.com/JUL-13N/Home-loan-discrimination", 
         "GitHub Repository"),
  style = "text-align: center; padding-top: 1020px; padding-left: 35px; padding-right: 35px; background-color: #f5f5f5;",
  ## Add a script tag to adjust padding dynamically.
  tags$script(
    HTML(
      "if (isMobileDevice()) {",
      "  document.querySelector('footer').style.marginTop = '-987px';",  ## Raise the text up with negative top margin padding.
      # "  document.querySelector(‘footer’).style.paddingLeft = '275px';", ## Add left padding
      # "  document.querySelector(‘footer’).style.paddingRight = '275px';", ## Add right padding         
      "}"
    )
  )
),
  tags$footer(
    "Training data from the Home Mortgage Disclosure Act of Washingston State in 2016.",
    style = "text-align: center; padding-bottom: 3px; background-color: #f5f5f5;"
  )
),

## || NEW PAGE: PLOTS ||
## _____________________
## Render plot in the new page defined by the UI.
## Use UI input to determine which data to plot.
tabPanel(title = "Analysis", style = "text-align: center; padding-left: 18px; padding-top: 1px; background-color: #f5f5f5;",
  h1("Home Loan Discrimination"),
  style = "text-align: left; padding-left: 18px; padding-top: 1px; padding-right: 20px; background-color: #f5f5f5;",
  
  ## Select column to plot.
  # selectInput(inputId = "column", label = "Choose a column", choices = vector_names),
  
  ## Select column to plot.
  selectInput(inputId = "column", 
              label = h4("Analyze the weight of each vector on the model prediction
                         to determine its importance and discrimination toward
                         the race, gender, and ethnicity of applicants."),
              # label = h4("Analyze the effect that each vector has on the model prediction."),
                         # , style = "padding-left: 25px"),
              choices = c("Approved", "Denied", "MeanDecreaseAccuracy", "MeanDecreaseGini")
  ),
  
  
  ## || PLOTS OUTPUT FUNCTIONS ||
  ## ____________________________
  mainPanel(
  ## Display plot.
  plotOutput("vecImpPlot"),
  textOutput("analysis")
  ),
  # img(src = "varImpPlot(model).png", height = "400px", width = "600px"),
  # "Applicant Income (Thousands)",
  # img(src = "Applicant_Income_(Thousands).png", height = "400px", width = "600px"),
  tags$script(src = "_mobileDetection.js"),
  
  tags$footer(
    "Copyright © 2024 JUL-13N. ",
    tags$a(href = "https://github.com/JUL-13N/Home-loan-discrimination", 
           "GitHub Repository"),
    style = "text-align: center; padding-top: 613px; padding-bottom: 3px; marginBottom = 10px; background-color: #f5f5f5;",
    
    ),
  ),

## || NEW PAGE: ABOUT ||
## ________________________
## About page with a panel in the navigation bar.
tabPanel("About",
         style = "text-align: left; padding-left: 30px; padding-right: 30px; padding-top: 1px; background-color: #f5f5f5;",
         HTML("
<h1><img src=\"https://media.tenor.com/SNL9_xhZl9oAAAAi/waving-hand-joypixels.gif\" alt=\"waving hand\" width='75' height='75'/> Hey, I'm JUL-13N!</h1>

<h4>I’m a data scientist with a passion for building machine learning applications with a particular interest in algorithmic discrimination for financial data.</h4>

Over 7 years of experience managing and analyzing big data from financial
records, hospital reports, specimen samples, blockchains, websites,
social media, games, and cloud databases. Specialized in training machine
learning and Al models using data manipulation, processing, and visualization
to identify trends and develop solutions for financial and clinical environments.
I utilize Python, R Shiny, R, Power BI, Git, SQL, Java, JavaScript, HTML and CSS
among various languages and frameworks to create interactive web applications and
dashboards for revealing insightful results and tailored solutions from big data.
<br>
<br>

With all the projects that I take on, I bring high value skills, creative
solutions, and a hunger for knowledge.

<p>
How about we tackle some big data challenges together? Let's go. I'm ready to build.
Feel free to <a href=\"mailto:jul13ns10@gmail.com?subject=Professional Enquiry &body=Hey JUL-13N, I was just checking your website...\"> send me an email</a>.
</p>

<div align=center>

<h4>Learn more about me in my links below.</h4>

<br>
<br>

<a href=\"https://github.com/JUL-13N/Home-loan-discrimination\">
  <img style=\"margin:0.55rem\" src=\"https://github-readme-stats.vercel.app/api/pin/?username=JUL-13N&repo=Home-loan-discrimination&bg_color=2c3e50&theme=react\" />
</a> <a href=\"https://github.com/JUL-13N/Parallel-market-trends\">
  <img style=\"margin:0.55rem\" src=\"https://github-readme-stats.vercel.app/api/pin/?username=JUL-13N&repo=Parallel-market-trends&bg_color=2c3e50&theme=react\" />
</a>
<br>

 <a href=\"https://github.com/JUL-13N\">
  <img width='400px' alt=\"JUL-13N's GitHub Stats\" src=\"https://github-readme-stats.vercel.app/api?username=JUL-13N&show_icons=true&count_private=true&bg_color=2c3e50&theme=react\" />
</a>

<a href=\"https://github.com/JUL-13N\">
  <img width='420px' alt=\"GitHub Streak Stats\" src=\"https://github-readme-streak-stats.herokuapp.com?user=JUL-13N&background=2c3e50&theme=react\">
  </a>  
<br>

<img width='400px' alt=\"GitHub Most Used Languages\" src=\"https://github-readme-stats.vercel.app/api/top-langs/?username=JUL-13N&hide=html,css&langs_count=10&bg_color=2c3e50&layout=compact&theme=react\" />
    </a>
</div>
<br>
"),
         tags$footer(
           "Copyright © 2024 JUL-13N. ",
           tags$a(href = "https://github.com/JUL-13N/Home-loan-discrimination", "GitHub Repository"),
           style = "text-align: center; padding-top: -1px; padding-bottom: 3px; marginBottom = 3px; background-color: #f5f5f5;"
           ## Add a script tag to adjust padding dynamically.
           # tags$script(
           #   HTML(
           #     "if (isMobileDevice()) {",
           #     "  document.querySelector('footer').style.marginTop = '-1300px';",  ## Raise the text up with negative top margin padding.
           #     "  document.querySelector(‘footer’).style.paddingLeft = '275px';", ## Add left padding
           #     "  document.querySelector(‘footer’).style.paddingRight = '275px'';", ## Add right padding                "}"
           #   )
           # )
         )
    )

  )
)

## || Extra Notes ||
## _________________
## This code finds the element in the data frame df that is in the row named “c”
## and the column named “column_name”, and prints it.
## x = df[(row.names(df))["c"],df$column_name]
## print(x)
##
## Select the value in row "c" and column "column_name"
## row_index <- which(row.names(df) == "c")
## col_index <- which(colnames(df) == "column_name")
## x <- df[row_index, col_index]

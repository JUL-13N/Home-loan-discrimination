#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/19/23             ##
## https://github.com/JulienSimons ##
#####################################
## Making port environment for gateway connection.
library(shiny)

## If there is no default port environment variable set on the local machine,
## then use ShinyApps default port number to satisfy the gateway connection.
port <- Sys.getenv('PORT')
if (port == "") port <- 3838

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)

#####################################
## JUL-13N                         ##
## 12/08/23 - 02/05/24             ##
## https://github.com/JUL-13N      ##
#####################################
## || LOGS ||
## __________
## 02/05/24 init_v02: Installing all necessary packages if not yet installed.
## 12/19/23 init_v01: Installing packages if not already installed.

## Installing all necessary packages if not yet installed. 
my_packages = c("shiny", "data.table", "randomForest", "ggplot2",
                "dplyr", "reshape2", "shinythemes")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

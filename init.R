#####################################
## Julien J. Simons                ##
## 12/08/23 - 12/19/23             ##
## https://github.com/JulienSimons ##
#####################################

## Install packages if not already installed.
my_packages = c("randomForest", "data.table")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

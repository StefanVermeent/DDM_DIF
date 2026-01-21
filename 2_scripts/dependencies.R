#install.packages('groundhog')
library(groundhog)

# Only run the commented out code below if the `groundhog/` folder needs to be initiated again 

meta.groundhog("2025-10-01")
#groundhog::set.groundhog.folder(file.path(getwd(), 'groundhog'))

# Dependencies for this project
pkgs <- 
  c(
    "tidyverse",
    "haven",
    "OpenMx",
    "lavaan",
    "sjlabelled",
    "glue",
    "RWiener",
    "flextable",
    "english",
    "ggsci",
    "rstan",
    "runjags",
    "finalfit",
    "openxlsx",
    "purrr",
    "future",
    "mice"
  )


# The date of which we want to install and load the package versions
date <- "2025-10-01"

# Using the groundhog package, we install and load the project dependencies
groundhog.library(pkg = pkgs, date = date)


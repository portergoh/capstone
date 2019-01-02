#-----------------------------------------------------------------------#
# Loads all the required functions from github
#-----------------------------------------------------------------------#

source("https://raw.githubusercontent.com/portergoh/capstone/master/functions.R")

packages<-c("rvest",
            "fastDummies", 
            "GGally", 
            "tidyverse",
            "httr",
            "jsonlite",
            "ggthemes",
            "modelr",
            "broom")
#-----------------------------------------------------------------------#
# Check if packages are missing from system and install them if 
# necessary, otherwise load the require libraries
#-----------------------------------------------------------------------#

check_packages(packages)


#Project - supplementary R script

#################################################################################
#Libraries

#install.packages("readxl")-  in case you need to install the library
library(ggplot2) #activating library
library(readxl)

####################################################################################
#Data loading
ds <- read_excel("Project\\Database.xlsx", sheet = 1)  # Specify the sheet if there are multiple sheets
#you need to set working directory to the Project one: Session -> Set working directory -> To project directory

###############################################################################¨
View(ds)
summary(ds)

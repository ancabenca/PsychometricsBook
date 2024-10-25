#Project - supplementary R script

#################################################################################
#Libraries

#install.packages("readxl")-  in case you need to install the library
#install.packages("dplyr")
library(ggplot2) #activating library
library(readxl)
library(dplyr)

#Plot setting (same as in the source material)
theme_fig <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_blank(),
      legend.background = element_blank()
    )
}



####################################################################################
#Data loading
western_slovenia <- read_excel("Project\\Database-updated.xlsx", sheet = 1)  
eastern_slovenia <- read_excel("Project\\Database-updated.xlsx", sheet = 2)  
#you need to set working directory to the Project one: Session -> Set working directory -> To project directory

#Data manipulation¨
## New atribute created (western) which is indicator if the subject is from Western Slovenia (=1) or not (=0)

# Add the Western attribute
western_slovenia <- western_slovenia %>%
  mutate(Western = 1)

eastern_slovenia <- eastern_slovenia %>%
  mutate(Western = 0)

# Check the changes
summary(western_slovenia)
summary(eastern_slovenia)

# Merge the dataframes
ds <- bind_rows(western_slovenia, eastern_slovenia)
str(ds)

summary(ds)
c(nrow(western_slovenia) + nrow(eastern_slovenia) == nrow(ds))


#Optionally set the Code as name of the rows-----------------------------

# Set 'Code' column as row names
# Convert tibble to data frame
#ds <- as.data.frame(ds)

# Set 'Code' column as row names
#rownames(ds) <- ds$Code
#-------------------------------------------------------------------------

# Renaming for easier handling (Cause mezery sucks)


ds <- ds %>% rename(Year = `year of study`)
unique(ds$Year)

# Create a new column 'YearNumber' based on the values in 'Year'
ds <- ds %>%
  mutate(YearNumber = case_when(
    Year == "2019/20" ~ 1,
    Year == "2020/21" ~ 2,
    Year == "2021/22" ~ 3,
    Year == "2022/23" ~ 4,
    Year == "2023/24" ~ 5
  ))

summary(ds)
dim(ds)
dim(western_slovenia)

#Two tests: make separate sheet - maybe useful
ds_og <- ds[,c(1:46,91:95,101:105,111:112)]
ds_close <- ds[,c(1:2,47:90,96:100,106:112)]
str(ds_og)
str(ds_close)

#Note all items should be on same scale with the same "sign" (as oposed to documentation)


###############################################################################
#BASIC OVERVIEW

head(ds)
tail(ds)
str(ds)
View(ds)
summary(ds)

#-------------------------------------------------------------------------------
#BASIC EXPLORATION PER ITEM
exam_item <- ds$"BFI_1" # set here for easier manipulation

#Table A =1,....
table(exam_item)

#Normalized
proportions(table(exam_item))
prop.table(table(exam_item)) #same


# Examining possible NA values

rows_with_na <- ds %>%
  filter(if_any(everything(), is.na))

# View the result
rows_with_na #one row is NA -> I reccomend to not include it in our analysis

# Deleting the row
ds <- ds %>%
  filter(if_all(everything(), ~ !is.na(.)))


table(ds$Western)
table(ds$YearNumber)

# Checking for duplicates
#1. Code
n_distinct(ds$Code) == nrow(ds) #nothing

#2 row
ds[duplicated(ds),] #nothing


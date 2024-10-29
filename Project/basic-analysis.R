#Project - supplementary R script

#################################################################################
#Libraries

#install.packages("readxl")-  in case you need to install the library
#install.packages("dplyr")
library(ggplot2) #activating library
library(readxl)
library(dplyr)
library(lmtest)
library(psych)
library(ggdendro)
library(ShinyItemAnalysis)

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


##############################################################################
#Chapter 2: Validity
#What can we compare: Two sets of test - one from self-eval, the second from close ones
#It probably should be by one trait
#I would personally explain the theory on one trait and than as supplement mentioned the results from the others

#We cannot do the prop tables, since items were not rated by profs
#?? What is ZBFI? I guess something standardized? Z score perhpas

#We may also compare the Years and Western/Eastern (first anova, second two sample t test)

#?? Do we have flaged which items are for what trait? Idk if we need it, but I can assume it would be nice
# Combine datasets for side-by-side comparison


#A. Extraversion
combined_data <- data.frame(
  score = c(ds_og$BFI_extraversion, ds_close$BFI_closeones_extraversion),
  group = factor(rep(c("Self", "Close"), 
                     c(length(ds_og$BFI_extraversion), length(ds_close$BFI_closeones_extraversion))))
)

ggplot(combined_data, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  ylab("Total score") +
  xlab("") +
  theme_fig() +
  theme(legend.position = "none")

t.test(ds$BFI_extraversion, ds$BFI_closeones_extraversion, paired = TRUE)
#yep there is difference
#not sure if this contributes to validity of the test tho
#it states, that folks do not rate themselves (in the mean) in comparison with close ones

#-----------------------------------------------------------------------------
#note: I made it little bit complicated, one could use just ds and this scripts

# ggplot(data.frame(
# score = c(HCIprepost$score.pre, HCIprepost$score.post),
# group = factor(rep(c("Pre", "Post"), each = 16),
#                levels = c("Pre", "Post"))),
# aes(x = group, y = score, fill = group)) +
#   geom_boxplot() + ylab("Total score") + xlab("") +
#   theme_fig() + theme(legend.position = "none")

#-----------------------------------------------------------------------------


#B Agreeableness
var1 <- ds$BFI_agreeableness
var2 <- ds$BFI_closeones_agreeableness

ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")


t.test(var1,var2, paired = TRUE) #rejecting HO


#------------------------------------------------------------------------------

#C Conscientiousness
var1 <- ds$BFI_conscientiousness
var2 <- ds$BFI_closeones_conscientiousness

ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")


t.test(var1,var2, paired = TRUE) #rejecting HO


#--------------------------------------------------------------------------------


#D Neuroticism
var1 <- ds$BFI_neuroticism
var2 <- ds$BFI_closeones_neuroticism

ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")


t.test(var1,var2, paired = TRUE) #rejecting HO

#------------------------------------------------------------------------------

#E Openness
var1 <- ds$BFI_openness
var2 <- ds$BFI_closeones_openness

ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")


t.test(var1,var2, paired = TRUE) #rejecting HO

#----------------------------------------------------------------------------
#Big picture
# Calculate differences for each trait
diff_extraversion <- ds$BFI_extraversion - ds$BFI_closeones_extraversion
diff_agreeableness <- ds$BFI_agreeableness - ds$BFI_closeones_agreeableness
diff_conscientiousness <- ds$BFI_conscientiousness - ds$BFI_closeones_conscientiousness
diff_neuroticism <- ds$BFI_neuroticism - ds$BFI_closeones_neuroticism
diff_openness <- ds$BFI_openness - ds$BFI_closeones_openness

# Combine all differences into a single data frame
combined_diffs <- data.frame(
  difference = c(
    diff_extraversion, 
    diff_agreeableness, 
    diff_conscientiousness, 
    diff_neuroticism, 
    diff_openness
  ),
  trait = factor(rep(c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness"),
                     times = c(length(diff_extraversion), 
                               length(diff_agreeableness), 
                               length(diff_conscientiousness), 
                               length(diff_neuroticism), 
                               length(diff_openness)))
  )
)

# Create the boxplot for the differences
ggplot(combined_diffs, aes(x = trait, y = difference)) +
  geom_boxplot(fill = "lightblue") +
  ylab("Difference (Self - Close Ones)") +
  xlab("Traits") +
  theme_fig() +
  theme(legend.position = "none")

#We see that most of the time the close ones scored higher the individual. But in neurotism we
#can spot opačný trend (furthmore the biggest difference)

###############################################################################
#Between west/east

#I am lazy so I let CGPT generate these in bulk, it is simple for cycle
# Define the traits to test
traits <- c("BFI_extraversion", "BFI_agreeableness", "BFI_conscientiousness", "BFI_neuroticism", "BFI_openness")

# Initialize a list to store results
t_test_results <- list()

# Loop through each trait and perform the t-test
for (trait in traits) {
  # Define group1 and group2 for the current trait
  group1 <- ds[[trait]][ds$Western == 0]  # Scores where Western == 0
  group2 <- ds[[trait]][ds$Western != 0]  # Scores where Western != 0
  
  # Perform the two-sample t-test
  t_test_results[[trait]] <- t.test(group1, group2)
}

# Print the results for each trait
for (trait in traits) {
  cat("\nResults for", trait, ":\n")
  print(t_test_results[[trait]])
}

#Interesting, only one was rejected: Agreeableness , Easterners has higher agreeableness (in the mean) than Westerners
#-------------------------------------------------------------------------

# Define the traits to test
traits2 <- c("BFI_closeones_extraversion", "BFI_closeones_agreeableness", "BFI_closeones_conscientiousness", "BFI_closeones_neuroticism", "BFI_closeones_openness")

# Initialize a list to store results
t_test_results <- list()

# Loop through each trait and perform the t-test
for (trait in traits2) {
  # Define group1 and group2 for the current trait
  group1 <- ds[[trait]][ds$Western == 0]  # Scores where Western == 0
  group2 <- ds[[trait]][ds$Western != 0]  # Scores where Western != 0
  
  # Perform the two-sample t-test
  t_test_results[[trait]] <- t.test(group1, group2)
}

# Print the results for each trait
for (trait in traits2) {
  cat("\nResults for", trait, ":\n")
  print(t_test_results[[trait]])
}

#but the second questionnare did not reject anything, I should probably not make any conclusion
#however Easterns rates themselves as more agreeable than Westerns, but close ones do not think so ><

#-------------------------------------------------------------------------------
#Year - I guess just for the whole picture


#!!! ANOVA assumptions
#The responses for each factor level have a normal population distribution. -> need to check
#These distributions have the same variance. -> need to check
#The data are independent. -> this one is okay



# Define the traits to test
traits <- c("BFI_extraversion", "BFI_agreeableness", "BFI_conscientiousness", "BFI_neuroticism", "BFI_openness")

# Initialize a list to store ANOVA results
anova_results <- list()

# Loop through each trait and perform ANOVA
for (trait in traits) {
  # Perform ANOVA using the formula interface
  anova_results[[trait]] <- aov(ds[[trait]] ~ as.factor(ds$YearNumber), data = ds)
}

# Print the results for each trait
for (trait in traits) {
  cat("\nANOVA results for", trait, ":\n")
  print(summary(anova_results[[trait]]))
}


#No signifcant difference
#---------------------------------------------------------------------------------
traits2 <- c("BFI_closeones_extraversion", "BFI_closeones_agreeableness", "BFI_closeones_conscientiousness", "BFI_closeones_neuroticism", "BFI_closeones_openness")

# Loop through each trait and perform ANOVA
for (trait in traits2) {
  # Perform ANOVA using the formula interface
  anova_results[[trait]] <- aov(ds[[trait]] ~ as.factor(ds$YearNumber), data = ds)
}

# Print the results for each trait
for (trait in traits2) {
  cat("\nANOVA results for", trait, ":\n")
  print(summary(anova_results[[trait]]))
}

#No significant difference

########################################################################################
#Assumption verification

#A+B = normality + homoscedascisity (aka equal variance)

plot(ds$BFI_agreeableness)
plot(ds$BFI_closeones_agreeableness)
hist(ds$BFI_agreeableness) #well, this is not really normal, right?
hist(ds$BFI_closeones_agreeableness)
shapiro.test(ds$BFI_agreeableness) #test for normality, it is highly sensitive to outlier, so it is not defeinite proof

traits2 <- c(, "BFI_closeones_agreeableness", "BFI_closeones_conscientiousness", "BFI_closeones_neuroticism", "BFI_closeones_openness")

var <- ds$"BFI_extraversion"
var2 <- ds$"BFI_closeones_extraversion"
  
plot(var)
plot(var2)
hist(var) 
hist(var2)
var.test(var,var2) #test for equal variance

#The rest vars are the same -> TODO
################################################################################

#Correlation Coefficient
hist(ds$BFI_agreeableness) #could make finer boxes
hist(ds$BFI_closeones_agreeableness) #hmmm, not really normal
hist(ds$BFI_agreeableness + ds$BFI_closeones_agreeableness) #rather heavytailed


ggplot(ds, aes(x = BFI_extraversion, y = BFI_closeones_extraversion)) +
  geom_point() + theme_fig() +
  xlab("Self") + ylab("Close") #nice,there is some relantionship visible?

var1 <- ds$BFI_extraversion
var2 <- ds$BFI_closeones_extraversion
cor.test(var1, var2) #correlated

##TODO for others
##??Can we ask questions if two traits are correlated? Heck, why not?
#-------------------------------------------------------------------
ggplot(ds, aes(x = BFI_extraversion, y = BFI_agreeableness)) +
  geom_point() + theme_fig() +
  xlab("extraversion") + ylab("aggreableness") #nice,there is some relantionship visible?

var1 <- ds$BFI_extraversion
var2 <- ds$BFI_agreeableness
cor.test(var1, var2) #correlated

#---------------------------------------------------------------------
#Regression

#Question: Let's say I am interested for two models:
#A. How do other traits depend on the aggreablness (Y)

#Not sure if I have to use ds_og or if ds is alright (??Need to ask)

summary(ds)
lm.model1 <- lm(BFI_agreeableness ~ BFI_extraversion + BFI_conscientiousness + BFI_neuroticism +  BFI_openness, data = ds_og)
summary(lm.model1)
drop1(lm.model1, test ="Chisq") #F test -> more accurate than significant test from the summary ()


plot(lm.model1) #diadnostics is okay, not the greatest, but okay
#-----------------
lm.model1.1 <- lm(BFI_agreeableness ~ BFI_conscientiousness + BFI_neuroticism, data = ds_og)
summary(lm.model1.1)
drop1(lm.model1.1, test ="Chisq") #F test -> more accurate than significant test from the summary ()

plot(lm.model1.1) #diadnostics is okay, not the greatest, but okay

anova(lm.model1, lm.model1.1) #IThe null hypothesis states that the simpler model is sufficient, meaning the parameters omitted in the submodel do not significantly improve the model.
#It is okay to simplify to lm.model1.1

#-----------------
lm.model1.2 <- lm(BFI_agreeableness ~BFI_neuroticism, data = ds_og)
summary(lm.model1.2)
drop1(lm.model1.2, test ="Chisq") #F test -> more accurate than significant test from the summary ()

anova(lm.model1.1, lm.model1.2) #can be simplified
plot(lm.model1.2) #okay assumptions 

#However trash model -> I am still not sure, how to deal with this type of dataset
#since I assume, there is multicolinearity in the Xs -> regression does not make sense here


############################################################################
#B agreable on items ->okay this is bullshit, because I just found in lm.model2.1 how was this score computed
str(ds_og)

lm.model2 <- lm(BFI_agreeableness ~ BFI_1 + BFI_2 + BFI_3 + BFI_4 + BFI_5 + BFI_6 + BFI_7 + BFI_8 + BFI_9 + BFI_10 + BFI_11 + BFI_12 + BFI_13 + BFI_14 + BFI_15 + BFI_16 + BFI_17 + BFI_18 + BFI_19 + BFI_20 + BFI_21 + BFI_22 + BFI_23 + BFI_24 + BFI_25 + BFI_26 + BFI_27 + BFI_28 + BFI_29 + BFI_30 + BFI_31 + BFI_32 + BFI_33 + BFI_34 + BFI_35 + BFI_36 + BFI_37 + BFI_38 + BFI_39 + BFI_40 + BFI_41 + BFI_42 + BFI_43 + BFI_44
,data = ds_og) #It could probably be written much more neatly, but hey this works

summary(lm.model2) #too much variables

lm.model2.1 <- lm(BFI_agreeableness ~ BFI_2 + BFI_7 + BFI_12 +BFI_17 + BFI_22 +BFI_27 +BFI_32 +BFI_37 + BFI_42, data = ds_og)
summary(lm.model2.1)

#########################################################################################
########################################################################################
##CHAPTER 3
str(ds[,3:90])
#Correlation--------------------------------------------------------------------

#we could also do format, where we sum number of answer from A, B, C -> and rows would be 
#the items themselves

cor(table(ds$BFI_1,ds$BFI_2))
cor.test(ds$BFI_1,ds$BFI_2) #cannot reject H0

tetrachoric(table(ds$BFI_1,ds$BFI_2)) #non functioning -> probably not the right data for it (TODO)

#nop our multiple-answer items are not suitable, since the test relies on 2X2 contigency tables (I think)¨

polychoric(table(ds$BFI_1,ds$BFI_2))#I am dummy, this the generalization of tetra above
#)
(corFull <- polychoric(x = ds[,3:90])$rho) # for both questionarres

#TODO interpretation (page)

#Clustering---------------------------------------------------------------------------
hc <- hclust(d = as.dist(1 - corFull), method = "ward.D2")
ggdendrogram(data = hc)
plot_corr(Data = ds[,3:90], cor = "polychoric", clust_method = "ward.D2")
#I think it is natural to have such layout
#TODO Interpretation

#Factor Analysis--------------------------------------------------------------

fa(r = corFull, nfactors = 1, fm = "ml")
FA1 <- fa(r = ds[,3:90], cor = "polychoric", nfactors = 1, fm = "ml")

#Okay, I undersestimated the Factor analysis, big TODO till the end of the day


#Project - supplementary R script

#################################################################################
#Libraries
rm(list = ls())
#install.packages("readxl")-  in case you need to install the library
#install.packages("dplyr")
install.packages("forecast")
library(ggplot2) #activating library
library(readxl)
library(dplyr)
library(lmtest)
library(psych)
library(ggdendro)
library(ShinyItemAnalysis)
library(moments)
library(forecast)
library(gridExtra)
library(tibble)
library(moments)
library(corrplot)


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


# Remove the row where Code is "MZ31SG"
eastern_slovenia <- subset(eastern_slovenia, Code != "MZ31SG")



# Merge the dataframes
ds_load <- bind_rows(western_slovenia, eastern_slovenia)
str(ds_load)
summary(ds_load)
summary(ds_load)
c(nrow(western_slovenia) + nrow(eastern_slovenia) == nrow(ds_load))


#Optionally set the Code as name of the rows-----------------------------

# Set 'Code' column as row names
# Convert tibble to data frame
#ds <- as.data.frame(ds)

# Set 'Code' column as row names
#rownames(ds) <- ds$Code
#-------------------------------------------------------------------------

# Renaming for easier handling (Cause mezery sucks)


ds_load <- ds_load %>% rename(Year = `year of study`)
unique(ds_load$Year)

# Create a new column 'YearNumber' based on the values in 'Year'
ds_load <- ds_load %>%
  mutate(YearNumber = case_when(
    Year == "2019/20" ~ 1,
    Year == "2020/21" ~ 2,
    Year == "2021/22" ~ 3,
    Year == "2022/23" ~ 4,
    Year == "2023/24" ~ 5
  ))



#Two tests: make separate sheet - maybe useful
ds_og <- ds_load[,c(1:46,91:95,101:105,111:112)]
ds_close <- ds_load[,c(1:2,47:90,96:100,106:112)]
str(ds_og)
str(ds_close)
summary(ds_og)
summary(ds_close)

#--------------------------------------------------------
write.csv(ds, file = "ds_table.csv", row.names = FALSE)
selected_cols <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42")
  

selcol2 <- c("BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
"BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
"BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42")


SIAver <- ds_load[, selected_cols]
SIAver2 <- ds_load[, selcol2]
SIAver3 <- ds_load[, "BFI_closeones_agreeableness"]


write.csv(SIAver, file = "ds_table2.csv", row.names = FALSE)
write.csv(SIAver2, file = "ds_table3.csv", row.names = FALSE)
write.csv(SIAver3, file = "ds_tableCriterion.csv", row.names = FALSE)

View(ds_og)
#Note all items should be on same scale with the same "sign" (as oposed to documentation)
#############################################################################
#Now run the reversingVar

#This is piggy style, but I am not by hand replacing ds with ds_combined
ds <- ds_combined

###############################################################################
#BASIC OVERVIEW

head(ds)
tail(ds)
str(ds)
View(ds)
summary(ds)

# Load necessary libraries
library(tibble)
library(moments)  # For skewness and kurtosis calculations

# Calculate descriptive statistics and skewness/kurtosis for both total scores
descriptive_stats <- tibble(
  Score_Type = c("BFI_agreeableness", "BFI_closeones_agreeableness"),
  Mean = c(mean(ds$BFI_agreeableness, na.rm = TRUE), mean(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  SD = c(sd(ds$BFI_agreeableness, na.rm = TRUE), sd(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Min = c(min(ds$BFI_agreeableness, na.rm = TRUE), min(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Max = c(max(ds$BFI_agreeableness, na.rm = TRUE), max(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Skewness = c(skewness(ds$BFI_agreeableness, na.rm = TRUE), skewness(ds$BFI_closeones_agreeableness, na.rm = TRUE)),
  Kurtosis = c(kurtosis(ds$BFI_agreeableness, na.rm = TRUE), kurtosis(ds$BFI_closeones_agreeableness, na.rm = TRUE))
)

# Print the formatted table
print(descriptive_stats)

descriptive_stats

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

#------------------------------------------------------------------------------
#Basic plotting

ds
# Create the first histogram for BFI_agreeableness
plot1 <- ggplot(ds, aes(x = BFI_agreeableness)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of BFI Agreeableness", x = "BFI Agreeableness Score", y = "Frequency") +
  theme_minimal()

# Create the second histogram for BFI_closeones_agreeableness
plot2 <- ggplot(ds, aes(x = BFI_closeones_agreeableness)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of BFI Closeones Agreeableness", x = "BFI Closeones Agreeableness Score", y = "Frequency") +
  theme_minimal()

# Arrange the two plots side by side
combined_plot<- grid.arrange(plot1, plot2, ncol = 2)
ggsave("combined_histograms.png", plot = combined_plot, width = 12, height = 6, dpi = 300)


#Item desctiptive
# Load necessary libraries
 # For skewness and kurtosis

# List of score variables
score_vars <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42"
)

# Calculate descriptive statistics
descriptive_stats <- tibble(
  Score_Type = score_vars,
  Mean = sapply(score_vars, function(var) mean(ds[[var]], na.rm = TRUE)),
  SD = sapply(score_vars, function(var) sd(ds[[var]], na.rm = TRUE)),
  Min = sapply(score_vars, function(var) min(ds[[var]], na.rm = TRUE)),
  Max = sapply(score_vars, function(var) max(ds[[var]], na.rm = TRUE)),
  Skewness = sapply(score_vars, function(var) skewness(ds[[var]], na.rm = TRUE)),
  Kurtosis = sapply(score_vars, function(var) kurtosis(ds[[var]], na.rm = TRUE))
)

# View the descriptive statistics
print(descriptive_stats)

descriptive_stats_year_western <- tibble(
  Score_Type = c("YearNumber", "Western"),
  Mean = c(mean(ds$YearNumber, na.rm = TRUE), mean(ds$Western, na.rm = TRUE)),
  SD = c(sd(ds$YearNumber, na.rm = TRUE), sd(ds$Western, na.rm = TRUE)),
  Min = c(min(ds$YearNumber, na.rm = TRUE), min(ds$Western, na.rm = TRUE)),
  Max = c(max(ds$YearNumber, na.rm = TRUE), max(ds$Western, na.rm = TRUE)),
  Skewness = c(skewness(ds$YearNumber, na.rm = TRUE), skewness(ds$Western, na.rm = TRUE)),
  Kurtosis = c(kurtosis(ds$YearNumber, na.rm = TRUE), kurtosis(ds$Western, na.rm = TRUE))
)
print(descriptive_stats_year_western)

#--------------------------------------------------------------------------------
# 3. Compute Z-scores, T-scores, and percentiles for both scores
ds_res <- ds %>%
  mutate(
    BFI_agreeableness_z = (BFI_agreeableness - mean(BFI_agreeableness, na.rm = TRUE)) / sd(BFI_agreeableness, na.rm = TRUE),
    BFI_agreeableness_t = BFI_agreeableness_z * 10 + 50,
    BFI_agreeableness_percentile = pnorm(BFI_agreeableness_z) * 100,
    
    BFI_closeones_agreeableness_z = (BFI_closeones_agreeableness - mean(BFI_closeones_agreeableness, na.rm = TRUE)) / sd(BFI_closeones_agreeableness, na.rm = TRUE),
    BFI_closeones_agreeableness_t = BFI_closeones_agreeableness_z * 10 + 50,
    BFI_closeones_agreeableness_percentile = pnorm(BFI_closeones_agreeableness_z) * 100
  )

# Compute success rates for each respondent based on a threshold (e.g., score >= 50)
threshold <- 50
ds_res <- ds_res %>%
  mutate(
    BFI_agreeableness_success = if_else(BFI_agreeableness >= threshold, "Success", "Fail"),
    BFI_closeones_agreeableness_success = if_else(BFI_closeones_agreeableness >= threshold, "Success", "Fail")
  )

z_t_percentile_table <- ds_res %>%
  select(
    BFI_agreeableness_z,
    BFI_agreeableness_t,
    BFI_agreeableness_percentile,
    BFI_closeones_agreeableness_z,
    BFI_closeones_agreeableness_t,
    BFI_closeones_agreeableness_percentile
  )

# Create a table for the relevant metrics for the first respondent
first_respondent_table <- tibble(
  Score_Type = c("BFI_agreeableness", "BFI_closeones_agreeableness"),
  Z_Score = c(ds_res$BFI_agreeableness_z[1], ds_res$BFI_closeones_agreeableness_z[1]),
  T_Score = c(ds_res$BFI_agreeableness_t[1], ds_res$BFI_closeones_agreeableness_t[1]),
  Percentile = c(ds_res$BFI_agreeableness_percentile[1], ds_res$BFI_closeones_agreeableness_percentile[1]),
  Success_Rate = c(ds_res$BFI_agreeableness_success[1], ds_res$BFI_closeones_agreeableness_success[1])
)

write.csv(z_t_percentile_table, "z_t_percentiles.csv", row.names = FALSE)

# Display the first respondent's results
print("Results for the first respondent:")
print(first_respondent_table)


View(ds)

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
summary(ds)

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

boxplotsAGG <- ggplot(data.frame(
  score = c(var1, var2),
  group = factor(rep(c("Self", "Close"), 
                     times = c(length(var1), length(var2))),  # Use 'times' for correct repetitions
                 levels = c("Self", "Close"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + 
  ylab("Agreeableness") + 
  xlab("") +
  theme_minimal() +
  scale_fill_manual(values = c("Self" = "lightblue", "Close" = "lightcoral"))  # Set custom colors



t.test(var1,var2, paired = TRUE) #rejecting HO
ggsave("boxplotAGG.png", plot = boxplotsAGG, width = 12, height = 6, dpi = 300)


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
  scale_fill_manual(values = c("Self" = "lightblue", "Close" = "lightcoral")) +  # Specify colors here
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
diffBP <- ggplot(combined_diffs, aes(x = trait, y = difference)) +
  geom_boxplot(fill = "lightcoral") +
  ylab("Difference (Self - Close Ones)") +
  xlab("Traits") +
  theme_minimal()
ggsave("diffBP.png", plot = diffBP, width = 12, height = 6, dpi = 300)



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
str(ds)
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


# Assuming you have a data frame named ds

# Select the relevant columns
selected_columns <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42",
  "BFI_agreeableness", "BFI_closeones_agreeableness"
  
)

# Create the correlation matrix
correlation_matrix <- cor(ds[, selected_columns], use = "complete.obs")

write.csv(correlation_matrix, "correlation_matrix.csv", row.names = FALSE)

# Optionally, visualize the correlation matrix
# You can use the corrplot package for better visualization


# Plot the correlation matrix
corTESTscores <- corrplot(
  correlation_matrix,
  method = "circle",
  tl.col = "black",                     # Neutral label color
  tl.cex = 0.8,                # Set coefficient text color to gray
  col = colorRampPalette(c("red", "white", "blue"))(200), # Color gradient
  title = "Correlation Matrix of Selected Variables",
  mar = c(0, 0, 1, 0),                  # Minimal margin
  cl.pos = "b",                         # Position color legend at the bottom
  cl.cex = 0.7                          # Adjust color legend size
)
# Optional: Clean up the plot background to make it minimalist
   
ggsave("corTESTscores.png", plot = corTESTscores, width = 12, height = 6, dpi = 300)
par(bg = "white")

cor(ds$BFI_agreeableness,ds$BFI_closeones_agreeableness)




plot_labels <- c(
  "Z2", "Z7", "Z12", "Z17", "Z22", 
  "Z27", "Z32", "Z37", "Z42",
  "Zc2", "Zc7", "Zc12", 
  "Zc17", "Zc22", "Zc27", 
  "Zc32", "Zc37", "Zc42",
  "Ys", "Yc"
)

# Assign new labels to correlation matrix columns and rows for plot
colnames(correlation_matrix) <- plot_labels
rownames(correlation_matrix) <- plot_labels

install.packages("ggcorrplot")
library(ggcorrplot)
# Create a ggplot-based correlation plot
corTESTscores <- ggcorrplot(
  correlation_matrix,
  hc.order = FALSE,                    # Hierarchically cluster to show stronger correlations together
  type = "lower",                     # Show only lower triangle
  lab = TRUE,                         # Display correlation coefficients in the plot
  lab_size = 3,                       # Set the size of correlation labels
  colors = c("red", "white", "blue"), # Set color scale
  legend.title = "Correlation"
) +
  theme_minimal() +                   # Use a clean theme for better readability
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
    )

ggsave("corTESTscores.png", plot = corTESTscores, width = 12, height = 6, dpi = 300)


correlation_matrix["Z2",1:9] - corsubrho["Z2",]


correlation_matrix[1:9,1:9] - corsubrho

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
corFull
#TODO interpretation (page)
subdataset <- ds_og[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]
summary(ds_og)
str(subdataset)

# Convert columns to factors if they are not
subdataset[] <- lapply(subdataset, function(x) {
  if (is.numeric(x)) {
    factor(x, levels = 1:5, ordered = TRUE)
  } else {
    x
  }
})

corsub <- polychoric(as.data.frame(subdataset))
corsubrho <- corsub$rho

labels <- c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")

subdataset2 <- ds_og[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]


# Create a named list of labels for the plot
names(corsubrho) <- labels
rownames(corsubrho) <- labels
colnames(corsubrho) <- labels
rownames(subdataset2) <- labels
colnames(subdataset2) <- labels



# Plot using ggcorrplot
polychoricPlot <- ggcorrplot(
  corsubrho,
  hc.order = FALSE,                     # Hierarchically cluster to show stronger correlations together
  type = "lower",                      # Show only lower triangle
  lab = TRUE,                          # Display correlation coefficients in the plot
  lab_size = 3,                        # Set the size of correlation labels
  colors = c("red", "white", "blue"),  # Set color scale
  legend.title = "Correlation"
) +
  theme_minimal() +                    # Use a clean theme for better readability
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
ddgram <-ggdendrogram(data = hc)



ggsave("corTESTpoly.png", plot = polychoricPlot, width = 12, height = 6, dpi = 300)



#Clustering---------------------------------------------------------------------------
hc <- hclust(d = as.dist(1 - corsubrho), method = "ward.D2")
ggdendrogram(data = hc)
# Define custom labels
labels <- c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")



#Factor Analysis--------------------------------------------------------------
str(subdataset)
FA1 <- fa(r = corsubrho, nfactors = 1, fm = "ml")
FA1 <- fa(r = corsubrho, nfactors = 1, fm = "ml")
summary(FA1) 

subdataset2 <- subdataset2 %>%
  mutate(across(everything(), ~ factor(.x, levels = 1:5, ordered = TRUE)))


# Make sure data is numeric and within the correct range
subdataset2 <- subdataset2 %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Run factor analysis with polychoric correlation
FA2 <- fa(subdataset2, nfactors = 2, cor = "polychoric")

FA3 <- fa(subdataset2, nfactors = 3, cor = "polychoric",rotate = "none")
FA4 <- fa(subdataset2, nfactors = 4, cor = "polychoric",rotate = "none")


# Remove Z32 from subdataset2
subdataset2_no_Z32 <- subdataset2 %>% select(-Z32)

# Run factor analysis with polychoric correlation without Z32
FA2_no_Z32 <- fa(subdataset2_no_Z32, nfactors = 2, cor = "polychoric")


#goal: is to divide them into groups that correlates highest with each other
#factor: hidden variable; that affects observed values

# explain the individual factors + provide table -> each factor should represent something different, but not contrasting
# good rule of thumb is to see cumulative variance and pick highest ones (or BIC)
# h2 -> how well is the variance explained by the factors, u2 =1-h2
#com -> how many factors (complexity)



#M1: Z2, Z12, Z27, Z37
#M2: Z17, Z22, Z42
#M3: Z7, Z32

# h2 the worst: Z17, 
################################################################################xxx
#PCA analysis------------------------------------------------------------------------

# Convert all columns to numeric values
subdataset2 <- subdataset2 %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Compute the polychoric correlation matrix
polycor_matrix <- polychoric(subdataset2)$rho

# Run PCA on the polychoric correlation matrix
?principal
pca_result <- principal(polycor_matrix, nfactors = ncol(subdataset2), rotate = "none")
pca_result <- principal(corsubrho, nfactors = ncol(subdataset2), rotate = "none")

pca_result2 <- principal(corsubrho, nfactors = ncol(subdataset2)-6, rotate = "none")
# View the PCA summary
print(pca_result)
plot(pca_result$values, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", main = "Scree Plot")
plot(pca_result2$values, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", main = "Scree Plot")

#PC1, potentialy PC2 are strong contestant (loadings matrix)
#h2 says how well the variance is explained
#SS loadings -> how well does each principal component explains variance
#Proportional ->in %
#Mean item complexity ->each item related in PC 3
#Fit statistics RMSE = 0 -> perfect fit

#Interpretability
# PC 1 explains 42 % of the variance -> Z2, Z27, Z37 has high loading -> these items
#might reflect the core charactertics of agreee or related traits
# PC2 still not bad (14%)

#Community values:  higher ->PCA effectivelly captures the data
#Complexity of 3.1 -> items are influenced by multiple underlying factors (so we might not 
#measure only one thing)

#Math
#Loadings = eigenvalues of C covariance matrix
#Z2 = 0.72 -> Z2 contribues signif to the direction of the first principal compoennt 



# I could probably use variance already computed in the data
# Extract variance explained (proportion of variance) from pca_result
explained_variance <- pca_result$values / sum(pca_result$values)

# Create a data frame for plotting
variance_df <- data.frame(
  Principal_Component = seq_along(explained_variance),
  Variance_Explained = explained_variance
)
variance_df$Principal_Component <- factor(variance_df$Principal_Component)

# Create the bar plot with corrected x-axis labels
screeplot <- ggplot(variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = paste0("PC", 1:nrow(variance_df)))



ggsave("screeplot.png", plot = screeplot, width = 12, height = 6, dpi = 300)

#Variance Explained by Principal Components



first_pc_loadings <- pca_result$loadings[, 1]

# Plot as a bar plot
barplot(first_pc_loadings, main = "First Principal Component Loadings", 
        xlab = "Variables", ylab = "Loadings", col = "lightblue", 
        names.arg = colnames(corsubrho), las = 2, cex.names = 0.8)

# Alternatively, plot as a scatter plot
plot(first_pc_loadings, type = "p", main = "First Principal Component Loadings",
     xlab = "Variables", ylab = "Loadings", col = "blue", pch = 19, 
     xaxt = "n")  # suppress x-axis labels for customization
axis(1, at = 1:length(first_pc_loadings), labels = colnames(corsubrho), las = 2, cex.axis = 0.8)


# Extract loadings for the first two principal components and convert to a data frame
loadings_df <- data.frame(
  PC1 = pca_result$loadings[, 1],
  PC2 = pca_result$loadings[, 2],
  Variable = rownames(pca_result$loadings)
)


screeplot <- ggplot(variance_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = paste0("PC", 1:nrow(variance_df)))





# Create the scatter plot using ggplot2
loadings <- ggplot(loadings_df, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(color = "lightcoral", size = 3) +                     # Points in blue
  geom_text(hjust = 0.5, vjust = -0.5, color = "blue") +  # Labels in dark red
  labs(x = "PC1 Loadings", y = "PC2 Loadings") +
  theme_minimal()      


screeplotlod<- grid.arrange(screeplot, loadings, ncol = 2)

ggsave("screelodplot.png", plot = screeplotlod, width = 12, height = 6, dpi = 300)

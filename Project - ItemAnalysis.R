#BFI ANALYSIS
#ABE-KZA
#November 2024
##################################################################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Theme
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

###############################################################################
selected_columns <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42",
  "BFI_agreeableness", "BFI_closeones_agreeableness"
  
)
subdataset <- ds_og[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]

subdataset_close <-ds_close[,c("BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
                          "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
                          "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42")]
##############################################################################
# Calculate means and standard deviations
stats <- data.frame(
  Item = names(subdataset),
  Mean = sapply(subdataset, mean),
  SD = sapply(subdataset, sd),
  Group = "Dataset 1"
)

# Create the ggplot
ggplot(stats, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Mean and SD of Items",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For close ones
stats_close <- data.frame(
  Item = names(subdataset_close),
  Mean = sapply(subdataset_close, mean),
  SD = sapply(subdataset_close, sd),
  Group = "Dataset 2"
)


ggplot(stats_close, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Mean and SD of Items",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#TODO: rename the labels

#Plotted together
combined_stats <- rbind(stats, stats_close)

# Create the ggplot
ggplot(combined_stats, aes(x = Item, y = Mean, color = Group, shape = Group)) +
  geom_point(size = 3) +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Add error bars
  labs(
    title = "Mean and SD of Items Across Datasets",
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Custom colors

#################################################################
# Calculate the difference of means and combined standard deviation
difference_stats <- data.frame(
  Item = names(subdataset),
  MeanDifference = sapply(subdataset, mean) - sapply(subdataset_close, mean),
  SDDifference = sqrt(sapply(subdataset, sd)^2 + sapply(subdataset_close, sd)^2)  # Variance sum rule
)

# Create the ggplot for differences
ggplot(difference_stats, aes(x = Item, y = MeanDifference)) +
  geom_point(size = 3, color = "purple") +  # Plot mean differences
  geom_errorbar(aes(ymin = MeanDifference - SDDifference, ymax = MeanDifference + SDDifference), 
                width = 0.2, color = "black") +  # Add error bars
  labs(
    title = "Difference in Means with Combined SD",
    x = "Items",
    y = "Difference (Dataset1 - Dataset2)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########################################################################
#Normalizing ordinal data
# Define the function to calculate πˆi
calculate_norm <- function(column) {
  mean_col <- mean(column)            # Average item score Ȳ•i
  min_col <- min(column)              # Minimum possible score min(Yi)
  max_col <- max(column)              # Maximum possible score max(Yi)
  (mean_col - min_col) / (max_col - min_col)  # Scaled score
}

# Apply the function to each column
pi_hat <- sapply(subdataset, calculate_norm)

# Display the results
pi_hat #TODO: interpretation

###############################################################################
#Item difficulty 

# Function to calculate πˆi for maximal score
calculate_pi_max <- function(column) {
  max_col <- max(column)  # Maximal possible score
  mean(column == max_col) # Proportion of respondents with max score
}

# Function to calculate πˆi for a cutoff value ki
calculate_pi_cutoffs <- function(column, cutoffs) {
  # Calculate proportions for each cutoff
  sapply(cutoffs, function(cutoff) mean(column >= cutoff))
}

# Calculate πˆi for maximal score
pi_hat_max <- sapply(subdataset, calculate_pi_max)

# Define cut-off values for each item (example cut-offs)
cutoffs_list <- c(3,4,5)  # Set your cut-off values here

# Calculate πˆi for cut-off values
calculate_pi_cutoffs <- function(dataset, cutoffs) {
  # Create an empty data frame to store results
  results <- data.frame(matrix(ncol = length(cutoffs), nrow = ncol(dataset)))
  
  # Set column and row names
  colnames(results) <- paste0("Cutoff_", cutoffs)
  rownames(results) <- colnames(dataset)
  
  # Loop through columns (items)
  for (i in seq_along(dataset)) {
    # Get the current column
    column <- dataset[[i]]
    
    # Loop through cutoffs
    for (j in seq_along(cutoffs)) {
      # Calculate the proportion for the current cutoff
      results[i, j] <- mean(column >= cutoffs[j])
    }
  }
  
  return(results)
}

result_df <- calculate_pi_cutoffs(subdataset, cutoffs)

#################################################################################
#Item discrimination

#A. RIT -TODO: Incorrect?
subdataset$'BFI_total' <- ds$BFI_agreeableness
sapply(subdataset[,1:9], function(i) cor(i, subdataset$BFI_total))
ItemAnalysis(subdataset[,1:9])$RIT
#TODO: I do not know about the negative correlation, what should we do, is it okay?

#B. RIR - TODO: Incorrect?
dataR <- subdataset$BFI_total - subdataset[, 1:9]
diag(cor(subdataset[, 1:9], dataR))
ItemAnalysis(subdataset[,1:9])$RIR #different results?

#ULI
gDiscrim(Data = subdataset[, 1:9])
?gDiscrim
# k number of groups to be divided in
# l: lower group
# i: upper group
gDiscrim(Data = subdataset[, 1:9], k = 5, l = 2,u = 5)

##################################################################################
# Plotting 
DDplot(Data = subdataset[, 1:9], discrim = "ULI")
DDplot(Data = subdataset[, 1:9], discrim = "ULI", k = 5, l = 2, u = 5, thr = 0.1)
#Why it is negative??? Interpretation of that

#ICC #####################################################################################

# Create a function to calculate the proportion of correct answers for each item grouped by total score
calculate_proportion <- function(subdataset, total_score_column) {
  item_names <- colnames(subdataset)[-which(names(subdataset) == total_score_column)] # Exclude total score
  proportions <- data.frame()
  
  for (item in item_names) {
    # Create a data frame with total score and the item response
    data <- data.frame(total_score = subdataset[[total_score_column]], item_response = subdataset[[item]])
    
    # Group by total score and calculate the mean (proportion of correct answers) for each group
    item_proportion <- data %>%
      group_by(total_score) %>%
      summarize(proportion_correct = mean(item_response, na.rm = TRUE))
    
    # Add item name
    item_proportion$item <- item
    proportions <- rbind(proportions, item_proportion)
  }
  
  return(proportions)
}

# Calculate proportions for each item
proportions_data <- calculate_proportion(subdataset, "BFI_total")

# Plot the ICCs using ggplot2
ggplot(proportions_data, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Item Characteristic Curves (ICCs)",
    x = "Total Score",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

#for normalizing we could use probably the pi's above
########################################################################################
#Distractors#######################################


#######################################################################################
#Realibity

psych::alpha(x = subdataset[,1:9])$total[1]
psych::alpha(x = subdataset[,1:9])$alpha.drop[, 1]

#TODO: do it better -> incorrect
ItemAnalysis(Data = subdataset[,1:9])$Alpha.drop

#########################################################################################
#Item Validity
ItemAnalysis(Data = subdataset[,1:9], criterion = ds_close$BFI_closeones_agreeableness)$Corr.criterion
DDplot(Data = subdataset[,1:9], criterion = ds_close$BFI_closeones_agreeableness, thr = NULL)
#######################################################################################
#Ordinal logistic regression
library(foreign)
library(ggplot2)
library(reshape2)
library(MASS)
library(Hmisc)
library(mirt)
# We model E[Y_pi | X_p, Xi_i], Y_pi item score of person p in item i, X_p observed ability of person p, Xi_i parameter of the item i

# Compute the mean and standard deviation of the total scores
mean_total <- mean(ds_rev$BFI_agreeableness, na.rm = TRUE)
sd_total <- sd(ds_rev$BFI_agreeableness, na.rm = TRUE)

# Compute z-scores
ds_rev$BFI_agreeableness_z <- (ds_rev$BFI_agreeableness - mean_total) / sd_total
zscore <- ds_rev$BFI_agreeableness_z
A_items <- subdataset - 1

for (label in labels) {
  A_items[[label]] <- ordered(factor(A_items[[label]], 
                                     levels = 0:max(A_items[[label]], na.rm = TRUE)))
}
str(A_items)

#--------------------------------------------------------------------------------
# Cumulative
library(VGAM)

# Create a list to store the model fits
model_fits <- list()

# Loop over each label
for (label in labels) {
  # Fit the cumulative model for the current item
  model_fits[[label]] <- vglm(A_items[[label]] ~ zscore, 
                              family = cumulative(reverse = TRUE, parallel = TRUE))
  
  # Optionally, print a summary of the fit
  cat("\nSummary for:", label, "\n")
  print(summary(model_fits[[label]]))
}

m12 <- model_fits[['Z12']]
# IRT Parametrization
c(-coef(m12)[1:4] / coef(m12)[5], coef(m12)[5])

deltamethod(list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
            mean = coef(m12), cov = vcov(m12))

library(ShinyItemAnalysis)
plotCumulative(m12, type = "cumulative") #Interpretation to do

# more detailed - TODO
plotCumulative(
  m12,
  type = "cumulative", matching.name = "Z-score"
)  +
  theme_fig() + 
  xlim(-1.1, 5.2) + 
  theme(legend.position = c(0.79, 0.23),
        legend.box = "horizontal",
        legend.margin = margin(0, -5, 0, 0),
        legend.background = element_blank())  


plotCumulative(m12, type = "category")
#-------------------------------------------------------------------------------
# Adjacent
model_fits_a <- list()

for (label in labels) {
  # Fit the cumulative model for the current item
  model_fits_a[[label]] <- vglm(A_items[[label]] ~ zscore, 
                              family = acat(reverse = FALSE, parallel = TRUE))
  
  # Optionally, print a summary of the fit
  cat("\nSummary for:", label, "\n")
  print(summary(model_fits_a[[label]]))
}


m12_a <- model_fits_a[['Z12']]
c(-coef(m12_a)[1:4] / coef(m12_a)[5], coef(m12_a)[5])
deltamethod(list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
            mean = coef(m12_a), cov = vcov(m12_a))


plotAdjacent(m12_a, matching.name = "Z-score")

#joint model + person item map
#??? Can I use that?

###########################################################################################¨
#########################################################################################################
#Chapter 7: IRT Models


#A:mirt -> probably use this package
#For us is relevant chapter 8
library(mirt)
# Convert the matrix of ordered factors (A_items) to numeric
A_items_numeric <- as.data.frame(lapply(A_items, function(x) as.numeric(as.character(x))))

# Check the structure of the resulting numeric data
str(A_items_numeric)

fit.grm.mirt <- mirt(A_items_numeric, model = 1, itemtype = 'graded', SE = TRUE) 
fit.grm.mirt2 <- mirt(A_items_num_merge, model = 1, itemtype = 'graded', SE = TRUE, dentype = 'EH') 
summary(fit.grm.mirt)
summary(fit.grm.mirt2)

coef(fit.grm.mirt, IRTpars = TRUE, simplify = TRUE)
coef(fit.grm.mirt2, IRTpars = TRUE, simplify = TRUE)


response_counts <- apply(A_items, 2, function(x) table(factor(x, levels = 0:4)))

#Info about the package
?mirt
#uses Maximum Likelihood
#latent trait regression
#model = 1 -> unidimensional
#itemtype = graded -> GRM, grsm -> GRSM (graded rated scaled model)
#gpcm -> generalized parcial credit model 
#method = EM (default) - okay for us, who chose 2FA
#dentype = Gaussian (default), maybe EH would be better?

#the formula is present in the description

#--------------------------------------------------------------------------------
#Plotting from book

plot(fit.grm.mirt, type = 'trace') #Item probability functions
#Y axis P(theta) - prob of selecting particular response for the item
#X axis latent trait theta (estimate)
#each graph = one item, one subplot = different categories
#curve = how likely a person is to choose a specific response at different levels of latent trait

# item response curves (IRC) for item 1 using function itemplot()
itemplot(fit_GRM_mirt, item = 1, type = "trace")

# item score function for item 1
itemplot(fit_GRM_mirt, item = 1, type = "score")

# item information curve (IIC) and SE for item 1
itemplot(fit_GRM_mirt, item = 1, type = "info")

# IRC combined with IIC for item 1
itemplot(fit_GRM_mirt, item = 1, type = "infotrace")

# item information curve (IIC) and SE for item 1
itemplot(fit_GRM_mirt, item = 1, type = "infoSE")
#--------------
# further test plots (not displayed in the book):
# test score curve
plot(fit_GRM_mirt)
# test score curve with 95% CI
plot(fit_GRM_mirt, MI = 200)

# test information curve (TIC)
plot(fit_GRM_mirt, type = "info") # test information
plot(fit_GRM_mirt, type = "infoSE") # test information and SE


#Latent ability estimates
fSE <- fscores(fit.grm.mirt, full.scores.SE = TRUE) #Find predicted scores for the latent traits using estimation methods
fSE <- as.data.frame(fSE)

# Then proceed with creating the db dataframe
db <- data.frame(
  F1 = fSE$F1, 
  SE = fSE$SE_F1, 
  numbers = 1:nrow(fSE),
  Lower =  fSE$F1 - 1.96 * fSE$SE_F1,
  Upper =  fSE$F1 + 1.96 * fSE$SE_F1
)

# Plot
ggplot(db, aes(x = reorder(numbers, F1), y = F1)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "red") +
  labs(
    title = "Sorted F1 Scores with Confidence Intervals",
    x = "Index",
    y = "F1 Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 5)  # Rotate x-axis labels
  )
#TODO: merge the categories 0 and 1

#################################################################################

#B: lmt
#library(ltm)
#fit.grm.lmt <- grm(A_items_numeric)
#coef(fit.grm.lmt)
#?grm
#--------------------------------------------------------------------------------
#plot(fit.grm.lmt, type = 'OCCu',items = 9) #for individual item
#plot(fit.grm.lmt, type = 'ICC',items = 9)

#ltm::factor.scores(fit.grm.lmt, resp.patterns = A_items_numeric)
#ltm::factor.scores(fit.grm.lmt, resp.patterns = t(as.matrix(rep(1,9))))
##################################################################################
A_items_num_merge <- A_items_numeric

# Using apply to modify all columns
A_items_num_merge <- apply(A_items_num_merge, 2, function(x) ifelse(x == 0, 1, x))
str(A_items_num_merge)
# Convert back to data frame if needed
A_items_num_merge <- as.data.frame(A_items_num_merge)
A_items_num_merge <- A_items_num_merge - 1

#GRCMx
fit.GRSMirt.mirt <- mirt(A_items_num_merge, model = 1, itemtype = "grsmIRT")
coef(fit.GRSMirt.mirt, simplify = TRUE)
anova(fit.GRSMirt.mirt, fit.grm.mirt2) #I do not think we can compare those

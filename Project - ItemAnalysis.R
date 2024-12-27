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
subdataset <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]

colnames(subdataset) <- c("Z2", "Z7", "Z12", "Z17", 
                          "Z22", "Z27", "Z32", "Z37", 
                          "Z42")

# Corrected code: Subset only the columns that exist
subdataset_close <- ds_rev_close[, c("BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
                                 "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
                                 "BFI_closeones_32","BFI_closeones_37", "BFI_closeones_42")]

colnames(subdataset_close) <- c("Zc2", "Zc7", "Zc12", "Zc17", 
                          "Zc22", "Zc27", "Zc32", "Zc37", 
                          "Zc42")

subdataset2 <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                         "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                         "BFI_42")]
colnames(subdataset2) <- c("Zc2", "Zc7", "Zc12", "Zc17", 
                                "Zc22", "Zc27", "Zc32", "Zc37", 
                                "Zc42")


colnames(ds_close)
##############################################################################
# Calculate means and standard deviations
stats <- data.frame(
  Item = names(subdataset),
  Mean = sapply(subdataset, mean),
  SD = sapply(subdataset, sd),
  Group = "Self"
)

# Create the ggplot
ggplot(stats, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
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
  Group = "Close"
)


ggplot(stats_close, aes(x = Item, y = Mean)) +
  geom_point(size = 3, color = "blue") +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "black") +  # Add error bars
  labs(
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#TODO: rename the labels

#Plotted together
combined_stats <- rbind(stats, stats_close)

# Create the ggplot
itemBasic <- ggplot(combined_stats, aes(x = Item, y = Mean, color = Group, shape = Group)) +
  geom_point(size = 3) +  # Plot means
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +  # Add error bars
  labs(
    x = "Items",
    y = "Mean with Confidence Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))  # Custom colors

ggsave("itemsBasic.png", plot = itemBasic, width = 12, height = 6, dpi = 300)


#################################################################
# Calculate the difference of means and combined standard deviation
difference_stats <- data.frame(
  Item = names(subdataset[1:9]),
  MeanDifference = sapply(subdataset[1:9], mean) - sapply(subdataset_close[1:9], mean),
  SDDifference = sqrt(sapply(subdataset[1:9], sd)^2 + sapply(subdataset_close[1:9], sd)^2)  # Variance sum rule
)

# Create the ggplot for differences
itemdif <- ggplot(difference_stats, aes(x = Item, y = MeanDifference)) +
  geom_point(size = 3, color = "blue") +  # Plot mean differences
  geom_errorbar(aes(ymin = MeanDifference - SDDifference, ymax = MeanDifference + SDDifference), 
                width = 0.2, color = "black") +  # Add error bars
  labs(
    x = "Items",
    y = "Difference (Self - Close)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("itemsdif.png", plot = itemdif, width = 12, height = 6, dpi = 300)

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
pi_hat[1:9] #TODO: interpretation

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
cutoffs_list <- c(3,4)  # Set your cut-off values here

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

result_df <- calculate_pi_cutoffs(subdataset, cutoffs_list)

#Neutral items---------------------------------------------------------------------------
# Count the number of '3's (neutral opinions) for each item in the subdataset
neutral_counts <- sapply(subdataset, function(column) sum(column == 3, na.rm = TRUE))

# Convert the result into a data frame for better readability
neutral_counts_df <- data.frame(Item = names(neutral_counts), Neutral_Count = neutral_counts)

# Calculate the total number of respondents (assuming all columns have the same number of rows)
total_respondents <- nrow(subdataset)

# Add a new column for the proportion of neutral responses
neutral_counts_df$Proportion_Neutral <- neutral_counts_df$Neutral_Count / total_respondents

# Print the updated data frame
print(neutral_counts_df)


# Count the neutral responses (3) for each item in subdataset_close
neutral_counts_close <- sapply(subdataset_close, function(column) sum(column == 3, na.rm = TRUE))

# Convert the result into a data frame for better readability
neutral_counts_close_df <- data.frame(Item = names(neutral_counts_close), Neutral_Count = neutral_counts_close)

# Calculate the total number of respondents (rows) in subdataset_close
total_respondents_close <- nrow(subdataset_close)

# Add a column for the proportion of neutral responses
neutral_counts_close_df$Proportion_Neutral <- neutral_counts_close_df$Neutral_Count / total_respondents_close

# Print the results
print(neutral_counts_close_df)






#################################################################################
#Item discrimination
library(ShinyItemAnalysis)
#A. RIT -TODO: Incorrect?
#subdataset$'BFI_total' <- ds$BFI_agreeableness
subdataset$partTotal <-rowSums(subdataset[, c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")])
#lol not needed
sum(subdataset$partTotal) == sum(ds_rev$BFI_agreeableness) #equivalencec

sapply(subdataset[,1:9], function(i) cor(i, subdataset$partTotal))

ItemAnalysis(subdataset[,1:9])$RIT
#TODO: I do not know about the negative correlation, what should we do, is it okay?

#B. RIR - TODO: Incorrect?
dataR <- subdataset$partTotal - subdataset[, 1:9]
diag(cor(subdataset[, 1:9], dataR))
ItemAnalysis(subdataset[,1:9])$RIR #different results?

#ULI
gDiscrim(Data = subdataset[, 1:9])
?gDiscrim
# k number of groups to be divided in
# l: lower group
# i: upper group
gDiscrim(Data = subdataset[, 1:9], k = 5, l = 2,u = 5) #???

hist(subdataset$partTotal)
##################################################################################
# Plotting 
DDplot(Data = subdataset[, 1:9], discrim = "ULI", k = 3) #based 3 groups
DDplot(Data = subdataset[, 1:9], discrim = "ULI")
DDplot(Data = subdataset[, 1:9], discrim = "ULI", k = 3, l = 1, u = 3, thr = 0.1)
#Why it is negative??? Interpretation of that

#ICC #####################################################################################

# Create a function to calculate the proportion of correct answers for each item grouped by total score
library(dplyr)
library(ggplot2)
library(tidyr)
# Dichotomize the 9 items
subdataset_dichotomized <- subdataset[, 1:9] %>%
  mutate(across(everything(), ~ case_when(
    . %in% c(1, 2, 3) ~ 0,   # Values 1, 2, 3 become 0
    . %in% c(4, 5) ~ 1,      # Values 4, 5 become 1
    TRUE ~ .                 # Keep other values unchanged (if any)
  )))

# Calculate the total score for the 9 items
subdataset_dichotomized$total_score <- rowSums(subdataset_dichotomized)

# Select Z7 and Z17
selected_items <- subdataset_dichotomized# %>%
  #select(total_score, Z7, Z17)
  

# Compute proportions for each item
proportions_data <- selected_items %>%
  pivot_longer(cols = c(Z2, Z7, Z12, Z17, Z22, Z27, Z32, Z37, Z42), names_to = "item", values_to = "response") %>%
  group_by(total_score, item) %>%
  summarize(
    proportion_correct = mean(response, na.rm = TRUE), # Proportion of "1"s
    .groups = "drop"
  )

proportions_data2 <- selected_items %>%
  pivot_longer(cols = c(Z7, Z17), names_to = "item", values_to = "response") %>%
  group_by(total_score, item) %>%
  summarize(
    proportion_correct = mean(response, na.rm = TRUE), # Proportion of "1"s
    .groups = "drop"
  )


# Plot ICC
iccs2 <- ggplot(proportions_data, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Total Score of Dichotomized Agreeableness Related Items",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


iccs <- ggplot(proportions_data2, aes(x = total_score, y = proportion_correct, color = item)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Total Score of Dichotomized Agreeableness Related Items",
    y = "Proportion of Correct Answers"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave("iccs.png", plot = iccs, width = 12, height = 6, dpi = 300)
dim(subdataset)

#######################################################################################
#Realibity

psych::alpha(x = subdataset[,1:9])$total[1]
psych::alpha(x = subdataset[,1:9])

#TODO: do it better -> incorrect
ItemAnalysis(Data = subdataset[,1:9])$Alpha.drop

#########################################################################################
#Item Validity
# Do the partial sum
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
A_items <- subdataset[1:9] - 1
labels <- colnames(A_items)  # Or define it accordingly
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
                              family = cumulative(reverse = TRUE, parallel = TRUE)) #  parallel = TRUE implies the proportional odds assumption, meaning the effect of predictors (coefficients for zscore) is constant across all thresholds.
  
  # Optionally, print a summary of the fit
  cat("\nSummary for:", label, "\n")
  print(summary(model_fits[[label]]))
}
?vglm
m12 <- model_fits[['Z12']]

m7 <- model_fits[['Z7']]
m17 <- model_fits[['Z17']]
summary(m7)
summary(m17)
# IRT Parametrization
model <- m7
drop1(m17, test = "Chisq")
library(lmtest)
lrtest_vglm(m7)
lrtest_vglm(m17)

exp(coef(model))


c(-coef(model)[1:4] / coef(model)[5], coef(model)[5])
library(msm)
deltamethod(list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
            mean = coef(model), cov = vcov(model))



library(ShinyItemAnalysis)
library(gridExtra)
cumu17 <- plotCumulative(m17, type = "cumulative",matching.name = "Standardized Total Score, item Z17") #Interpretation to do
cumu7 <- plotCumulative(m7, type = "cumulative",matching.name = "Standardized Total Score, item Z7")

cumuPlot<- grid.arrange(cumu7, cumu17, ncol = 2)
ggsave("cumuPlot.png", plot = cumuPlot, width = 12, height = 6, dpi = 300)
?plotCumulative


cumucat17 <- plotCumulative(m17, type = "category", matching.name = "Standardized Total Score, item Z17")
cumucat7 <- plotCumulative(m7, type = "category", matching.name = "Standardized Total Score, item Z7")
cumucatPlot<- grid.arrange(cumucat7, cumucat17, ncol = 2)
ggsave("cumucatPlot.png", plot = cumucatPlot, width = 12, height = 6, dpi = 300)



count_answers <- table(subdataset2$Zc7)
count_answers2 <- table(subdataset2$Zc17)

prop.table(count_answers)
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
?mirt
fit.grm.mirt <- mirt(A_items_numeric, model = 1, itemtype = 'graded', SE = TRUE) 
fit.grm.mirt_2F <- mirt(A_items_numeric, model = 2, itemtype = 'graded', SE = TRUE) 

fit.grm.mirt2 <- mirt(A_items_num_merge, model = 1, itemtype = 'graded', SE = TRUE, dentype = 'EH') 


summary(fit.grm.mirt)
summary(fit.grm.mirt_2F)
anova(fit.grm.mirt,fit.grm.mirt_2F)

summary(fit.grm.mirt2)
coef(fit.grm.mirt, simplify = TRUE)
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
#method = EM (default) - okay for us,because we chose 2FA
#dentype = Gaussian (default)

#the formula is present in the description

#--------------------------------------------------------------------------------
#Plotting from book

plot(fit.grm.mirt, type = 'trace') #Item probability functions
#Y axis P(theta) - prob of selecting particular response for the item
#X axis latent trait theta (estimate)
#each graph = one item, one subplot = different categories
#curve = how likely a person is to choose a specific response at different levels of latent trait

# item response curves (IRC) for item 1 using function itemplot()
par(mfrow = c(2,2))

itemplot(fit.grm.mirt, item = 'Z17', type = "trace") #individual item
# item score function for item 1
#So what score in E() does correspond to latent trait
#In this case: Folks with negative latent trait (?not agreeable) are likely to answer 0
#Full range, good info

# item information curve (IIC) and SE for item 1
itemplot(fit.grm.mirt, item = 'Z17', type = "info")
# tem information indicates how well an item discriminates between different levels of the latent trait.
#High information means that the item provides precise measurement for people at a particular level of the latent trait. This is ideal for accurately estimating the trait at that level.
#Full range, good info; peak around -2,2 ->we get reliable info in this interval
#symmetric around

# IRC combined with IIC for item 1
#itemplot(fit.grm.mirt,  item = 'Z17', type = "infotrace")
itemplot(fit.grm.mirt, item = 'Z17', type = "score")
# item information curve (IIC) and SE for item 1
itemplot(fit.grm.mirt, item = 'Z17', type = "infoSE")
?itemplot
plot1 <- itemplot(fit.grm.mirt, item = 'Z17', type = "trace")
plot2 <- itemplot(fit.grm.mirt, item = 'Z17', type = "score")
plot3 <- itemplot(fit.grm.mirt, item = 'Z17', type = "infotrace")  # Uncomment this line if you want this plot
plot4 <- itemplot(fit.grm.mirt, item = 'Z17', type = "infoSE")
library(gridExtra)
# Create a 2x2 grid of plots
griditems <- grid.arrange(plot2, plot4, ncol = 2)

# Save the 2x2 grid as a PNG image
ggsave("griditems.png",griditems,  width = 12, height = 6)
#The standard error is inversely related to the information: smaller SE values correspond to higher precision

#--------------
# further test plots (not displayed in the book):
# expected test score curve
plot(fit.grm.mirt)
 
# test score curve with 95% CI
plot5 <- plot(fit.grm.mirt, MI = 200)
plot6 <- plot(fit.grm.mirt, type = "infoSE")

# Create a 1x2 grid of plots
gridTotal <- grid.arrange(plot5, plot6, ncol = 2)

# Save the 1x2 grid as a PNG image
ggsave("gridTotal.png",gridTotal, width = 12, height = 6)

# test information curve (TIC)
plot(fit.grm.mirt, type = "info") # test information
plot(fit.grm.mirt, type = "infoSE") # test information and SE


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
library(ggplot2)
# Plot
f1scores <-  ggplot(db, aes(x = reorder(numbers, F1), y = F1)) +
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
ggsave("f1scores.png", plot = f1scores, width = 12, height = 6, dpi = 300)
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



itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "trace") #individual item
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "score")
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "info")
itemplot(fit.GRSMirt.mirt,  item = 'Z17', type = "infotrace")
itemplot(fit.GRSMirt.mirt, item = 'Z17', type = "infoSE")


plot(fit.GRSMirt.mirt, type = 'trace')
plot(fit.GRSMirt.mirt, MI = 200)
plot(fit.GRSMirt.mirt, type = "infoSE") # test information and SE

################################################################################
#Dichom rasch
fit_rasch_mirt <- mirt(data = subdataset_dichotomized[,-10], model = 1, itemtype = "Rasch", 
                       SE = TRUE)
coef(fit_rasch_mirt, simplify = TRUE)
coef(fit_rasch_mirt, printSE = TRUE)
coef(fit_rasch_mirt, IRTpars = TRUE, simplify = TRUE)
summary(fit_rasch_mirt)

# TSC (test score curve)
plot(fit_rasch_mirt)
#--------------

#--------------
# ICC
plot(fit_rasch_mirt, type = "trace", facet_items = FALSE)
#--------------

#--------------
# further plots (not displayed in the book)
plot(fit_rasch_mirt, type = "trace", facet_items = TRUE) # ICC separately
plot(fit_rasch_mirt, type = "infotrace", facet_items = FALSE) # Item information curves (IIC)
plot(fit_rasch_mirt, type = "infotrace", facet_items = TRUE) # IICs separately
plot(fit_rasch_mirt, type = "info", facet_items = FALSE) # Test information curve (TIC)
plot(fit_rasch_mirt, type = "infoSE", facet_items = FALSE) # TIC and SE
#--------------

#--------------
# latent abilities (factor scores) with SE
fs_rasch_mirt_SE <- fscores(fit_rasch_mirt, full.scores.SE = TRUE)
head(fs_rasch_mirt_SE, n = 3)

fit_2PL_mirt <- mirt(data = subdataset_dichotomized[,-10], model = 1, itemtype = "2PL")

##################################################################################
ability_estimates <- fscores(fit.grm.mirt)
# Item difficulty parameters from the model
item_difficulties <- coef(fit.grm.mirt, IRTpars = TRUE)$items[, 1]  # Extract item difficulties

# Create a data frame for plotting the Wright map
wright_map_data <- data.frame(
  Ability = ability_estimates[, 1],   # Latent trait (ability)
  ItemDifficulty = item_difficulties  # Item difficulty
)

# Plot the Wright map
ggplot(wright_map_data, aes(x = Ability, y = ItemDifficulty)) +
  geom_point(aes(color = Ability), size = 3) +  # Use color to show ability
  scale_color_gradient(low = "blue", high = "red") +  # Gradient for abilities
  theme_minimal() +
  labs(x = "Ability (Theta)", y = "Item Difficulty", title = "Wright Map")





#TODO MUltiple Analysis Model 
#Choose two traits and agggreableness and something else and do it




table(subdataset_dichotomizedDIF$Z7)
#################################################################################¨
##################################################################################

#DIF
subdataset_dichotomizedDIF <- subdataset_dichotomized
subdataset_dichotomizedDIF$Western <- ds_rev$Western
library(dplyr)
subdataset_dichotomizedDIF <- subdataset_dichotomizedDIF[, !(names(subdataset_dichotomizedDIF) == "total_score")]


counts_ones <- subdataset_dichotomizedDIF %>%
  group_by(Western) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE), .names = "count_{col}"))


str(subdataset_dichotomizedDIF)
#Western = focal group

library(deltaPlotR)

#Does not work---------------------------------------------------------------------------
# delta plot using fixed threshold
?deltaPlot
(DP_fixed <- deltaPlot(data = subdataset_dichotomizedDIF, group = 'Western', focal.name = 1, thr = 1.5))
(DP_norm <- deltaPlot(data = subdataset_dichotomizedDIF, group = 'Western', focal.name = 1, 
                      thr = "norm"))

deltaPlot(data = subdataset_dichotomizedDIF, group = "Western", focal.name = 1, 
          thr = "norm", purify = TRUE)

#Computation by hand from the book
#         Z2          Z7         Z12         Z17         Z22         Z27         Z32         Z37 
#-0.41904951  0.67332160  0.02698986  0.28395407 -0.07772496 -0.26801754 -0.32149268 -0.09644839 
#Z42 
#0.19846756 

#----------------------------------------------------------------------------------------



total_score_value <- 1

# Subset the data for total_score == 6
subset_data <- subdataset_dichotomizedDIF[subdataset_dichotomizedDIF$total_score == total_score_value, ]
subdataset
# Define the items you're interested in
items <- c('Z7')

# Initialize a list to store contingency tables
contingency_tables <- list()

# Loop through each item and calculate the contingency table
for (item in items) {
  contingency_table <- matrix(0, nrow = 2, ncol = 2,
                              dimnames = list(c("Reference group (0)", "Focal group (1)"),
                                              c("Yi = 1", "Yi = 0")))
  
  # Calculate the counts for the reference group (group == 0)
  ref_group <- subset_data[subset_data$Western == 0, ]
  contingency_table["Reference group (0)", "Yi = 1"] <- sum(ref_group[[item]] == 1)
  contingency_table["Reference group (0)", "Yi = 0"] <- sum(ref_group[[item]] == 0)
  
  # Calculate the counts for the focal group (group == 1)
  focal_group <- subset_data[subset_data$Western == 1, ]
  contingency_table["Focal group (1)", "Yi = 1"] <- sum(focal_group[[item]] == 1)
  contingency_table["Focal group (1)", "Yi = 0"] <- sum(focal_group[[item]] == 0)
  
  # Store the contingency table in the list
  contingency_tables[[item]] <- contingency_table
}

summary(subdataset_dichotomizedDIF)

# Print contingency tables for all items
contingency_tables
library(difR)#--------------
?difMH
difModel <- difMH(Data = subdataset_dichotomizedDIF, group = "Western", focal.name = 1,match = "score",
                  p.adjust.method = "none", purify = FALSE)

table(subdataset_dichotomizedDIF$Western)

create_contingency_tables <- function(data, items, group_column, score_column) {
  # Initialize a list to store contingency tables for each item and total score
  contingency_tables <- list()
  
  # Loop through each item
  for (item in items) {
    item_tables <- list()
    
    # Get unique total scores
    total_scores <- unique(data[[score_column]])
    
    # Loop through each total score
    for (score in total_scores) {
      # Subset data for the current total score
      subset_data <- data[data[[score_column]] == score, ]
      
      # Initialize contingency table for the current score and item
      contingency_table <- matrix(0, nrow = 2, ncol = 2,
                                  dimnames = list(c("Reference group (0)", "Focal group (1)"),
                                                  c("Yi = 1", "Yi = 0")))
      
      # Calculate counts for the reference group (group == 0)
      ref_group <- subset_data[subset_data[[group_column]] == 0, ]
      contingency_table["Reference group (0)", "Yi = 1"] <- sum(ref_group[[item]] == 1, na.rm = TRUE)
      contingency_table["Reference group (0)", "Yi = 0"] <- sum(ref_group[[item]] == 0, na.rm = TRUE)
      
      # Calculate counts for the focal group (group == 1)
      focal_group <- subset_data[subset_data[[group_column]] == 1, ]
      contingency_table["Focal group (1)", "Yi = 1"] <- sum(focal_group[[item]] == 1, na.rm = TRUE)
      contingency_table["Focal group (1)", "Yi = 0"] <- sum(focal_group[[item]] == 0, na.rm = TRUE)
      
      # Store the contingency table for the current score
      item_tables[[as.character(score)]] <- contingency_table
    }
    
    # Store all contingency tables for the current item
    contingency_tables[[item]] <- item_tables
  }
  
  return(contingency_tables)
}

# Example usage:
contingency_tables <- create_contingency_tables(
  data = subdataset_dichotomizedDIF,
  items = c("Z32"),
  group_column = "Western",
  score_column = "total_score"
)

plotMHDIF <- plot.MH(difModel, names = names_list)
ggsave("plotMHDIF.png", plot = plotMHDIF, width = 12, height = 6, dpi = 300)

##########SIBTEST#########TRUE##########SIBTEST########################################################
  difSIBTEST(Data = subdataset_dichotomizedDIF, group = "Western", focal.name = 1)
#No diff

difSIBTEST(Data = subdataset_dichotomizedDIF, group = "Western", focal.name = 1, type = "nudif")


lapply(1:9, function(i)
  SIBTEST(dat = subdataset_dichotomizedDIF[, 1:9], group = subdataset_dichotomizedDIF$Western, suspect_set = i))


###########################################################################
#Regression Models
library(difNLR)
fit_ORD1 <- difORD(Data = A_items_numeric, group = subdataset_dichotomizedDIF$Western, focal.name = 1, model = "cumulative")
summary(fit_ORD1)
coef(fit_ORD1, SE = TRUE, CI =0)$Z17
coef(fit_ORD1, SE = TRUE, CI =0)$Z32

  

par(mfrow = c(2,2))
plot(fit_ORD1, item = "Z17", plot.type = "cumulative",
     group.names = c("Eastern", "Western"))
plot(fit_ORD1, item = "Z32", plot.type = "cumulative",
     group.names = c("Eastern", "Western"))

plot(fit_ORD1, item = "Z17", plot.type = "category",
     group.names = c("Eastern", "Western"))
plot(fit_ORD1, item = "Z32", plot.type = "category",
     group.names = c("Eastern", "Western"))


combined_plot_DIF<- grid.arrange(pl1, pl2, ncol = 2)
ggsave("combined_plot_DIF.png", plot = combined_plot_DIF, width = 12, height = 6, dpi = 300)


predict(fit_ORD1, item = "Z17", match = 0, group = c(0, 1))

predict(fit_ORD1, item = "Z17", match = 0, group = c(0, 1), type = "cumulative")


#IRT
library(lordif)
lordiffModel <- lordif(resp.data = A_items_numeric, group = subdataset_dichotomizedDIF$Western, alpha = 0.05)
summary(lordiffModel)

lordiffModel$ipar.sparse


ShinyItemAnalysis::run_app()


subdataset_export <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                                             "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                                             "BFI_42")]


colnames(subdataset_export) <- c("Z2", "Z7", "Z12", "Z17", 
                          "Z22", "Z27", "Z32", "Z37", 
                          "Z42")


names_list <- c("Z2", "Z7", "Z12", "Z17", 
           "Z22", "Z27", "Z32", "Z37", 
           "Z42")

write.table(subdataset_export, 
            file = "data_export.csv", 
            sep = ",",          # Custom separator
            row.names = FALSE,  # Exclude row names
            col.names = TRUE,   # Include column names
            quote = TRUE)       # Quote character or factor data


subdataset_export_Western <- ds_rev[, c("Western")]
dim(subdataset_export)



write.table(subdataset_export_Western, 
            file = "data_export_western.csv", 
            sep = ",",          # Custom separator
            row.names = FALSE,  # Exclude row names
            col.names = TRUE,   # Include column names
            quote = TRUE)       # Quote character or factor data






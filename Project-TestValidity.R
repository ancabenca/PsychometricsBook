#BFI ANALYSIS
#ABE-KZA
#November 2024
################################################################################
#CH2 + 3 - Validity testing
library(psychometric)
library(ggplot2) 
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
library(corrplot)
library(ggcorrplot)


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

################################################################################

##############################################################################
#Chapter 2: Validity
summary(ds)

#BASIC OVERVIEW FOR ALL TRAITS------------------------------------------------

#A. Extraversion
combined_data <- data.frame(
  score = c(ds_rev$BFI_extraversion, ds_rev_close$BFI_closeones_extraversion),
  group = factor(rep(c("Self", "Close"), 
                     c(length(ds_rev$BFI_extraversion), length(ds_rev_close$BFI_closeones_extraversion))))
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


var <- ds$"BFI_extraversion"
var2 <- ds$"BFI_closeones_extraversion"

plot(var)
plot(var2)
hist(var) 
hist(var2)
var.test(var,var2) #test for equal variance

################################################################################
###################################################################################
#CORRELATION ANALYSIS

#Assumptions-------------------------------------------------------------------

hist(ds$BFI_agreeableness) #could make finer boxes
hist(ds$BFI_closeones_agreeableness) #hmmm, not really normal
hist(ds$BFI_agreeableness + ds$BFI_closeones_agreeableness) #rather heavytailed

#------------------------------------------------------------------------------

ggplot(ds, aes(x = var1, y = var2)) +
  geom_point() + theme_fig() +
  xlab("Self") + ylab("Close") #nice,there is some relantionship visible?

var1 <- ds$BFI_agreeableness
var2 <- ds$BFI_closeones_agreeableness
cor.test(var1, var2) #correlated


#-------------------------------------------------------------------
ggplot(ds, aes(x = BFI_extraversion, y = BFI_agreeableness)) +
  geom_point() + theme_fig() +
  xlab("extraversion") + ylab("aggreableness") #nice,there is some relantionship visible?

var1 <- ds$BFI_extraversion
var2 <- ds$BFI_agreeableness
cor.test(var1, var2) #correlated


selected_columns <- c(
  "BFI_2", "BFI_7", "BFI_12", "BFI_17", "BFI_22", 
  "BFI_27", "BFI_32", "BFI_37", "BFI_42",
  "BFI_closeones_2", "BFI_closeones_7", "BFI_closeones_12", 
  "BFI_closeones_17", "BFI_closeones_22", "BFI_closeones_27", 
  "BFI_closeones_32", "BFI_closeones_37", "BFI_closeones_42",
  "BFI_agreeableness", "BFI_closeones_agreeableness"
  
)

# Create the correlation matrix ->normal Pearson
correlation_matrix <- cor(ds[, selected_columns], use = "complete.obs")
plot_labels <- c(
  "Z2", "Z7", "Z12", "Z17", "Z22", 
  "Z27", "Z32", "Z37", "Z42",
  "Zc2", "Zc7", "Zc12", 
  "Zc17", "Zc22", "Zc27", 
  "Zc32", "Zc37", "Zc42",
  "Ys", "Yc"
)
colnames(correlation_matrix) <- plot_labels #better labels for the plot
rownames(correlation_matrix) <- plot_labels

#----------------------------------------------------------------------------
# Plot the correlation matrix (This is not really correct ->need polychoric)
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


#ggsave("corTESTscores.png", plot = corTESTscores, width = 12, height = 6, dpi = 300)

cor(ds$BFI_agreeableness,ds$BFI_closeones_agreeableness) #correlations between individual total scores





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
  ) #Much nicer plot -> into the report

ggsave("corTESTscores.png", plot = corTESTscores, width = 12, height = 6, dpi = 300)



################################################################################
#Testing Validity using LM

summary(ds)
lm.model1 <- lm(BFI_agreeableness ~ BFI_extraversion + BFI_conscientiousness + BFI_neuroticism +  BFI_openness, data = ds_rev)
summary(lm.model1)
drop1(lm.model1, test ="Chisq") #F test -> more accurate than significant test from the summary ()


plot(lm.model1) #diadnostics is okay, not the greatest, but okay
#-----------------
lm.model1.1 <- lm(BFI_agreeableness ~ BFI_conscientiousness + BFI_neuroticism, data = ds_rev)
summary(lm.model1.1)
drop1(lm.model1.1, test ="Chisq") #F test -> more accurate than significant test from the summary ()

plot(lm.model1.1) #diadnostics is okay, not the greatest, but okay

anova(lm.model1, lm.model1.1) #IThe null hypothesis states that the simpler model is sufficient, meaning the parameters omitted in the submodel do not significantly improve the model.
#It is okay to simplify to lm.model1.1

#-----------------
lm.model1.2 <- lm(BFI_agreeableness ~BFI_neuroticism, data = ds_rev)
summary(lm.model1.2)
drop1(lm.model1.2, test ="Chisq") #F test -> more accurate than significant test from the summary ()

anova(lm.model1.1, lm.model1.2) #can be simplified
plot(lm.model1.2) #okay assumptions 

#However trash model -> I am still not sure, how to deal with this type of dataset
#since I assume, there is multicolinearity in the Xs -> regression does not make sense here


############################################################################
#B agreable on items ->okay this is blbost, because I just found in lm.model2.1 how was this score computed


lm.model2 <- lm(BFI_agreeableness ~ BFI_1 + BFI_2 + BFI_3 + BFI_4 + BFI_5 + BFI_6 + BFI_7 + BFI_8 + BFI_9 + BFI_10 + BFI_11 + BFI_12 + BFI_13 + BFI_14 + BFI_15 + BFI_16 + BFI_17 + BFI_18 + BFI_19 + BFI_20 + BFI_21 + BFI_22 + BFI_23 + BFI_24 + BFI_25 + BFI_26 + BFI_27 + BFI_28 + BFI_29 + BFI_30 + BFI_31 + BFI_32 + BFI_33 + BFI_34 + BFI_35 + BFI_36 + BFI_37 + BFI_38 + BFI_39 + BFI_40 + BFI_41 + BFI_42 + BFI_43 + BFI_44
                ,data = ds_rev) #It could probably be written much more neatly, but hey this works

summary(lm.model2) #too much variables

lm.model2.1 <- lm(BFI_agreeableness ~ BFI_2 + BFI_7 + BFI_12 +BFI_17 + BFI_22 +BFI_27 +BFI_32 +BFI_37 + BFI_42, data = ds_rev)
summary(lm.model2.1)

#########################################################################################
########################################################################################
##CHAPTER 3 - Internal Structure

#CORRELATION PART 2

cor(table(ds$BFI_1,ds$BFI_2))
cor.test(ds$BFI_1,ds$BFI_2) #cannot reject H0


polychoric(table(ds$BFI_1,ds$BFI_2))#I am dummy, this the generalization of tetra above
#)
(corFull <- polychoric(x = ds[,3:90])$rho) # for both questionarres

subdataset <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]

#convert columns to factors if they are not (needed for polychoric functions)
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

subdataset2 <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                         "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                         "BFI_42")] #additional subdataset for testing


# Create a named list of labels for the plot
names(corsubrho) <- labels
rownames(corsubrho) <- labels
colnames(corsubrho) <- labels
rownames(subdataset2) <- labels
colnames(subdataset2) <- labels

#Difference betweem the normal and polychoric Pearson
correlation_matrix["Z2",1:9] - corsubrho["Z2",]
correlation_matrix[1:9,1:9] - corsubrho

#Clustering---------------------------------------------------------------------------
hc <- hclust(d = as.dist(1 - corsubrho), method = "ward.D2")
ggdendrogram(data = hc)
# Define custom labels
labels <- c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")



#-----------------------------------------------------------------------------
#plot the final results together
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





##################################################################################
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

#FA for polychoric
FA2 <- fa(subdataset2, nfactors = 2, cor = "polychoric")
FA3 <- fa(subdataset2, nfactors = 3, cor = "polychoric",rotate = "none")
FA4 <- fa(subdataset2, nfactors = 4, cor = "polychoric",rotate = "none")


#removing Z32
colnames(subdataset2)
library(dplyr)
subdataset2_no_Z32 <- subdataset2[, !names(subdataset2) %in% "Z32"]

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

#converting values into numeric
subdataset2 <- subdataset2 %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

#compute the polychoric correlation matrix (I could have used one already computred)
polycor_matrix <- polychoric(subdataset2)$rho

#PCA, no rotation
pca_result <- principal(polycor_matrix, nfactors = ncol(subdataset2), rotate = "none")
pca_result <- principal(corsubrho, nfactors = ncol(subdataset2), rotate = "none")

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


#------------------------------------------------------------------------------
#Plotting
#
explained_variance <- pca_result$values / sum(pca_result$values)
variance_df <- data.frame(
  Principal_Component = seq_along(explained_variance),
  Variance_Explained = explained_variance
)
variance_df$Principal_Component <- factor(variance_df$Principal_Component)



#ggsave("screeplot.png", plot = screeplot, width = 12, height = 6, dpi = 300)

#Variance Explained by Principal Components
first_pc_loadings <- pca_result$loadings[, 1]
#------------------------------------------------------------------------------
#bar plot -> was not used
barplot(first_pc_loadings, main = "First Principal Component Loadings", 
        xlab = "Variables", ylab = "Loadings", col = "lightblue", 
        names.arg = colnames(corsubrho), las = 2, cex.names = 0.8)

#scatter plot -> was not used
plot(first_pc_loadings, type = "p", main = "First Principal Component Loadings",
     xlab = "Variables", ylab = "Loadings", col = "blue", pch = 19, 
     xaxt = "n")  # suppress x-axis labels for customization
axis(1, at = 1:length(first_pc_loadings), labels = colnames(corsubrho), las = 2, cex.axis = 0.8)
#-------------------------------------------------------------------------------

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


#PC1x PC2
loadings <- ggplot(loadings_df, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(color = "lightcoral", size = 3) +                     # Points in blue
  geom_text(hjust = 0.5, vjust = -0.5, color = "blue") +  # Labels in dark red
  labs(x = "PC1 Loadings", y = "PC2 Loadings") +
  theme_minimal()      


screeplotlod<- grid.arrange(screeplot, loadings, ncol = 2)

ggsave("screelodplot.png", plot = screeplotlod, width = 12, height = 6, dpi = 300)

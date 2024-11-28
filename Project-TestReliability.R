#BFI ANALYSIS
#ABE-KZA
#November 2024
################################################################################
#CH4 - Reliability testing
library(psychometric)




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
#SB formula
#--------------
rho_original <- 0.637# reliability of original data
items_original <- 9 # number of items in original data
items_new <- 18 # number of items in new data
(m <- items_new / items_original) # ratio of tests lengths

# new reliability
m * rho_original / (1 + (m - 1) * rho_original)

SBrel(Nlength = m, rxx = rho_original) #alternative

#-----------------------------------
rho_new <- 0.90 # desired reliability
# determining test length
(m <- rho_new * (1 - rho_original) / (rho_original * (1 - rho_new)))
ceiling(m * items_original) # new test length
#--------------

#--------------
(m <- SBlength(rxxp = rho_new, rxx = rho_original))
ceiling(m * items_original) # new test length
#--------------

#--------------
rho_new <- 0.7 # desired reliability
(m <- rho_new * (1 - rho_original) / (rho_original * (1 - rho_new)))
ceiling(m * items_original) # new test length
#--------------

#--------------
(m <- SBlength(rxxp = rho_new, rxx = rho_original))
ceiling(m * items_original) # new test length
#--------------

####################################################################################
#Split-half testing
set.seed(123)
subdataset <- ds_rev[, c("BFI_2", "BFI_7", "BFI_12", "BFI_17", 
                        "BFI_22", "BFI_27", "BFI_32", "BFI_37", 
                        "BFI_42")]
labels <- c("Z2", "Z7", "Z12", "Z17", "Z22", "Z27", "Z32", "Z37", "Z42")
rownames(subdataset) <- labels
colnames(subdataset) <- labels

subdataset7 <- subdataset[-7,] #need even samples!!! -> tossed out Z32
#------------------------------------------------------------------------
#Random
samp <- sample(1:8, 4) # choose 4 random items for df1
df1 <- subdataset7[, samp]                 # df1 with 4 random columns
df2 <- subdataset7[, setdiff(1:8, samp)]   # df2 with the remaining 4 columns

#_--------------------------------------------------------------------
#first half
df1 <- subdataset7[, 1:4]; df2 <- subdataset7[, 5:8]
#----------------------------------------------------------------------
#even-odd
df1 <- subdataset7[, seq(1, 8, 2)]; df2 <- subdataset7[, seq(2, 8, 2)]
#-------------------------------------------------------------------------

# calculating total scores for subdatasets
ts1 <- rowSums((df1))
ts2 <- rowSums(df2)
# calculating correlation between total scores of df1 and df2
cor_x <- cor(ts1, ts2)
cor_x
# Spearman-Brown formula to estimate reliability
reliability <- 2 * cor_x / (1 + cor_x)
reliability

#not the best results -> low number of items?
#best reliability has even odd split
###############################################################################
#Cronbach alpha
#popular for internal consistency measuring
# questions are highly correlated  -> high internal consistency
psych::alpha(subdataset,check.keys = TRUE)$total[1]
psych::alpha(subdataset,check.keys = TRUE)
cor.test(ds$BFI_agreeableness,ds$BFI_closeones_agreeableness)

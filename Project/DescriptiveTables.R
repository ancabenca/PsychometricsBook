descriptive_stats_western <- ds %>%
  group_by(Western) %>%
  summarise(
    Mean_BFI_agreeableness = mean(BFI_agreeableness, na.rm = TRUE),
    SD_BFI_agreeableness = sd(BFI_agreeableness, na.rm = TRUE),
    Min_BFI_agreeableness = min(BFI_agreeableness, na.rm = TRUE),
    Max_BFI_agreeableness = max(BFI_agreeableness, na.rm = TRUE),
    Skewness_BFI_agreeableness = skewness(BFI_agreeableness, na.rm = TRUE),
    Kurtosis_BFI_agreeableness = kurtosis(BFI_agreeableness, na.rm = TRUE),
    
    Mean_BFI_closeones = mean(BFI_closeones_agreeableness, na.rm = TRUE),
    SD_BFI_closeones = sd(BFI_closeones_agreeableness, na.rm = TRUE),
    Min_BFI_closeones = min(BFI_closeones_agreeableness, na.rm = TRUE),
    Max_BFI_closeones = max(BFI_closeones_agreeableness, na.rm = TRUE),
    Skewness_BFI_closeones = skewness(BFI_closeones_agreeableness, na.rm = TRUE),
    Kurtosis_BFI_closeones = kurtosis(BFI_closeones_agreeableness, na.rm = TRUE)
  )
view(descriptive_stats_western)
show(descriptive_stats_western)
write.csv(descriptive_stats_western)

descriptive_stats_year <- ds %>%
  group_by(YearNumber) %>%
  summarise(
    Mean_BFI_agreeableness = mean(BFI_agreeableness, na.rm = TRUE),
    SD_BFI_agreeableness = sd(BFI_agreeableness, na.rm = TRUE),
    Min_BFI_agreeableness = min(BFI_agreeableness, na.rm = TRUE),
    Max_BFI_agreeableness = max(BFI_agreeableness, na.rm = TRUE),
    Skewness_BFI_agreeableness = skewness(BFI_agreeableness, na.rm = TRUE),
    Kurtosis_BFI_agreeableness = kurtosis(BFI_agreeableness, na.rm = TRUE),
    
    Mean_BFI_closeones = mean(BFI_closeones_agreeableness, na.rm = TRUE),
    SD_BFI_closeones = sd(BFI_closeones_agreeableness, na.rm = TRUE),
    Min_BFI_closeones = min(BFI_closeones_agreeableness, na.rm = TRUE),
    Max_BFI_closeones = max(BFI_closeones_agreeableness, na.rm = TRUE),
    Skewness_BFI_closeones = skewness(BFI_closeones_agreeableness, na.rm = TRUE),
    Kurtosis_BFI_closeones = kurtosis(BFI_closeones_agreeableness, na.rm = TRUE)
  )

View(descriptive_stats_year)
write.csv(descriptive_stats_year)

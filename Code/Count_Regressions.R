# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reef_Residency

#### LIBRARIES ####
# install packages (only need to do this once)
# install.packages(c("tidyverse", "ggplot2", "MASS", "conflicted", "dplyr", "here",
#                    "easypackages", "pscl", "lmtest", "AER", "glmnet", "pscl"))

# load packages
library(easypackages)
libraries("tidyverse", "ggplot2", "MASS", "conflicted", "dplyr", "here", 
          "easypackages", "pscl", "lmtest", "AER", "glmnet", "pscl")
# prevent conflicts between packages
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distinct", "dplyr")

# set seed to ensure reproducibility
set.seed(7)   

#### DIRECTORIES ####
# set working directory and relative project path
setwd("E:/Data/Florida/FL_Patch_Reef_Residency/")
# set_here() # set if using here() in this location for the first time
here::i_am(".here")
here::here() 

##### Data Prep and Exploration ####
# read in the model calibration and evaluation data subsets from GitHub Repo

# gray snapper (Lutjanus griseus (lg))
lg_train = read.csv(here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Training.csv"))
lg_test = read.csv(here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Testing.csv"))

# bluestriped grunt (Haemulon sciurus (hs))
hs_train = read.csv(here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Training.csv"))
hs_test = read.csv(here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Testing.csv"))

lg_full = rbind(lg_train, lg_test)
hs_full = rbind(hs_train, hs_test)

# how many fish were present on patch reefs during the study period?
sum(round(lg_full$Abundance, digits = 0)) # 688 fish
sum(round(hs_full$Abundance, digits = 0)) # 1729 fish

# how many absence sites were there for each species?
lg_full %>%
  mutate(Rounded_Abundance = round(Abundance, digits = 0)) %>%
  filter(Rounded_Abundance == 0) %>%
  count(Species) # 172 absences

hs_full %>%
  mutate(Rounded_Abundance = round(Abundance, digits = 0)) %>%
  filter(Rounded_Abundance == 0) %>%
  count(Species) # 128 absences

# check that the training patch reef records are the same across both species
setequal(lg_train$Patch_ID, hs_train$Patch_ID) # should be TRUE

# check that the testing patch reef records are the same across both species
setequal(lg_test$Patch_ID, hs_test$Patch_ID) # should be TRUE

# plot basic histograms of species abundance in the training subsets
ggplot(lg_train, aes(x = Abundance)) + geom_histogram(binwidth = 1) + theme_bw()
ggplot(hs_train, aes(x = Abundance)) + geom_histogram(binwidth = 1) + theme_bw()

# create a new column "Count" where values represent abundance as integers 
# (whole numbers), rather than averages across buddy diver pairs. 
lg_train = lg_train %>%
  mutate(Count = round(lg_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

lg_test = lg_test %>%
  mutate(Count = round(lg_test$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

hs_train = hs_train %>%
  mutate(Count = round(hs_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

hs_test = hs_test %>%
  mutate(Count = round(hs_test$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

# remember that habitat is a categorical (factor) variable, not a continuous (numeric)
# one. numeric values are an artifact of rasterization of the original habitat vector data,
# so bring back in the habitat categories from the Unified Reef Map!
urm_classes = data.frame(
  ID = as.numeric(1:14),
  ClassLv1 = c("Individual or Aggregated Patch Reef",
               "Scattered Coral/Rock in Unconsolidated Sediment",
               "Seagrass (Continuous)",
               "Seagrass (Discontinuous)",
               "Unconsolidated Sediment",
               "Aggregate Reef",
               "Not Classified",
               "Pavement",
               "Land",
               "Reef Rubble",
               "Mangrove",
               "Artificial",
               "Dredged/Excavated",
               "Ridge"))

lg_train = lg_train %>%
  left_join(urm_classes, by = c("Habitat" = "ID")) %>%
  rename(Surveyed_Habitat = ClassLv1) %>%
  relocate(Surveyed_Habitat, .after = "Habitat")

lg_test = lg_test %>%
  left_join(urm_classes, by = c("Habitat" = "ID")) %>%
  rename(Surveyed_Habitat = ClassLv1) %>%
  relocate(Surveyed_Habitat, .after = "Habitat")

hs_train = hs_train %>%
  left_join(urm_classes, by = c("Habitat" = "ID")) %>%
  rename(Surveyed_Habitat = ClassLv1) %>%
  relocate(Surveyed_Habitat, .after = "Habitat")

hs_test = hs_test %>%
  left_join(urm_classes, by = c("Habitat" = "ID")) %>%
  rename(Surveyed_Habitat = ClassLv1) %>%
  relocate(Surveyed_Habitat, .after = "Habitat")

# keep only columns of interest for modeling, where count is the response & 13:28 is the
# range of predictors. center and scale the predictors at this time to improve later
# numerical stability and coefficient interpretability. scaling predictors helps in comparing 
# the effect sizes of coefficients, as each coefficient then represents the effect of a 
# one-standard-deviation change in the predictor.
str(lg_train) # remember to make habitat a factor!!! and BPI Broad numeric!!!

lg_train = lg_train %>%
  select(Count, Surveyed_Habitat:Pred_Density) %>%
  mutate(Surveyed_Habitat = as.factor(Surveyed_Habitat),
         BPI_Broad = as.numeric(BPI_Broad),
         Area_SG = ifelse(is.na(Area_SG), 0, Area_SG)) %>%
  mutate(across(Mangrove_Dist:Pred_Density, scale))

lg_test = lg_test %>%
  select(Count, Surveyed_Habitat:Pred_Density) %>%
  mutate(Surveyed_Habitat = as.factor(Surveyed_Habitat),
         BPI_Broad = as.numeric(BPI_Broad),
         Area_SG = ifelse(is.na(Area_SG), 0, Area_SG)) %>%
  mutate(across(Mangrove_Dist:Pred_Density, scale))

hs_train = hs_train %>%
  select(Count, Surveyed_Habitat:Pred_Density) %>%
  mutate(Surveyed_Habitat = as.factor(Surveyed_Habitat),
         BPI_Broad = as.numeric(BPI_Broad),
         Area_SG = ifelse(is.na(Area_SG), 0, Area_SG)) %>%
  mutate(across(Mangrove_Dist:Pred_Density, scale))

hs_test = hs_test %>%
  select(Count, Surveyed_Habitat:Pred_Density) %>%
  mutate(Surveyed_Habitat = as.factor(Surveyed_Habitat),
         BPI_Broad = as.numeric(BPI_Broad),
         Area_SG = ifelse(is.na(Area_SG), 0, Area_SG)) %>%
  mutate(across(Mangrove_Dist:Pred_Density, scale))

# check the structure of the data and ensure that there are no NAs
str(lg_train)
str(hs_train)

#### Fitting Poisson and ZINB Models ####
# Overview of Poisson Regression assumptions
# 1. Poisson Response - the response variable is a count per unit of time or space,
# described by a Poisson distribution. 
# 2. Independence - the observations must be independent of one another. 
# 3. Mean = Variance - by definition, the mean of a Poisson random variable must
# be equal to its variance. --> equidispersion
# 4. Linearity - the log of the mean rate, log(lambda), must be a linear function of x.

# first compare the observed mean and variance for each species
mean(lg_train$Count)
var(lg_train$Count)

mean(hs_train$Count)
var(hs_train$Count)
# variance far exceeds the mean for both species, especially bluestriped grunts

##### Gray Snapper #####
# having considered these assumptions, let's proceed with caution...
# fit a simple Poisson Regression to the gray snapper training data
lg_pois = glm(Count ~ ., data = lg_train, family = poisson(link = "log"))

# check out the results
summary(lg_pois)

# calculate the p-value for the deviance goodness of fit test --> calculate the 
# probability to the right of the deviance value for the chi-squared distribution
# on 150 degrees of freedom:
pchisq(lg_pois$deviance, df = lg_pois$df.residual, lower.tail = FALSE)

# the null hypothesis is that our model is correctly specified. according to the 
# deviance goodness of fit test, we have strong evidence to reject the null 
# hypothesis (suggesting a poor model fit). 

# overdispersion is likely a problem, let's use the following code to check the 
# null hypothesis of equidispersion:
dispersiontest(lg_pois, trafo = 1, alternative = "greater") 
# evidence of slight overdispersion at a significance level of 0.05

# instead fit a zero-inflated negative binomial model to the data
lg_zinb = zeroinfl(Count ~ . | 1, data = lg_train, dist = "negbin")
summary(lg_zinb)

# residual plot for Poisson regression
lg_pois_res = resid(lg_pois)
plot(fitted(lg_pois), lg_pois_res, col = 'steelblue', pch = 16, 
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Poisson')
abline(0,0)

# residual plot for zero-inflated negative binomial regression 
lg_nb_res = resid(lg_zinb)
plot(fitted(lg_zinb), lg_nb_res, col = 'steelblue', pch = 16,
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Zero-Inflated Negative Binomial')
abline(0,0)

# likelihood ratio test to compare models (null H0 is that the restricted Poisson
# model provides a better fit than the more complex negative binomial model)
# by hand...
pchisq(2 * (logLik(lg_zinb) - logLik(lg_pois)), df = 1, lower.tail = FALSE)

# using lmtest package...
lrtest(lg_pois, lg_zinb)

# reject H0, there is a statistically significant difference between the zero-inflated 
# negative binomial and poisson regressions, NB provides a better fit to the training data.

##### Bluestriped Grunt #####
# fit a simple Poisson Regression to the bluestriped grunt training data
hs_pois = glm(Count ~ ., data = hs_train, family = poisson(link = "log"))

# check out the results
summary(hs_pois)

# calculate the p-value for the deviance goodness of fit test --> calculate the 
# probability to the right of the deviance value for the chi-squared distribution
# on 150 degrees of freedom:
pchisq(hs_pois$deviance, df = hs_pois$df.residual, lower.tail = FALSE)

# the null hypothesis is that our model is correctly specified. according to the 
# deviance goodness of fit test, we have strong evidence to reject the null 
# hypothesis (suggesting a poor model fit). 

# overdispersion is likely a problem, let's use the following code to check the 
# null hypothesis of equidispersion:
dispersiontest(hs_pois, trafo = 1, alternative = "greater") 
# strong evidence of overdispersion at a significance level of 0.05

# instead fit a zero-inflated negative binomial model to the data
hs_zinb = zeroinfl(Count ~ . | 1, data = hs_train, dist = "negbin")
summary(hs_zinb)

# residual plot for Poisson regression
hs_pois_res = resid(hs_pois)
plot(fitted(hs_pois), hs_pois_res, col = 'steelblue', pch = 16, 
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Poisson')
abline(0,0)

# residual plot for zero-inflated negative binomial regression 
hs_nb_res = resid(hs_zinb)
plot(fitted(hs_zinb), hs_nb_res, col = 'steelblue', pch = 16,
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Zero-Inflated Negative Binomial')
abline(0,0)

# likelihood ratio test to compare models (null H0 is that the restricted Poisson
# model provides a better fit than the more complex negative binomial model)
# by hand...
pchisq(2 * (logLik(hs_zinb) - logLik(hs_pois)), df = 1, lower.tail = FALSE)

# using lmtest package...
lrtest(hs_pois, hs_zinb)


#### Interpreting ZINB Models ####
##### Gray Snapper #####
# get the summary of the model
summary_lg_zinb = summary(lg_zinb)
print(round(AIC(lg_zinb), digits = 2))

# extract coefficients for count and zero components
lg_count_coefficients = summary_lg_zinb$coefficients$count
lg_zero_coefficients = summary_lg_zinb$coefficients$zero

# convert to data frames
lg_count_coefficients_df = as.data.frame(lg_count_coefficients)
lg_zero_coefficients_df = as.data.frame(lg_zero_coefficients)

# extract estimates, standard errors, and p-values for count and zero components
lg_original_estimates_count = lg_count_coefficients_df[, "Estimate"]
lg_original_estimates_zero = lg_zero_coefficients_df[, "Estimate"]

lg_std_errors_count = lg_count_coefficients_df[, "Std. Error"]
lg_std_errors_zero = lg_zero_coefficients_df[, "Std. Error"]

lg_p_values_count = lg_count_coefficients_df[, "Pr(>|z|)"]
lg_p_values_zero = lg_zero_coefficients_df[, "Pr(>|z|)"]

# calculate IRR (exponentiate coefficients)
lg_irr_count = exp(lg_original_estimates_count)
lg_irr_zero = exp(lg_original_estimates_zero)

# calculate 95% confidence intervals for count and zero components
lg_ci_lower_count = exp(lg_original_estimates_count - 1.96 * lg_std_errors_count)
lg_ci_upper_count = exp(lg_original_estimates_count + 1.96 * lg_std_errors_count)

lg_ci_lower_zero = exp(lg_original_estimates_zero - 1.96 * lg_std_errors_zero)
lg_ci_upper_zero = exp(lg_original_estimates_zero + 1.96 * lg_std_errors_zero)

# create a summary table for the count component
lg_count_results_table= data.frame(
  Variable = rownames(lg_count_coefficients_df),
  Estimate = lg_original_estimates_count,
  IRR = lg_irr_count,
  CI_Lower = lg_ci_lower_count,
  CI_Upper = lg_ci_upper_count,
  p_Value = lg_p_values_count)

# create a summary table for the zero component
lg_zero_results_table = data.frame(
  Variable = rownames(lg_zero_coefficients_df),
  Estimate = lg_original_estimates_zero,
  IRR = lg_irr_zero,
  CI_Lower = lg_ci_lower_zero,
  CI_Upper = lg_ci_upper_zero,
  p_Value = lg_p_values_zero)

# print the results tables
print("Count Component Results:")
print(lg_count_results_table)

print("Zero Component Results:")
print(lg_zero_results_table)

# remove intermediate products
rm(list = c("lg_count_coefficients", "lg_zero_coefficients", "lg_count_coefficients_df",
            "lg_zero_coefficients_df", "lg_original_estimates_count",
            "lg_original_estimates_zero", "lg_std_errors_count", "lg_std_errors_zero",
            "lg_p_values_count", "lg_p_values_zero", "lg_irr_count", "lg_irr_zero",
            "lg_ci_lower_count", "lg_ci_lower_zero", "lg_ci_upper_count", "lg_ci_upper_zero"))

# now try predicting the validation data subset using the fitted negative binomial models
lg_test$ZINB_Predicted = predict(lg_zinb, lg_test, type = "response")

# plot the observed counts vs. the predicted counts
ggplot(lg_test, aes(ZINB_Predicted, Count)) +
  geom_point() +
  ggtitle("Zero-inflated negative binomial regression") +
  ylab("Observed count") +
  xlab("Predicted count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# how often did the ZINB model accurately predict the known counts?
# make a new dataframe, where a value of 1 means a correct prediction and a value 
# of 0 means an incorrect prediction 
lg_predictions = as.data.frame(ifelse(
  lg_test$Count == round(lg_test$ZINB_Predicted, digits = 0), 1, 0)) %>%
  mutate(Correct_Prediction = ifelse(lg_test$Count == round(lg_test$ZINB_Predicted, digits = 0), 1, 0)) %>%
  mutate(Observed_Count = lg_test$Count) %>%
  mutate(Predicted_Count = round(lg_test$ZINB_Predicted, digits = 0)) %>%
  select(Observed_Count, Predicted_Count, Correct_Prediction)

# calculate accuracy by summing the correct predictions, dividing by the total
# number of predictions, and multiplying by 100
lg_zinb_accuracy = (sum(lg_predictions$Correct_Prediction)/as.numeric(nrow(lg_predictions))) * 100
print(round(lg_zinb_accuracy, digits = 2))

# how about a predicted fish count within (+-) 5 from the observed count
lg_predictions$Within_Five = 
  ifelse((abs(lg_test$Count - round(lg_test$ZINB_Predicted, digits = 0)) <= 5), 1, 0)

# calculate accuracy by summing the correct predictions (+- 5 fishes), dividing 
# by the total number of predictions, and multiplying by 100
lg_within_five_accuracy = (sum(lg_predictions$Within_Five)/as.numeric(nrow(lg_predictions))) * 100
print(round(lg_within_five_accuracy, digits = 2))

# save predictions for mapping 
lg_map_pred = lg_test %>%
  mutate(Observed_Count = lg_test$Count) %>%
  mutate(Predicted_Count = round(lg_test$ZINB_Predicted, digits = 0)) %>%
  mutate(Correct_Prediction = ifelse(Observed_Count == Predicted_Count, 1, 0)) %>%
  mutate(Within_Five = ifelse((abs(Observed_Count - Predicted_Count) <= 5), 1, 0))

# how well did the model predict known absence (count = 0) sites?
# add a new column to identify correctly predicted zeros
lg_map_pred = lg_map_pred %>%
  mutate(Correct_Zero_Prediction = ifelse(Observed_Count == 0 & Predicted_Count == 0, 1, 0))

# calculate the percentage of correctly predicted zeros
lg_total_actual_zeros = sum(lg_map_pred$Observed_Count == 0)  # Total number of actual zeros
lg_correct_zero_predictions = sum(lg_map_pred$Correct_Zero_Prediction)  # Correctly predicted zeros

# percentage of correctly predicted zeros
lg_accuracy_zero_predictions = (lg_correct_zero_predictions / lg_total_actual_zeros) * 100
print(round(lg_accuracy_zero_predictions, digits = 2))

# make a QQ plot using the fitted model
# extract Pearson residuals from the model
lg_residuals_pearson = residuals(lg_zinb, type = "pearson")

# create the QQ plot
qqnorm(lg_residuals_pearson, main = expression(italic("Lutjanus griseus")))
qqline(lg_residuals_pearson, col = "black")

# check for homogeneity of variance
lg_fitted_values = fitted(lg_zinb)

plot(lg_fitted_values, lg_residuals_pearson, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals")
abline(h = 0, col = "black", lty = 2)

##### Bluestriped Grunt #####
# get the summary of the model
summary_hs_zinb = summary(hs_zinb)
print(round(AIC(hs_zinb), digits = 2))

# extract coefficients for count and zero components
hs_count_coefficients = summary_hs_zinb$coefficients$count
hs_zero_coefficients = summary_hs_zinb$coefficients$zero

# convert to data frames
hs_count_coefficients_df = as.data.frame(hs_count_coefficients)
hs_zero_coefficients_df = as.data.frame(hs_zero_coefficients)

# extract estimates, standard errors, and p-values for count and zero components
hs_original_estimates_count = hs_count_coefficients_df[, "Estimate"]
hs_original_estimates_zero = hs_zero_coefficients_df[, "Estimate"]

hs_std_errors_count = hs_count_coefficients_df[, "Std. Error"]
hs_std_errors_zero = hs_zero_coefficients_df[, "Std. Error"]

hs_p_values_count = hs_count_coefficients_df[, "Pr(>|z|)"]
hs_p_values_zero = hs_zero_coefficients_df[, "Pr(>|z|)"]

# calculate IRR (exponentiate coefficients)
hs_irr_count = exp(hs_original_estimates_count)
hs_irr_zero = exp(hs_original_estimates_zero)

# calculate 95% confidence intervals for count and zero components
hs_ci_lower_count = exp(hs_original_estimates_count - 1.96 * hs_std_errors_count)
hs_ci_upper_count = exp(hs_original_estimates_count + 1.96 * hs_std_errors_count)

hs_ci_lower_zero = exp(hs_original_estimates_zero - 1.96 * hs_std_errors_zero)
hs_ci_upper_zero = exp(hs_original_estimates_zero + 1.96 * hs_std_errors_zero)

# create a summary table for the count component
hs_count_results_table= data.frame(
  Variable = rownames(hs_count_coefficients_df),
  Estimate = hs_original_estimates_count,
  IRR = hs_irr_count,
  CI_Lower = hs_ci_lower_count,
  CI_Upper = hs_ci_upper_count,
  p_Value = hs_p_values_count)

# create a summary table for the zero component
hs_zero_results_table = data.frame(
  Variable = rownames(hs_zero_coefficients_df),
  Estimate = hs_original_estimates_zero,
  IRR = hs_irr_zero,
  CI_Lower = hs_ci_lower_zero,
  CI_Upper = hs_ci_upper_zero,
  p_Value = hs_p_values_zero)

# print the results tables
print("Count Component Results:")
print(hs_count_results_table)

print("Zero Component Results:")
print(hs_zero_results_table)

rm(list = c("hs_count_coefficients", "hs_zero_coefficients", "hs_count_coefficients_df",
            "hs_zero_coefficients_df", "hs_original_estimates_count",
            "hs_original_estimates_zero", "hs_std_errors_count", "hs_std_errors_zero",
            "hs_p_values_count", "hs_p_values_zero", "hs_irr_count", "hs_irr_zero",
            "hs_ci_lower_count", "hs_ci_lower_zero", "hs_ci_upper_count", "hs_ci_upper_zero"))

# now try predicting the validation data subset using the fitted negative binomial models
hs_test$ZINB_Predicted = predict(hs_zinb, hs_test, type = "response")

# plot the observed counts vs. the predicted counts
ggplot(hs_test, aes(ZINB_Predicted, Count)) +
  geom_point() +
  ggtitle("Zero-inflated negative binomial regression") +
  ylab("Observed count") +
  xlab("Predicted count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# how often did the ZINB model accurately predict the known counts?
# make a new dataframe, where a value of 1 means a correct prediction and a value 
# of 0 means an incorrect prediction 
hs_predictions = as.data.frame(ifelse(
  hs_test$Count == round(hs_test$ZINB_Predicted, digits = 0), 1, 0)) %>%
  mutate(Correct_Prediction = ifelse(hs_test$Count == round(hs_test$ZINB_Predicted, digits = 0), 1, 0)) %>%
  mutate(Observed_Count = hs_test$Count) %>%
  mutate(Predicted_Count = round(hs_test$ZINB_Predicted, digits = 0)) %>%
  select(Observed_Count, Predicted_Count, Correct_Prediction)

# calculate accuracy by summing the correct predictions, dividing by the total
# number of predictions, and multiplying by 100
hs_zinb_accuracy = (sum(hs_predictions$Correct_Prediction)/as.numeric(nrow(hs_predictions))) * 100
print(round(hs_zinb_accuracy, digits = 2))

# how about a predicted fish count within (+-) 5 from the observed count
hs_predictions$Within_Five = 
  ifelse((abs(hs_test$Count - round(hs_test$ZINB_Predicted, digits = 0)) <= 5), 1, 0)

# calculate accuracy by summing the correct predictions (+- 5 fishes), dividing 
# by the total number of predictions, and multiplying by 100
hs_within_five_accuracy = (sum(hs_predictions$Within_Five)/as.numeric(nrow(hs_predictions))) * 100
print(round(hs_within_five_accuracy, digits = 2))

# save predictions for mapping 
hs_map_pred = hs_test %>%
  mutate(Observed_Count = hs_test$Count) %>%
  mutate(Predicted_Count = round(hs_test$ZINB_Predicted, digits = 0)) %>%
  mutate(Correct_Prediction = ifelse(Observed_Count == Predicted_Count, 1, 0)) %>%
  mutate(Within_Five = ifelse((abs(Observed_Count - Predicted_Count) <= 5), 1, 0))

# how well did the model predict known absence (count = 0) sites?
# add a new column to identify correctly predicted zeros
hs_map_pred = hs_map_pred %>%
  mutate(Correct_Zero_Prediction = ifelse(Observed_Count == 0 & Predicted_Count == 0, 1, 0))

# calculate the percentage of correctly predicted zeros
hs_total_actual_zeros = sum(hs_map_pred$Observed_Count == 0)  # Total number of actual zeros
hs_correct_zero_predictions = sum(hs_map_pred$Correct_Zero_Prediction)  # Correctly predicted zeros

# percentage of correctly predicted zeros
hs_accuracy_zero_predictions = (hs_correct_zero_predictions / hs_total_actual_zeros) * 100
print(round(hs_accuracy_zero_predictions, digits = 2))


#### Saving the results ####
# save the model results
write.csv(lg_count_results_table,
          row.names = FALSE,
          here("Data", "Model_Data", "Gray_Snapper_Count_Component_Results.csv"))
write.csv(lg_zero_results_table,
          row.names = FALSE,
          here("Data", "Model_Data", "Gray_Snapper_Zero_Component_Results.csv"))

write.csv(hs_count_results_table,
          row.names = FALSE,
          here("Data", "Model_Data", "Bluestriped_Grunt_Count_Component_Results.csv"))
write.csv(hs_zero_results_table,
          row.names = FALSE,
          here("Data", "Model_Data", "Bluestriped_Grunt_Zero_Component_Results.csv"))

# add back in the geographic coordinates of the testing sites
lg_test_coords = read.csv(here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Testing.csv"))
lg_test_coords = lg_test_coords %>% select(Long_M, Lat_M)
lg_map_pred = cbind(lg_map_pred, lg_test_coords)

hs_test_coords = read.csv(here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Testing.csv"))
hs_test_coords = hs_test_coords %>% select(Long_M, Lat_M)
hs_map_pred = cbind(hs_map_pred, hs_test_coords)

# save the map predictions
write.csv(lg_map_pred,
          row.names = FALSE,
          here("Data", "Model_Data", "Subadult_Gray_Snapper_Test_Site_Predictions.csv")) 
write.csv(hs_map_pred,
          row.names = FALSE,
          here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Test_Site_Predictions.csv")) 

#### Diagnostic Plots ####
##### Gray Snapper #####
lg_zinb_residuals = resid(lg_zinb, type = "pearson")  
# create a data frame for QQ plot
lg_qq_data = data.frame(
  sample_quantiles = sort(lg_zinb_residuals),
  theoretical_quantiles = qnorm(ppoints(length(lg_zinb_residuals))))
# make the plot
lg_qq_plot = 
  ggplot(lg_qq_data, aes(x = theoretical_quantiles, y = sample_quantiles)) +
  geom_point(color = "black") +  
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
lg_qq_plot

# create a data frame for homogeneity of variance plot
lg_homogeneity_data = data.frame(
  fitted = fitted(lg_zinb),
  residuals = lg_zinb_residuals)
# make the plot
lg_homogeneity_plot =
  ggplot(lg_homogeneity_data, aes(x = fitted, y = residuals)) +
  xlim(0, 200) +
  geom_point(color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fitted Values", 
       y = "Residuals") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
lg_homogeneity_plot

##### Bluestriped Grunt #####
hs_zinb_residuals = resid(hs_zinb, type = "pearson")  
# create a data frame for QQ plot
hs_qq_data = data.frame(
  sample_quantiles = sort(hs_zinb_residuals),
  theoretical_quantiles = qnorm(ppoints(length(hs_zinb_residuals))))
# make the plot
hs_qq_plot = 
  ggplot(hs_qq_data, aes(x = theoretical_quantiles, y = sample_quantiles)) +
  geom_point(color = "black") +  
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
hs_qq_plot

# create a data frame for homogeneity of variance plot
hs_homogeneity_data = data.frame(
  fitted = fitted(hs_zinb),
  residuals = hs_zinb_residuals)
# make the plot
hs_homogeneity_plot =
  ggplot(hs_homogeneity_data, aes(x = fitted, y = residuals)) +
  xlim(0, 200) +
  geom_point(color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fitted Values", 
       y = "Residuals") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
hs_homogeneity_plot

require(patchwork)
diagnostics_plots <- 
  (lg_qq_plot + hs_qq_plot +
     lg_homogeneity_plot + hs_homogeneity_plot) +
  plot_layout(ncol = 2) +  
  plot_annotation(tag_levels = 'A') +
  plot_annotation(
    # title = "Lutjanus griseus                                      Haemulon sciurus", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "italic")))

diagnostics_plots

ggsave(
  filename = here("Figures", "Supplementary_Figure_1.png"),
  plot = diagnostics_plots,
  width = 6, height = 6,  
  dpi = 450)

# save working environment
save.image(here("Data", "Model_Data", "Count_Regressions.RData"))

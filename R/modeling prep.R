library(caret)
library(stringr)
library(purrr)

setwd('C:/users/zack/dropbox/classes/stat616/project')
source('R/functions.R')
psid_clean <- read.csv('data/cleaned_data.csv')

# Seperate our feature columns into numeric and categorical for ease of use
not_predictors <- c(
  'X', 'release_nb', 
  'fam_int_id', 'donated',
  'donated_gt_25_charity_last_yr',
  'immigrant_fam_wt', 
  'respondant_was',
  'house_val', 
  paste0('race_ref_', 2:4),
  'religious_denom',
  'age' # replace with log_age
)

psid_clean$log_age <- log(psid_clean$age)

psid_clean <- relevel_factors(psid_clean)

# Split the data into training and testing data
data_split <- stratifiedSample(
  df = psid_clean,
  target = psid_clean$donated,
  sample_rates = c(.8, .2),
  seed = 1234
)

train <- data_split[[1]]
validation <- data_split[[2]]

predictors <- colnames(psid_clean)[!colnames(psid_clean) %in% not_predictors]
numeric_vars <- predictors[sapply(psid_clean[, predictors], is.numeric)]
categorical_vars <- predictors[sapply(psid_clean[, predictors], is.factor)]

dummy <- dummyVars(
  reformulate(categorical_vars, 'donated'),
  data = psid_clean,
  fullRank = TRUE # k-1 levels
)

x_train <- design_matrix(
  train,
  numeric_vars,
  dummy
)
y_train <- ifelse(train$donated == "Yes", 1, 0)

dummy_groups <- group_dummies(colnames(x_train))

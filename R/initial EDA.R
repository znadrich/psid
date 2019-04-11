library(ggplot2)
library(dplyr)
library(manipulate)
library(corrplot)
library(grid)

source('R/functions.R')
psid_clean <- read.csv('data/cleaned_data.csv')

# Seperate our feature columns into numeric and categorical for ease of use
not_predictors <- c(
  'X', 'release_nb', 
  'fam_int_id', 'donated',
  'donated_gt_25_charity_last_yr',
  'immigrant_fam_wt', 
  'respondant_was'
)
predictors <- colnames(psid_clean)[!colnames(psid_clean) %in% not_predictors]
numeric_vars <- predictors[sapply(psid_clean[, predictors], is.numeric)]
categorical_vars <- predictors[sapply(psid_clean[, predictors], is.factor)]

# Split the data into training and testing data
data_split <- stratifiedSample(
  df = psid_clean,
  target = psid_clean$donated,
  sample_rates = c(.8, .2),
  seed = 1234
)

train <- data_split[[1]]
validation <- data_split[[2]]

# Validate that the target distribution is similar
mean(train$donated == 'Yes')
mean(validation$donated == 'Yes')

# Check out distribution of numeric variables
manipulate(
  distributionPlot(var, type, min, max, use_scales, df = train),
  var = picker(as.list(numeric_vars)),
  type = picker(list('density', 'histogram')),
  min = slider(min = 0, max = 100),
  max = slider(min = 0, max = 100),
  use_scales = picker(as.list(c(FALSE, TRUE)))
)

# Correlation of predictors - note we need to remove NA first
corrplot(
  cor(train[complete.cases(train[, numeric_vars]), numeric_vars]),
  method = "color"
)

manipulate(
  logOddsPlot(df = train[, c(numeric_vars, 'donated')], char, cuts, min, filter_n),
  char = picker(as.list(numeric_vars)),
  cuts = slider(min = 2, max = 100),
  min = slider(min = 0, max = 100),
  filter_n = slider(min = 0, max = 1000)
)

manipulate(
  categoricalPlot(df = train, char),
  char = picker(as.list(categorical_vars))
)

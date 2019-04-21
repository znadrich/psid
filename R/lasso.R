library(oem)
library(doParallel)
library(dplyr)
library(ggplot2)

registerDoParallel(cores = 4)
system.time(
lasso.cv <- cv.oem(
  x = x_train,
  y = y_train,
  penalty = 'grp.lasso',
  family = 'binomial',
  groups = dummy_groups,
  intercept = TRUE,
  standardize = F,
  nfolds = 10,
  ncores = 4,
  parallel = TRUE
))
stopImplicitCluster()

plot(lasso.cv)
best_lambda_index <- which(lasso.cv$lambda[[1]] == lasso.cv$lambda.1se)
best_lambda <- lasso.cv$lambda.1se
lasso <- lasso.cv$oem.fit

nzero <- max(lasso$nzero[[1]]) - lasso$nzero[[1]][best_lambda_index]
coef <- lasso$beta$grp.lasso[, best_lambda_index]
coef[coef == 0]
coefplot.oem(coef)

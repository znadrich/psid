# Stratified sampling based on target
# Provide sample rates in form of probability vector and seed if you'd like
# Returns a list of dataframes 
stratifiedSample <- function(df, target, sample_rates, seed = round(rnorm(1, 10000, 1000))){
  n_samples <- length(sample_rates)
  
  # Stratify by class
  classes <- unique(target)
  class_dfs <- lapply(classes, function(x) df[target == x, ])
  
  set.seed(seed)
  sample_indicies <- lapply(class_dfs, 
                            function(x) {
                              sample(x = 1:n_samples,
                                     size = nrow(x),
                                     replace = TRUE,
                                     prob = sample_rates)
                            })
  
  # For each sample needed, we will select the indicicies for each class that belong
  # to that specific sample and make a dataframe. There is almost certainly a more elegant
  # way to do this...
  samples <- list()
  for(i in 1:n_samples){
    samples[[i]] <- lapply(1:length(classes), 
                           function(class, sample){
                             df <- class_dfs[[class]]
                             indicies <- sample_indicies[[class]]
                             return(df[indicies == sample, ])
                           },
                           sample = i) %>%
      do.call(rbind, .)
  }
  
  return(samples)
}

# View a density or histogram plot of var
distributionPlot <- function(var, type = 'density', min, max, use_scales, df = data){
  if(type == 'density'){
    plt <- ggplot(data = df, aes_string(x = var)) +
      geom_density()
  } else if (type == 'histogram') {
    plt <- ggplot(data = df, aes_string(x = var)) +
      geom_histogram() 
  }
  
  if(use_scales){
    plt <- plt + scale_x_continuous(limits = c(min, max))
  }
  plt
  
}


# Check out how the log odds varies with continuous variables
logOddsPlot <- function(df, char, cuts, min, filter_n){
  df %>%
    filter(!!sym(char) > min) %>%
    mutate(grp = cut(!!sym(char), cuts),
           donated_num = ifelse(donated == 'Yes', 1, 0)) %>%
    group_by(grp) %>%
    summarize(var = median(!!sym(char)),
              log_odds_donated = log(mean(donated_num)/(1-mean(donated_num))),
              n = n()) %>%
    filter(n > filter_n) %>%
    ggplot(aes(x = var, y = log_odds_donated)) + 
    geom_smooth(se = F, method = 'loess') + 
    geom_point() + 
    xlab(char)
}

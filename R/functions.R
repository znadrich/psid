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

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

categoricalPlot <- function(df, char){
  df$var <- df[, char]
  
  plot_df <- df %>%
    group_by(var) %>%
    mutate(donated_num = ifelse(donated == 'Yes', 1, 0)) %>%
    summarize(n = n(),
              donated = mean(donated_num))
  
  plt1 <- plot_df %>%
    ggplot(aes(x = var, y = n)) +
    geom_bar(stat = 'identity') + 
    coord_flip() +
    xlab(char)
  
  plt2 <- plot_df %>%
    ggplot(aes(x = var, y = donated)) +
    geom_bar(stat = 'identity') + 
    coord_flip() +
    xlab(char) 
  
  multiplot(plt2, plt1, cols = 1)
}

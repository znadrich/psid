##### Utility functions

# Return the metadata for a given column
get <- function(col, dict){
  return(dict[col][[1]])
}

get_old_name <- function(dict, name){
  bool <- sapply(dict, function(x) if(x == name) TRUE else FALSE)
  old_name <- names(which(bool))
  return(old_name)
}

# Retrieve the decoded column name
get_col_names <- function(df, dict){
  names <- colnames(df)
  new_names <- sapply(names, function(x) get(x, dict)$name)
  return(as.character(new_names))
}

# Determine if the column is categorical from metadata
is_categorical <- function(cols, dict){
  is_cat <- sapply(cols, function(x) get(x, dict)$type == "Categorical")
  return(as.logical(is_cat))
}

# Determine if the column is numeric from metadata
is_numeric <- function(cols, dict){
  is_num <- sapply(cols, function(x) get(x, dict)$type == "Numeric")
  return(as.logical(is_num))
}

# Translate the default value to the decoded value for a categorical variable
get_cat_val <- function(x, value_schema){
  x <- as.character(x)
  return(as.character(value_schema[x]))
}

# Translate the default value to the decoded value for all categorical variables
decode_vals <- function(colname, df, dict){
  vals <- df[, colname]
  col_dict <- get(colname, dict)
  new_vals <- sapply(vals, function(x) get_cat_val(x, col_dict$values))
  return(new_vals)
}

# Remove commmas from numeric variables, e.g. 9,000 -> 9000
fix_numeric_comma <- function(x){
  x <- as.character(x)
  new_x <- str_replace(
    string = x,
    pattern = ",",
    replacement = ""
  )
  return(as.numeric(new_x))
}

# Return null values for numeric values as determined by metadata
null_numeric <- function(x, null_schema){
  if(length(null_schema) == 0){
    return(x)
  } else {
    index_x <- as.character(x)
    if(index_x %in% names(null_schema)){
      return(NA)
    } else {
      return(x)
    }
  }
}

# Remove commmas from numeric variables, e.g. 9,000 -> 9000
# Return null values for numeric values as determined by metadata
clean_numeric <- function(colname, df, dict){
  vals <- df[, colname]
  col_dict <- get(colname, dict)
  cleaned_vals <- fix_numeric_comma(vals)
  new_vals <- sapply(cleaned_vals, function(x) null_numeric(x, col_dict$values$Null))
  return(new_vals)
}

apply_filters <- function(colname, df, dict){
  vals <- df[, colname]
  
  old_nm <- get_old_name(dict, colname)
  col_dict <- get(old_nm, dict)
  remove_vals <- col_dict$remove_if
  
  for(filter in remove_vals){
    df <- df[!(vals %in% remove_vals), ]
    removed_n <- length(vals) - nrow(df)
    vals <- df[, colname]
    
    str <- sprintf(
      "%s = %s - Removed %s",
      colname,
      filter,
      removed_n
    )
    
    print(str)
  }

  return(df)
}

discretize_house_val <- function(df, n_buckets){
  buckets <- df %>%
    filter(!is.na(house_val)) %>%
    mutate(grp = ntile(house_val, n_buckets)) %>% 
    group_by(grp) %>% 
    summarize(
      min = min(house_val), 
      max = max(house_val)
    ) %>%
    rbind(c(0, -1, 0))
  
  df$house_val[is.na(df$house_val)] <- -1
  
  lookup <- sapply(
    1:n_buckets, 
    function(x) (df$house_val >= buckets$min[x] & df$house_val < buckets$max[x]) 
  )
  
  which_grp <- sapply(
    1:ncol(lookup), 
    function(x) ifelse(m[, x], x, -1)
  )
  
  which_grp[df$house_val == max(df$house_val)] <- n_buckets
  
  grp <- apply(
    which_grp, 
    FUN = max, 
    MARGIN = 1
  )
  
  grp <- ifelse(grp == -1, nrow(buckets), grp)
  
  decode_grp <- paste(buckets$min[grp]/1000, buckets$max[grp]/1000, sep = ", ") %>%
    paste0("[", ., ")")
  decode_grp <- ifelse(decode_grp == "[-0.001, 0)", "Does not own home", decode_grp)
  return(decode_grp)
}

drop_remaining_na <- function(df){
  # Seperate our feature columns into numeric and categorical for ease of use
  not_predictors <- c(
    'X', 'release_nb', 
    'fam_int_id', 'donated',
    'donated_gt_25_charity_last_yr',
    'immigrant_fam_wt', 
    'respondant_was',
    'house_val'
  )
  predictors <- colnames(df)[!colnames(df) %in% not_predictors]
  keep <- complete.cases(df[, predictors])
  
  print(sprintf("Dropping %s rows.", sum(!keep)))
  
  df <- df[complete.cases(df[, predictors]), ]
}

# Cleans whole dataframe
# 1. Translate the default value to the decoded value for all categorical variables
# 2. Remove commmas from numeric variables, e.g. 9,000 -> 9000
# 3. Return null values for numeric values as determined by metadata
# 4. Decodes column names
clean_df <- function(df, dict){
  cols <- colnames(df)
  categoricals <- cols[is_categorical(cols, dict)]
  numerics <- cols[is_numeric(cols, dict)]
  
  df[, categoricals] <- sapply(
    categoricals, 
    decode_vals,
    df,
    dict
  )
  
  df[, numerics] <- sapply(
    numerics, 
    clean_numeric,
    df,
    dict
  )
  
  colnames(df) <- get_col_names(df, dict)
  
  for(categorical in categoricals){
    lookup <- get(categorical, dict)
    name <- lookup$name
    df <- apply_filters(
      colname = name,
      df = df,
      dict = dict
    )
  }
  
  df$house_val_thousand <- discretize_house_val(df, 8)
  df <- drop_remaining_na(df)
  return(df)
}

#########
library(jsonlite)
library(xlsx)
library(stringr)

setwd('C:/users/zack/dropbox/classes/stat616/project')
codes_json <- read_json("data/codes.json")
psid <- read.xlsx2("data/glm data 3.29.xlsx", sheetIndex = 1)
psid_clean <- clean_df(psid, codes_json)
psid_clean$donated <- ifelse(psid_clean$donated_gt_25_charity_last_yr == 'Yes', 'Yes', 'No')
write.csv(psid_clean, "data/cleaned_data.csv")

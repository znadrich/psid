##### Utility functions

# Return the metadata for a given column
get <- function(col, dict){
  return(dict[col][[1]])
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

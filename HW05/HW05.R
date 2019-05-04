library(dplyr)


foo <- function(df, row_selection, column_selection){
  process_column <- function(input_vec){
    if (is.numeric(input_vec)){
      return(sum(input_vec))
    } else {
      return(summary(factor(input_vec)))
    }
  }
  if (!is.character(column_selection)){
    columns <- colnames(df)[column_selection]
  } else {
    columns <- column_selection
  }

  df <- df %>%
    select(columns)
  df <- df[row_selection, ]
  
  return(list(df, lapply(df, process_column)))
}

df <- mtcars

out <- foo(df, c(T,F), c("mpg", "disp"))
out[1]
out[2]



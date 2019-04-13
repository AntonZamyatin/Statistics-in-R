foo <- function(df, row_selection, column_selection){
  process_column <- function(input_vec){
    if (is.numeric(input_vec)){
      return(sum(input_vec))
    } else {
      return(summary(factor(input_vec)))
    }
  }
  df <- df[row_selection, column_selection]
  return(list(df, lapply(df, process_column)))
}

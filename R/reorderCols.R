#' A column reordering function
#'
#' This function reorders a dataframe according to the order of identical elements of another dataframe
#' @param x The dataframe that needs to be reordered
#' @param matchData The dataframe that contains the column to match
#' @param xCol The column of x that needs to be reordered
#' @param newCol The index of the column in matchData ywith respect to which you want to reorder x
#' @export
#' @examples
#' reorderCols(x = my_df, match.data = new_df, x.col = 3, new.col = 4)

reorderCols <- function(x, match.data, x.col, new.col) {
  t <- x
  t <- t[order(match(x[, xCol], matchData[, newCol])), ]
  return(t)
}

#' A column reordering function
#'
#' This function reorders a dataframe according to the order of identical elements of another dataframe
#' @param x The dataframe that needs to be reordered
#' @param matchData The dataframe that contains the column to match
#' @param xCol The column of x that needs to be reordered
#' @param newCol The index of the column in matchData ywith respect to which you want to reorder x
#' @keywords reorder
#' @export
#' @examples
#' reorderCols(x = my_df, matchData = new_df, xCol = 3, newCol = 4)
#' reorderCols()

reorderCols <- function(x, matchData, xCol, newCol) {
  t <- x
  t <- t[order(match(x[, xCol], matchData[, newCol])), ]
  return(t)
}

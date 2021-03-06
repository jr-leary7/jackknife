% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reorderCols.R
\name{reorderCols}
\alias{reorderCols}
\title{A column reordering function}
\usage{
reorderCols(x, match.data, x.col, new.col)
}
\arguments{
\item{x}{The dataframe that needs to be reordered}

\item{matchData}{The dataframe that contains the column to match}

\item{xCol}{The column of x that needs to be reordered}

\item{newCol}{The index of the column in matchData ywith respect to which you want to reorder x}
}
\description{
This function reorders a dataframe according to the order of identical elements of another dataframe
}
\examples{
reorderCols(x = my_df, match.data = new_df, x.col = 3, new.col = 4)
}

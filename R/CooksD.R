#' Calculate Cook's Distance.
#'
#' @description A helper function to calculate Cook's Distance outliers in GLMs. Meant to be called from CooksDPlot().
#' @param leverage The leverage of each point.
#' @param level The level above which an observation is considered "high-leverage."
#' @param model The model which you'd like to analyze.
#' @param positive Should positive or negative Cook's Distance values be computed?
#' @examples
#' CooksD(leverage = ., level = .5, model = my_lm, positive = TRUE)

CooksD <- function(leverage, level, model, positive) {
  val <- sqrt(level * length(coef(model)) * (1 - leverage) / leverage)
  if (!positive) { val <- -val }
  return(val)
}

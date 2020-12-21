#' Generate a QQ-plot.
#'
#' @description This function reimplements the base R QQ-plot using ggplot2.
#' @import ggplot2
#' @param model The model you'd like to analyze. Defaults to NULL.
#' @export
#' @examples
#' QQPlot(model = my_lm)

QQPlot <- function(model = NULL) {
  if (is.null(model)) {stop("Please provide a model object to analyze.")}
  if (!class(model) %in% c("lm", "glm")) {stop("Object is not of class lm or glm.")}
  ggplot(NULL, aes(sample = sort(rstandard(model)))) +
    stat_qq(alpha = .7) +
    stat_qq_line(color = "deepskyblue3", size = 1, linetype = 2) +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, size = 1),
          panel.grid = element_blank())
}

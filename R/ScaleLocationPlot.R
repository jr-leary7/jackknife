#' Generate a Scale-Location plot.
#'
#' @description Reimplements the fitted values vs. square root-transformed standardized residuals plot found in plot.lm() in ggplot2.
#' @param model The model which you'd like to analyze. Defaults to NULL. \
#' @export
#' @examples
#' ScaleLocationPlot(model = my_lm)

ScaleLocationPlot <- function(model = NULL) {
  if (is.null(model)) stop("Please provide a model object to analyze.")
  if (!class(model) %in% c("lm", "glm")) stop("Object is not of class lm or glm.")
  ggplot(NULL, aes(x = fitted(model), y = sqrt(abs(rstandard(model))))) +
    geom_point(alpha = .7) +
    geom_smooth(method = "loess",
                color = "firebrick3",
                se = FALSE,
                span = .9,
                size = .5,
                alpha = .7) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, size = 1),
          panel.grid = element_blank()) +
    labs(x = "Fitted Values", y = expression(sqrt("Standardized Residuals")))
}

#' Generate a residuals vs. fitted values plot.
#'
#' @description This function reimplements the first result from plot.lm() using ggplot2.
#' @import ggplot2
#' @param model The model you'd like to analyze. Defaults to NULL.
#' @export
#' @examples
#' ResidPlot(model = my_lm)

ResidPlot <- function(model = NULL) {
  if (is.null(model)) stop("Please provide a model object to analyze.")
  if (!class(model) %in% c("lm", "glm")) warning("Object is not of class lm or glm, this might not work.")
  ggplot(NULL, aes(x = fitted(model), y = resid(model))) +
    geom_hline(yintercept = 0, color = "grey3", lty = 5, alpha = .5) +
    geom_point() +
    geom_smooth(method = "loess",
                span = .97,
                se = FALSE,
                color = "firebrick3",
                size = .5,
                alpha = .7) +
    labs(x = "Fitted Values", y = "Residuals") +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, size = 1),
          panel.grid = element_blank())
}

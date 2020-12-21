#' Generate an outlier plot.
#'
#' @description Reimplements the standardized residuals vs. leverage plot from base R, along with the Cook's Distance cutoff lines for outliers, in ggplot2. Points with leverage values greater than 2p/n are colored blue.
#' @param model The model you'd like to analyze. Defaults to NULL.
#' @import ggplot2
#' @export
#' @examples
#' OutlierPlot(model = my_lm)

OutlierPlot <- function(model = NULL) {
  if (is.null(model)) stop("Please provide a model object to analyze.")
  if (!class(model) %in% c("lm", "glm")) stop("Object is not of class lm or glm.")
  n_vars <- length(coef(model)) - 1
  n_obs <- length(resid(model))
  max_resid <- ceiling(max(rstandard(model)))
  min_resid <- floor(min(rstandard(model)))
  ggplot(NULL) +
    geom_point(aes(x = hatvalues(model), y = rstandard(model)),
               alpha = .7,
               color = ifelse(hatvalues(model) > 2 * n_vars / n_obs, "dodgerblue", "black")) +
    geom_hline(yintercept = 0, color = "grey3", alpha = .5, lty = 5) +
    geom_smooth(aes(x = hatvalues(model), y = rstandard(model)),
                color = "firebrick3",
                method = "loess",
                se = FALSE,
                span = .97,
                size = .5) +
    stat_function(fun = CooksD,
                  xlim = c(0, .5),
                  args = list(model = model, level = .5, positive = TRUE),
                  lty = 2,
                  color = "forestgreen") +
    stat_function(fun = CooksD,
                  xlim = c(0, .5),
                  args = list(model = model, level = .5, positive = FALSE),
                  lty = 2,
                  color = "forestgreen") +
    scale_y_continuous(limits = c(min_resid - 1, max_resid + 1)) +
    scale_x_continuous(limits = c(0, max(hatvalues(model)) + .1)) +
    annotate("label",
             x = max(hatvalues(model)),
             y = max_resid - 1,
             color = "forestgreen",
             label = "Cook's Distance",
             label.size = .8) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA, size = 1),
          panel.grid = element_blank()) +
    labs(x = "Leverage", y = "Standardized Residuals")
}

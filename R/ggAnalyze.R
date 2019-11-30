#' A function to analyze a linear model visually
#'
#' This function returns the four plots returned by default plot.lm(), but using ggplot2
#' @param mod This is the model to be analyzed
#' @keywords model, linear, ggplot2
#' @export
#' @import ggplot2
#' @examples
#' ggAnalyze(mod = my_lm)

ggAnalyze <- function(mod) {
  # residuals vs. fitted
  res_fit <- ggplot(mapping = aes(x = mod$fitted.values,
                                  y = mod$residuals)) +
    geom_point() +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                span = .99,
                size = .5) +
    geom_hline(yintercept = 0, color = "grey") +
    theme_minimal() +
    labs(title = "Residuals vs. Fitted Values",
         x = "Fitted",
         y = "Residuals")

  # qq
  y <- mod$model[1]
  x <- mod$model[2:ncol(mod$model)]
  x <- sort(x$x)
  props <- 1:length(x) / (length(x) + 1)
  quants <- qnorm(props, mean = mean(x), sd = sd(x))
  qq <- ggplot(mapping = aes(x = quants,
                             y = x)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "red",
                size = .5) +
    theme_minimal() +
    labs(title = "Normal QQ",
         x = "Theretical Quantiles",
         y = "Sample")

  # scale-location
  scale_loc <- ggplot(mapping = aes(x = mod$fitted.values,
                                    y = sqrt(abs(rstandard(mod))))) +
    geom_point() +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                span = .99,
                size = .5) +
    theme_minimal() +
    labs(title = "Scale-Location",
         x = "Fitted",
         y = expression(sqrt("Standardized Residuals")))

  ## residuals vs. leverage
  res_lev <- ggplot(mapping = aes(x = hatvalues(mod),
                                  y = rstandard(mod))) +
    geom_point() +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                size = .5,
                span = .99) +
    geom_hline(yintercept = 0, color = "grey") +
    theme_minimal() +
    labs(title = "Residuals vs. Leverage",
         x = "Leverage",
         y = expression(sqrt("Standardized Residuals")))

  # return
  print(res_fit)
  print(qq)
  print(scale_loc)
  print(res_lev)
}

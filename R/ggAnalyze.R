#' A function to analyze a linear model visually
#'
#' This function is a reimplementation of the four plots returned by default plot.lm() using the ggplot2 library.
#' @param mod This is the model to be analyzed
#' @import ggplot2
#' @export
#' @examples
#' ggAnalyze(mod = my_lm)

ggAnalyze <- function(mod) {
  # residuals vs. fitted
  res_fit <- ggplot(mapping = aes(x = mod$fitted.values,
                                  y = mod$residuals)) +
    geom_point(pch = 1) +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                span = .97,
                size = .5) +
    geom_hline(yintercept = 0, color = "grey") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black",
                                     fill = NA,
                                     size = 1)) +
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
    geom_point(pch = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "red",
                size = .5) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black",
                                      fill = NA,
                                      size = 1)) +
    labs(title = "Normal QQ",
         x = "Theretical Quantiles",
         y = "Sample")

  # scale-location
  scale_loc <- ggplot(mapping = aes(x = mod$fitted.values,
                                    y = sqrt(abs(rstandard(mod))))) +
    geom_point(pch = 1) +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                span = .97,
                size = .5) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black",
                                      fill = NA,
                                      size = 1)) +
    labs(title = "Scale-Location",
         x = "Fitted",
         y = expression(sqrt("Standardized Residuals")))

  ## residuals vs. leverage
  res_lev <- ggplot(mapping = aes(x = hatvalues(mod),
                                  y = rstandard(mod))) +
    geom_point(pch = 1) +
    geom_smooth(method = "loess",
                se = FALSE,
                color = "red",
                size = .5,
                span = .97) +
    geom_hline(yintercept = 0, color = "grey") +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black",
                                      fill = NA,
                                      size = 1)) +
    labs(title = "Residuals vs. Leverage",
         x = "Leverage",
         y = expression(sqrt("Standardized Residuals")))

  # return
  par(ask = TRUE)
  plot(res_fit)
  plot(qq)
  plot(scale_loc)
  plot(res_lev)
  par(ask = FALSE)
}

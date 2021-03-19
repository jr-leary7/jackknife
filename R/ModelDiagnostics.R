#' A function to provide model diagnostic statistics.
#'
#' This function builds on the output of glance() in order to make it neater and more aesthetically pleasing.
#' @import dplyr
#' @import broomExtra
#' @import kableExtra
#' @param model The model to be analyzed. Defaults to NULL.
#' @param kable Should the output be placed in kable format, or returned as a simple dataframe? Defaults to TRUE.
#' @param kable.font What font should be used? Defaults to "Avenir".
#' @param kable.caption Define a caption to be placed above the table. Defaults to "Model Coefficients".
#' @param kable.digits The number of digits to round to. Defaults to 4.
#' @export
#' @examples
#' ModelDiagnostics(my_glm, font = "Comic Sans")

ModelDiagnostics <- function (model = NULL,
                              kable = TRUE,
                              kable.font = "Avenir",
                              kable.caption = NULL,
                              kable.digits = 4) {
  # check parameters
  if (is.null(model)) stop("You forgot to provide a model.")
  # generate GLM summary table
  if ("glm" %in% class(model) && family(model)$family != "binomial") {
    model %>%
      glance_performance() %>%
      select(null.deviance, df.null, deviance, df.residual, aic, nobs, r2.nagelkerke, rmse) %>%
      rename(`Null Deviance` = null.deviance, `Null DF` = df.null, Deviance = deviance,
             `Residual DF` = df.residual, AIC = aic, N = nobs, `Nagelkerke R2` = r2.nagelkerke, RMSE = rmse) -> sum_temp
  } else if ("glm" %in% class(model) && family(model)$family == "binomial") {
    model %>%
      glance_performance() %>%
      select(null.deviance, df.null, deviance, df.residual, aic, nobs, r2.tjur, pcp) %>%
      rename(`Null Deviance` = null.deviance, `Null DF` = df.null, Deviance = deviance,
             `Residual DF` = df.residual, AIC = aic, N = nobs, `Tjur R2` = r2.tjur, PCP = pcp) -> sum_temp
  } else if (class(model) == "lm") {
    model %>%
      glance_performance() %>%
      select(adj.r.squared, sigma, statistic, p.value, aic, nobs, rmse) %>%
      rename(`Adj. R2` = adj.r.squared, RSE = sigma, `F-statistic` = statistic, `P-value` = p.value,
             AIC = aic, N = nobs, RMSE = rmse)-> sum_temp
  }
  # generate kable if desired
  if (kable) {
    sum_temp %>%
      kbl(digits = kable.digits, booktabs = TRUE, caption = kable.caption) %>%
      kable_minimal("hover", full_width = FALSE, html_font = kable.font) -> final_table
  } else {
    sum_temp -> final_table
  }
  return(final_table)
}

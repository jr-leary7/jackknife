#' A function to provide a summary of a linear model.
#'
#' This function takes in the output from lm() or glm() and provides a tidy, aesthetically-pleasing summary of the coefficients.
#' @param model The model to be analyzed. Defaults to NULL.
#' @param conf.level The confidence level for determining variable significance. Defaults to 95% aka 0.05.
#' @param kable Should the table be returned as an interactive kable instead of a simple dataframe? Defaults to TRUE.
#' @param kable.font What font should be used? Defaults to "Avenir".
#' @param kable.caption Define a caption to be placed above the table. Defaults to "Model Coefficients".
#' @param kable.digits The number of digits to round to. Defaults to 4.
#' @import dplyr
#' @import broom
#' @import kableExtra
#' @export
#' @examples
#' CoefSummary(model = my_glm, font = "Georgia")

CoefSummary <- function (model = NULL,
                         kable = TRUE,
                         conf.level = 0.05,
                         kable.font = "Avenir",
                         kable.caption = NULL,
                         kable.digits = 4) {
  # check parameters
  if (is.null(model)) stop("You forgot to provide a model.")
  # summarize data
  model %>%
    tidy() %>%
    rename(Variable = term, Beta = estimate, `Std. Error` = std.error, `T-statistic` = statistic, `P-value` = p.value) %>%
    mutate(Significant = ifelse(`P-value` < conf.level, "*", "")) -> coef_temp
  # exponentiate coefficients if criteria are met
  if ("glm" %in% class(model) && family(model)$link == "log") {
    coef_temp %>%
      mutate(`Exp(Beta)` = exp(Beta)) %>%
      relocate(Variable, Beta, `Exp(Beta)`) -> coef_temp
  }
  # create kable output if desired
  if (kable) {
    coef_temp %>%
      kbl(digits = kable.digits, booktabs = TRUE, caption = kable.caption) %>%
      kable_minimal("hover", full_width = FALSE, html_font = kable.font) -> final_table
  } else {
    final_table <- coef_temp
  }
  return(final_table)
}


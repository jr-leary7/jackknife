#' A function to negate the %in% operator.
#'
#' This function returns the complement / inverse of the %in% operator. Useful for subsetting, etc.
#'
#' @export
#' @examples
#' df[df$var %!in% pattern, ]

`%!in%` <- Negate(`%in%`)

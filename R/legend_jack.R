#' A function to control the legend.
#'
#' This function will serve as an easier-to-use wrapper around guide_legend().
#' @import ggplot2
#' @param legend.rows How many rows should the legend have? Mostly useful for top/bottom legends. Defaults to 1.
#' @param key.size How large should the legend icons be? Defaults to 3.
#' @export
#' @examples
#' legend_jack(nrow = 2)

legend_jack <- function(legend.rows = 1, key.size = 3) {
  g <- guides(color = guide_legend(nrow = legend.rows, override.aes = list(size = key.size)))
  return(g)
}

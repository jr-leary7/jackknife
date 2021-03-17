#' A function for easily modifying themes.
#'
#' This function allows easy modification of a minimalist `ggplot2` theme - remove the grid, change fonts, etc.
#' @import ggplot2
#' @param grid Should the plot include gridlines? Defaults to FALSE.
#' @param legend Should the legend be drawn? Defaults to TRUE.
#' @param border Should a rectangular border be drawn around the plot? Defaults to TRUE.
#' @param font A string detailing which font should be used on all plot text. Must be capitalized correctly. Defaults to NULL.
#' @param axis.nums Should the axes show number labels? Defaults to TRUE.
#' @param legend.pos Where should the legend be located? One of "left", "right", "bottom", "top". Defaults to "bottom".
#' @param center.title Should the plot title and subtitle be centered? Defaults to TRUE.
#' @param y.axis.horiz Should the y-axis be horizontal in order to make reading easier? Defaults to TRUE.
#' @export
#' @examples
#' theme_jack(grid = TRUE, font = "Helvetica")

theme_jack <- function(grid = FALSE,
                       legend = TRUE,
                       border = TRUE,
                       font = NULL,
                       ticks = FALSE,
                       axis.nums = TRUE,
                       legend.pos = "bottom",
                       center.title = TRUE,
                       y.axis.horiz = TRUE) {
  t <- theme_minimal()
  if (!grid) t <- t + theme(panel.grid = element_blank())
  if (!legend) t <- t + theme(legend.position = "none")
  if (border) t <- t + theme(panel.border = element_rect(fill = NA, size = 1))
  if (!is.null(font)) t <- t + theme(text = element_text(family = font))
  if (ticks) t <- t + theme(axis.ticks = element_line())
  if (!axis.nums) t <- t + theme(axis.text = element_blank())
  if (center.title) t <- t + theme(plot.title = element_text(hjust = 0.5),
                                   plot.subtitle = element_text(hjust = 0.5),
                                   plot.title.position = "plot")
  if (y.axis.horiz) t <- t + theme(axis.title.y = element_text(angle = 360, vjust = 0.5))
  t <- t + theme(legend.position = legend.pos)
  return(t)
}

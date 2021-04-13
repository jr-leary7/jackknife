#' Extract the legend from a ggplot2 object.
#'
#' This function pulls out the legend box from a ggplot2 object, which you can then arrange with other objects using e.g. patchwork.
#' @import ggplot2
#' @param my.plot The plot from which to extract the legend. Defaults to NULL.
#' @export
#' @examples
#' ExtractLegend(plot1)

ExtractLegend <- function(my.plot = NULL) {
  # check input
  if (is.null(my.plot)) stop("Please supply a ggplot2 object as input.")
  if (!("ggplot" %in% class(my.plot))) stop("Plot must be of class ggplot.")

  # extract legend
  gg <- ggplot_gtable(ggplot_build(my.plot))
  i_am_legend <- gg$grobs[[which(sapply(gg$grobs, function(x) x$name) == "guide-box")]]
  return(i_am_legend)
}

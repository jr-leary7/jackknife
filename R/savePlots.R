#' A Plot Saving Function
#'
#' This function saves a list of plots of gene expression as .png files in a user-provided directory
#' @param plotList A list of previously generated plots
#' @param geneList A list of gene names (or other labels) to use as filenames
#' @param wd The directory to which you wish to save the images
#' @export
#' @examples
#' savePlots(plot.list = my_plots, gene.list = myy_enes, wd = "~/username/plots")


savePlots <- function(plot.list, gene.list, wd) {
  setwd(wd)
  for(i in 1:length(plotList)) {
    png(paste0(as.character(geneList$geneID), ".png"))
    print(plotList[[i]])
    dev.off()
  }
}

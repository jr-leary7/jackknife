#' A function for plotting enrichment scores after GSEA
#'
#' This function returns a list of enrichment plots after running GSEA using the 'fgsea' package
#' @param geneSetLists A list of gene sets
#' @param numPlots The number of plots you wish to generate
#' @param fgseaRes The results obtained from runnning fgsea()
#' @param ranks The ranks obtained from running fgsea()
#' @keywords enrichment
#' @export
#' @examples
#' plotES(geneSetList = myGeneList, numPlots = 5, fgseaRes = myResults, ranks = myRanks)





plotES <- function(geneSetList, numPlots, fgseaRes, ranks) {
  plotList <- list()
  t <- head(fgseaRes[order(padj), ], n = numPlots)
  pathwayList <- as.list(t$pathway)
  for(i in 1:length(pathwayList)) {
    plotList[[i]] <- plotEnrichment(geneSetList[[pathwayList[[i]]]], ranks)
  }
  return(plotList)
}


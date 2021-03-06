% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotEnrichmentScores.R
\name{plotES}
\alias{plotES}
\title{A function for plotting enrichment scores after GSEA}
\usage{
plotES(gene.set.list, num.plots, fgsea.res, ranks)
}
\arguments{
\item{ranks}{The ranks obtained from running fgsea()}

\item{geneSetLists}{A list of gene sets}

\item{numPlots}{The number of plots you wish to generate}

\item{fgseaRes}{The results obtained from runnning fgsea()}
}
\description{
This function returns a list of enrichment plots after running GSEA using the 'fgsea' package
}
\examples{
plotES(gene.set.list = gene_list, num.plots = 5, fgsea.res = my_results, ranks = my_ranks)
}

#' A function to caluclate the proportion of cell types from a Seurat object
#'
#'This function takes as input a Seurat object, which contains cell types identified by the SingleR package. The function calculates the relative amount of each cell type.
#' @param data The Seurat object containing cell cluster identities
#' @keywords proportions, cell types, Seurat
#' @export
#' @examples
#' calcProps(data = my_seurat_object)



calcProps <- function(data) {
  if (is.null(data$SingleR.labels.bulk) & is.null(data$SingleR.labels.sc)) {
    stop("You either haven't assigned cell identities, or you misnamed them.")
  }

  if (!is.null(data$SingleR.labels.bulk)) {
    cell_types_bulk <- unique(data$SingleR.labels.bulk)
    bulk_props <- c()
    for (i in seq(cell_types_bulk)) {
      bulk_props[i] <- length(which(data$SingleR.labels.bulk == cell_types_bulk[i])) / length(data$SingleR.labels.bulk)
    }
    bulk_df <- data.frame(bulk_props, cell_types_bulk)
  }

  if (!is.null(data$SingleR.labels.sc)) {
    cell_types_sc <- unique(data$SingleR.labels.sc)
    sc_props <- c()
    for (i in seq(cell_types_sc)) {
      sc_props[i] <- length(which(data$SingleR.labels.sc == cell_types_sc[i])) / length(data$SingleR.labels.sc)
    }
    sc_df <- data.frame(sc_props, cell_types_sc)
  }

  if (!is.null(data$SingleR.labels.bulk) & !is.null(data$SingleR.labels.sc)) {
    t <- list(bulk_df, sc_df)
    return(t)
  } else {
    if (is.null(data$SingleR.labels.bulk)) {
      t <- list(bulk_df)
      return(t)
    } else {
      if (!is.null(data$SingleR.labels.bulk)) {
        t <- list(sc_df)
        return(t)
      }
    }
  }
}

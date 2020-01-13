#' A function to caluclate the proportion of cell types from a Seurat object
#'
#'This function takes as input a Seurat object, which contains cell types identified by the SingleR package. The function calculates the relative amount of each cell type.
#' @param data The Seurat object containing cell cluster identities
#' @keywords proportions, cell types, Seurat
#' @export
#' @examples
#' calcProps(data = my_seurat_object)



calcProps <- function(data) {
  # check to make sure labels exist
  if (!("SingleR.labels.bulk" %in% colnames(data@meta.data)) & !("SingleR.labels.sc" %in% colnames(data@meta.data)) & !("SingleR.labels.bulk.fine" %in% colnames(data@meta.data))) {
    stop("You either haven't assigned cell identities, or you misnamed them.")
  }

  # conditional statements for creating dataframes to return
  if ("SingleR.labels.bulk" %in% colnames(data@meta.data)) {
    cell_types_bulk <- unique(data$SingleR.labels.bulk)
    bulk_props <- c()
    for (i in seq(cell_types_bulk)) {
      bulk_props[i] <- length(which(data$SingleR.labels.bulk == cell_types_bulk[i])) / length(data$SingleR.labels.bulk)
    }
    bulk_df <- data.frame(bulk_props, cell_types_bulk)
  }

  if ("SingleR.labels.sc" %in% colnames(data@meta.data)) {
    cell_types_sc <- unique(data$SingleR.labels.sc)
    sc_props <- c()
    for (i in seq(cell_types_sc)) {
      sc_props[i] <- length(which(data$SingleR.labels.sc == cell_types_sc[i])) / length(data$SingleR.labels.sc)
    }
    sc_df <- data.frame(sc_props, cell_types_sc)
  }

  if ("SingleR.labels.bulk.fine" %in% colnames(data@meta.data)) {
    cell_types_bulk_fine <- unique(data$SingleR.labels.bulk.fine)
    bulk_fine_props <- c()
    for (i in seq(cell_types_bulk_fine)) {
      bulk_fine_props[i] <- length(which(data$SingleR.labels.bulk.fine == cell_types_bulk_fine[i])) / length(data$SingleR.labels.bulk.fine)
    }
    bulk_fine_df <- data.frame(bulk_fine_props, cell_types_bulk_fine)
  }

  # return statement architecture -- could make more precise but not really necessary right now
  if ("SingleR.labels.bulk" %in% colnames(data@meta.data) & "SingleR.labels.sc" %in% colnames(data@meta.data) & "SingleR.labels.bulk.fine" %in% colnames(data@meta.data)) {
    t <- list(bulk_df, sc_df, bulk_fine_df)
    return(t)
  } else {
    if ("SingleR.labels.bulk" %in% colnames(data@meta.data) & "SingleR.labels.sc" %in% colnames(data@meta.data)) {
      t <- list(bulk_df, sc_df)
      return(t)
    }
  }
}

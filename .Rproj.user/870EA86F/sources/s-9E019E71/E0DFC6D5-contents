#' An RMarkdown function
#'
#' This function renders an RMarkdown file containing scRNAseq data on the longleaf HPC cluster
#' @param
#' @keywords longleaf
#' @export
#' @examples
#' renderRMD()

renderRMD <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  cmd1 <- rmarkdown::render(input = paste0("/nas/longleaf/home/", args[3], "/singleCell/", args[2], "/", args[2], "_longleaf.Rmd"),
                            output_format = "html_document",
                            output_file = paste0(args[2], "_longleaf.html"),
                            output_dir = paste0("/nas/longleaf/home/", args[3], "/singleCell/", args[2], "/", args[2]))
  print(cmd1)
  system(cmd1)
  Sys.sleep(0.1)
}

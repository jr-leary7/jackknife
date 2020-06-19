#' A function for cleaner plots.
#'
#' This function is a `ggplot2` theme designed with dimension reduction plots (think PCA, t-SNE, UMAP) in mind, but can be applied to other types of plots as needed. It removes axis ticks and text, and changes the font to Helvetica.
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @export
#' @examples
#' themeYehLab()

themeYehLab <- function() {
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Helvetica", size = 12),
        axis.title.x = element_text(family = "Helvetica", size = 12),
        axis.title.y = element_text(family = "Helvetica", size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(family = "Helvetica", size = 8),
        plot.title = element_text(family = "Helvetica", size = 12, face = "bold"))
}

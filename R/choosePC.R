#' A function to choose principal components
#'
#' This function chooses an optimal number of principal components to explain the variance in your data. The amount of variance explained by the principal components is compared to the amount of variance explained by the principle components of a random reshuffing of the columns of the original dataframe, and the two are compared.
#' @param data The data on which PCA will be performed
#' @param plot Whether or not a plot should be returned comparing the two PCA, default is FALSE
#' @param seed The seed to use for reshuffling dataframe columns, default is 629
#' @param center Whether or not data should be centered prior to PCA, default is TRUE
#' @param scale Whether or not data should be scaled prior to PCA, default is TRUE
#' @param perms How many permutations to perform on the data, default is 10
#' @keywords PCA, t-SNE, dimension reduction
#' @export
#' @examples
#' choosePC(data = my_df, plot = TRUE, center = TRUE, scale = FALSE)

choosePC <- function (data, perms, plot, seed, center, scale) {

  if (is.null(plot)) {
    plot <- FALSE
  }

  if (is.null(perms)) {
    perms <- 10
  }

  if (is.null(seed)) {
    seed <- 629
  }

  if (is.null(center)) {
    center <- TRUE
  }

  if (is.null(scale)) {
    scale = TRUE
  }

  df <- data
  pc <- prcomp(df, center = center, scale = scale)
  var_expl <- pc$sdev ^ 2 / sum(pc$sdev ^ 2)

  var_expl_perm <- matrix(NA, ncol = length(pc$sdev), nrow = perms)

  for (i in seq(perms)) {
    df_perm <- apply(df, 2, sample)
    pc_perm <- prcomp(df_perm, center = center, scale = scale)
    var_expl_perm[i, ] <- pc_perm$sdev ^ 2 / sum(pc_perm$sdev ^ 2)
  }

  col_means_perm <- colMeans(var_expl_perm)
  opt_pc <- tail(which(var_expl > col_means_perm), 1)

  if (plot == TRUE) {
    mean_var_perm <- colSums(var_expl_perm) / perms
    p <- ggplot(mapping = aes(x = seq(ncol(var_expl_perm)))) +
      geom_line(mapping = aes(y = var_expl),
                color = "green") +
      geom_line(mapping = aes(y = col_means_perm),
                color = "red") +
      labs(x = "Principal Components",
           y = "Percentage of Variance Explained") +
      geom_vline(xintercept = opt_pc, color = "dodgerblue") +
      theme_minimal() +
      theme(legend.position = "top")
    print(p)
  }

  return(opt_pc)
}












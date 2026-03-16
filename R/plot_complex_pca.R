#' Plot Complex PCA
#'
#' Evaluates and plots a custom PCA biplot using input data.
#'
#' @param input_data A data frame containing parameters and values.
#' @return A ggplot object.
#' @export
#' @import FactoMineR factoextra ggplot2
plot_complex_pca <- function(input_data) {
  # Select variables for PCA (Columns 5 to 14 from example)
  dataPCA <- input_data[, 5:14]

  pca <- FactoMineR::PCA(dataPCA, graph = FALSE)

  # Create base plot
  p1 <- factoextra::fviz_pca_biplot(pca,
      palette = "Accent2",
      geom.ind = "point",
      pointshape = 21,
      pointsize = input_data$Concentration,
      fill.ind = input_data$Type,
      geom.var = c("text","arrow"),
      habillage = "none",
      col.var = "black",
      legend.title = list(fill = "Types", shape = "Group", size = "Concentration (μM)"),
      legend.key.size = 5,
      repel = TRUE,
      mean.point = FALSE,
      labelsize = 6,
      title = ""
  )

  # Add annotation
  p2 <- p1 + ggplot2::geom_point(ggplot2::aes(shape=input_data$Group, col=input_data$Types)) +
      ggplot2::theme(text = ggplot2::element_text(family = "Times", face = "bold", size = 18), 
                     axis.title = ggplot2::element_text(size = 16),
                     axis.text = ggplot2::element_text(size = 16), 
                     axis.line = ggplot2::element_line(linewidth = 1),
                     panel.grid.major = ggplot2::element_blank(), 
                     panel.grid.minor = ggplot2::element_blank()) + 
      # ggplot2::annotate("text", x= 6.5, y = 4, label = "Positive Control") +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 5))) +
      ggplot2::scale_fill_discrete(breaks=c("3 nm", "6 nm", "8 nm", "12 nm", "Neg")) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 5))) +
      ggplot2::labs(x = "PC1", y = "PC2")

  return(p2)
}

#' @title heatmapPlot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @importFrom ggplot2 ggplot aes geom_tile theme element_text
#' @importFrom cowplot get_legend plot_grid theme_map
#' @export
#' @examples
heatmapPlot <- function(experiment, assay = 1, autoScale = TRUE,
                     logTransform = TRUE,
                     cols = colnames(colData(experiment))){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)
  colnames(m)[colnames(m) == "value"] <- assay

  p <- ggplot(m, aes(x = .data$col, y = .data$row,
                     fill = .data[[assay]])) +
    geom_tile() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

  colplots <- lapply(cols, function(col){
    colplot <- ggplot(m, aes(x = .data$col, y = 1, fill = .data[[col]])) +
      geom_tile() +
      theme_void()  +
      theme(legend.position = "top")
  })
  legends <- plot_grid(plotlist = lapply(colplots, cowplot::get_legend))
  colplots <- plot_grid(plotlist = lapply(colplots, function(x){
    x + theme(legend.position = "none")
  }), ncol = 1)

  plot_grid(legends, colplots, p, ncol = 1,
            rel_heights = c(0.2, 0.05 * length(cols), 2),
            align = "v", axis = "lr") + cowplot::theme_map()
}

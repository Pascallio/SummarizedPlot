#' @title Scatter Plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
scatterPlot <- function(){

}

#' @title heatmapPlot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @importFrom ggplot2 ggplot aes geom_tile theme element_text
#' @importFrom cowplot get_legend plot_grid theme_map
#' @importFrom ggplotify as.ggplot
#' @export
#' @examples
heatmapPlot <- function(experiment, assay = 1, scaling = "auto", log = exp(1),
                        rows = NULL, columns = NULL, ...){
  assay <- getAssay(experiment, assay)
  m <- transformMatrix(assay(experiment, assay), log, scaling)

  if (!is.null(rows)) {
    rows <- as.data.frame(rowData(experiment)[, rows, drop = FALSE])
  }

  if (!is.null(columns)) {
    columns <- as.data.frame(colData(experiment)[, columns, drop = FALSE])
  }


  as.ggplot(pheatmap::pheatmap(
    m,
    annotation_col = columns,
    annotation_row = rows,
    cluster_rows = !NA %in% m,
    cluster_cols = !NA %in% m,
    ...
  ))
}

#' @title Correlogram
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
correlogramPlot <- function(){

}

#' @title 2D density plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
density2dPlot <- function(){

}

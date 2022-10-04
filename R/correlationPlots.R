#' @title Scatter Plot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_point
#' @export
#' @examples
scatterPlot <- function(experiment, assay = 1, annotate = NULL,
                        scaling = "auto", log = exp(1), ...){
  experiment <- experiment[, 1:2]
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log)

  m2 <- m[m$col == colnames(experiment)[1], ]
  m3 <- m[m$col == colnames(experiment)[2], ]

  m2$value2 <- m3$value
  rm(m)

  mapping <- aes(x = .data$value, y = .data$value2)
  if (!is.null(annotate) && annotate %in% colnames(m2)) {
    mapping <- aes(x = .data$value, y = .data$value2, color = .data[[annotate]])
  }

  p <- ggplot(m2, mapping) +
    geom_point(...) +
    xlab(colnames(experiment)[1]) +
    ylab(colnames(experiment)[2])

  if (!is.null(annotate)) p <- p + labs(fill = annotate)
  p

}

#' @title heatmapPlot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @inheritDotParams pheatmap::pheatmap -mat -cluster_rows -cluster_cols
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
#' @export
#' @examples
density2dPlot <- function(experiment, assay = 1, annotate = NULL,
                          scaling = "auto", log = exp(1), ...){

  experiment <- experiment[, 1:2]
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log)

  m2 <- m[m$col == colnames(experiment)[1], ]
  m3 <- m[m$col == colnames(experiment)[2], ]

  m2$value2 <- m3$value
  rm(m)

  mapping <- aes(x = .data$value, y = .data$value2)
  if (!is.null(annotate) && annotate %in% colnames(m2)) {
    mapping <- aes(x = .data$value, y = .data$value2, color = .data[[annotate]])
  }

  p <- ggplot(m2, mapping) +
    geom_density2d(...)  +
    xlab(colnames(experiment)[1]) +
    ylab(colnames(experiment)[2])

  if (!is.null(annotate)) p <- p + labs(color = annotate)
  p
}

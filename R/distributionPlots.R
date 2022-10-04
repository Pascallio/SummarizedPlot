#' @title Vilion plot
#' @description
#' @details
#' @returns
#' @param fill
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_violin
#' @export
#' @examples
violinPlot <- function(experiment, assay = 1, annotate = NULL, scaling = "auto",
                       log = exp(1), transpose = FALSE, ...){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)

  mapping <- aes(x = .data$col, y = .data$value, fill = .data$col)
  if (!is.null(annotate) && annotate %in% colnames(m)) {
    mapping <- aes(x = .data$col, y = .data$value, fill = .data[[annotate]])
  }

  p <- ggplot(m, mapping) +
    geom_violin(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)

  if (!is.null(annotate)) p <- p + labs(fill = annotate)
  p
}

#' @title Density plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_density
#' @export
#' @examples
densityPlot <- function(experiment, assay = 1, annotate = NULL, scaling = "auto",
                        log = exp(1), transpose = FALSE, ...){
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)

  mapping <- aes(x = .data$value, fill = .data$col)
  if (!is.null(annotate) && annotate %in% colnames(m)) {
    mapping <- aes(x = .data$col, fill = .data[[annotate]])
  }

  ggplot(m, mapping) +
    geom_density(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Density")
}

#' @title Histogram
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_histogram -stat
#' @importFrom ggplot2 geom_histogram
#' @export
#' @examples
histogramPlot <- function(experiment, assay = 1, scaling = "auto", log = exp(1),
                          transpose = FALSE, ...){
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)

  mapping <- aes(x = .data$value, group = .data$col, fill = .data$col)
  ggplot(m, mapping) +
    geom_histogram(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Count")
}

#' @title BarPlot
#' @description
#' @details
#' @returns
#' @param fill
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_boxplot
#' @export
#' @examples
boxPlot <- function(experiment, assay = 1, annotate = NULL, scaling = "auto",
                    log = exp(1), transpose = FALSE, ...){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)

  mapping <- aes(x = .data$col, y = .data$value, fill = .data$col)
  if (!is.null(annotate) && annotate %in% colnames(m)) {
    mapping <- aes(x = .data$col, y = .data$value, fill = .data[[annotate]])
  }

  p <- ggplot(m, mapping) +
    geom_boxplot(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)

  if (!is.null(annotate)) p <- p + labs(fill = annotate)
  p
}

#' @title Ridgeplot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
ridgePlot <- function(){

}

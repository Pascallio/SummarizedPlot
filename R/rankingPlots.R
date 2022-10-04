#' @title Bar plot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @export
#' @examples
barPlot <- function(experiment, assay = 1, annotate = NULL, scaling = "auto",
                    log = exp(1), transpose = FALSE){
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)
  mapping <- aes(x = .data$col, y = .data$value)
  if (!is.null(annotate) && annotate %in% colnames(m)) {
    mapping <- aes(x = .data$col,
                   y = .data$value,
                   fill = .data[[annotate]])
  }

  ggplot(m, mapping) +
    geom_bar(stat = "identity") +
    xlab("row") +
    ylab(assay) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

#' @title Lollipop plot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @inheritDotParams ggplot2::geom_point -mapping
#' @export
#' @examples
lollipopPlot <- function(experiment, assay = 1, annotate = NULL, scaling = "auto",
                         log = exp(1), transpose = FALSE, ...){

  if (!transpose) {
    experiment <- experiment[1, ]
  } else {
    experiment <- experiment[, 1]
  }
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)
  mapping <- aes(x = .data$col, y = .data$value)
  if (!is.null(annotate) && annotate %in% colnames(m)) {
    mapping <- aes(x = .data$col,
                   y = .data$value,
                   color = .data[[annotate]])
  }

  ggplot(m, mapping) +
    geom_point(mapping, ...) +
    geom_segment(aes(x = .data$col, y = 0, yend = .data$value, xend = .data$col)) +
    ylab(assay) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

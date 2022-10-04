#' @title Prepare data for plotting
#' @description
#' @details
#' @returns
#' @param experiment SummarizedExperiment object with at least
#' one (matrix) assay.
#' @param assay Name or index of the assay to plot. Defaults to
#' the first assay in [assayNames].
#' @param scaling Should autoscaling be performed on the assay?
#' Defaults to `TRUE`.
#' @param log Should the data be log transformed?
#' Defaults to `TRUE`
#' @importFrom ggplot2 ggplot aes geom_tile theme element_text
prepareData <- function(experiment, assay = 1, scaling = "auto",
                        log = exp(1), transpose = FALSE){

  assay <- assay(experiment, assay)
  m <- reshape2::melt(transformMatrix(assay, log, scaling))
  colnames(m) <- c("row", "col", "value")
  if (transpose) colnames(m) <- c("col", "row", "value")
  m <- cbind(m, colData(experiment), rowData(experiment), row.names = NULL)
  m[is.finite(m$value), ]
}

transformMatrix <- function(matrix, log = NULL, scaling = NULL,
                            transpose = FALSE){
  matrix <- as.matrix(matrix)
  if (transpose) matrix <- t(matrix)

  if (!is.null(log)) {
    matrix <- log(matrix, base = log)
    matrix[is.infinite(matrix)] <- 0
  }

  if (!is.null(scaling)) {
    matrix <- switch (scaling,
      "auto" = scale(na.omit(matrix), center = TRUE, scale = TRUE),
      "pareto" = apply(matrix, 2, function(x) (x - mean(x)) / sqrt(sd(x))),
      matrix
    )
  }

  matrix
}

getAssay <- function(experiment, assay){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }
  assay
}



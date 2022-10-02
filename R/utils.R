#' @title Prepare data for plotting
#' @description
#' @details
#' @returns
#' @param experiment SummarizedExperiment object with at least
#' one (matrix) assay.
#' @param assay Name or index of the assay to plot. Defaults to
#' the first assay in [assayNames].
#' @param autoScale Should autoscaling be performed on the assay?
#' Defaults to `TRUE`.
#' @param logTransform Should the data be log transformed?
#' Defaults to `TRUE`
#' @importFrom ggplot2 ggplot aes geom_tile theme element_text
prepareData <- function(experiment, assay = 1, scaling = "auto", log = exp(1)){

  m <- transformMatrix(assay(experiment, assay), log, scaling)

  cols <- rep(colData(experiment), nrow(m))
  cols <- cols[order(rownames(cols)), , drop = F]
  rownames(cols) <- seq_len(nrow(cols))

  m <- as.data.frame(stack(m))
  m <- data.frame(m, cols)
  m[complete.cases(m) & is.finite(m$value), ]
}

transformMatrix <- function(matrix, log = NULL, scaling = NULL){
  matrix <- as.matrix(matrix)

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

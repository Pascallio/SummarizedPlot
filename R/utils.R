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
prepareData <- function(experiment, assay = 1, autoScale = TRUE,
                        logTransform = TRUE){
  m <- assay(experiment, assay)

  if (logTransform) m <- log(m)

  if (autoScale) m <- scale(na.omit(m), center = TRUE, scale = TRUE)

  cols <- rep(colData(experiment), nrow(m))
  cols <- cols[order(rownames(cols)), , drop = F]
  rownames(cols) <- seq_len(nrow(cols))

  m <- as.data.frame(stack(m))
  m <- data.frame(m, cols)
  m[complete.cases(m) & is.finite(m$value), ]
}

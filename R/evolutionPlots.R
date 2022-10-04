#' @title Line Plot
#' @description
#' @details
#' @returns
#' @inheritParams prepareData
#' @export
#' @examples
linePlot <- function(experiment, assay = 1, color = NULL, scaling = "auto",
                     log = exp(1), transpose = FALSE, ...){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log, transpose)
  mapping <- aes(x = .data$row, y = .data$value, group = .data$col, color = .data$col)
  if (!is.null(color) && color %in% colnames(m)) {
    mapping <- aes(x = .data$col, y = .data$value, group = .data$col,
                   color = .data[[color]])
  }
  ggplot(m, mapping) +
    geom_line(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
}

#' @title Area plot
#' @description
#' @details
#' @returns
#' @param
#' @param
#' @export
#' @examples
areaPlot <- function(){

}

#' @title Stacked Area plot
#' @description
#' @details
#' @returns
#' @param
#' @param
#' @export
#' @examples
stackedAreaPlot <- function(){

}

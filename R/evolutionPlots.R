#' @title Line Plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
linePlot <- function(experiment, assay = 1, scaling = "auto", log = exp(1)){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log)

  ggplot(m, aes(x = .data$row, y = .data$value,
                group = .data$col, color = .data$col)) +
    geom_line() +
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

#' @title Vilion plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
violinPlot <- function(experiment, assay = 1, ...){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, ...)

  ggplot(m, aes(x = .data$col, y = .data$value)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
}

#' @title Density plot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
densityPlot <- function(experiment, assay = 1, scaling = "auto", log = exp(1)){
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log)

  ggplot(m, aes(x = .data$value, group = .data$col,
                fill = .data$col)) +
    geom_density(alpha = 0.5) +

    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Density")
}

#' @title Histogram
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
histogramPlot <- function(experiment, assay = 1, scaling = "auto", log = exp(1),
                          ...){
  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, scaling, log)


  ggplot(m, aes(x = .data$value, group = .data$col,
                fill = .data$col)) +
    geom_histogram(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Density")
}

#' @title BarPlot
#' @description
#' @details
#' @returns
#' @param experiment
#' @param assay
#' @export
#' @examples
boxPlot <- function(experiment, assay = 1, fill = NULL, ...){

  assay <- getAssay(experiment, assay)
  m <- prepareData(experiment, assay, ...)
  m$fill <- NULL

  if (!is.null(fill)) {
    m$fill <- fill
  }

  ggplot(m, aes(x = .data$col, y = .data$value, fill = .data$fill)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
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

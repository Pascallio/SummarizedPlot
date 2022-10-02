densityPlot <- function(experiment, assay = 1, autoScale = TRUE,
                        logTransform = TRUE){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)


  ggplot(m, aes(x = .data$value, group = .data$col,
                fill = .data$col)) +
    geom_density(alpha = 0.5) +

    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Density")
}

histogramPlot <- function(experiment, assay = 1, autoScale = TRUE,
                        logTransform = TRUE, ...){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)


  ggplot(m, aes(x = .data$value, group = .data$col,
                fill = .data$col)) +
    geom_histogram(...) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab(assay) +
    ylab("Density")
}

boxPlot <- function(experiment, assay = 1, autoScale = TRUE,
                    logTransform = TRUE){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)


  ggplot(m, aes(x = .data$col, y = .data$value)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
}

violinPlot <- function(experiment, assay = 1, autoScale = TRUE,
                       logTransform = TRUE){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)


  ggplot(m, aes(x = .data$col, y = .data$value)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
}

linePlot <- function(experiment, assay = 1, autoScale = TRUE,
                    logTransform = TRUE){
  if (is(assay, "numeric")) {
    assay <- assayNames(experiment)[assay]
  }

  m <- prepareData(experiment, assay, autoScale, logTransform)


  ggplot(m, aes(x = .data$row, y = .data$value,
                group = .data$col, color = .data$col)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab(assay)
}

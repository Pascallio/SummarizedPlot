library(SummarizedPlot)
data(airway, package = "airway")
experiment <- airway[1:100,]
rowData(experiment)$abc <- rep(1:(nrow(experiment) / 2), 2)

# Done and tested
# violinPlot(experiment[1:8, ], transpose = T, annotate = "abc")
# densityPlot(experiment[1:22, ], transpose = T, annotate = "abc")
# linePlot(experiment[1:10], transpose = T, annotate = "abc")
# histogramPlot(experiment[1:10, ], transpose = F) # no annotate
# boxPlot(experiment[1:10], transpose = T, annotate = "cell")
# scatterPlot(experiment[,c(1,2)], annotate = "dex")
# density2dPlot(experiment, annotate = "dex") # sometimes weird with rowData?
# barPlot(experiment, transpose = T, scaling = NULL, annotate = "abc")
# lollipopPlot(experiment, transpose = T, scaling = NULL, annotate = "abc")

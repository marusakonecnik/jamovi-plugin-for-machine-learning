
# This file is a generated template, your changes will not be overwritten

classificationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "classificationClass",
    inherit = classificationBase,
    private = list(
        .init = function() {
            confusion <- jmvcore::Preformatted$new(self$options, 'confusion')
            self$results$add(confusion)
            measure <- jmvcore::Preformatted$new(self$options, 'measures')
            self$results$add(measure)
        },
        .run = function() {
            confusion <- self$results$get('confusion')

            if (length(self$options$dep)   == 0 ||
                length(self$options$indep) == 0)
                return()

            data <- self$data
            options <- self$options
            confMatrixTable <- self$results$confusionMatrix
            measuresTable <- self$results$classMeasures
            classifMetricesTable <- self$results$classifMetrices

            # self$results[['confusionMatrix']]$setVisible(visible = TRUE)

            data[[self$options$dep]] <- jmvcore::toNumeric(data[[self$options$dep]])

            reportingSize <- length(options$reporting)
            lapply(1:reportingSize, function(option) {
                if (!is.null(options$reporting[option])) {
                    op <- options$reporting[option]

                if (!is.na(op))
                    self$results[[op]]$setVisible(visible = TRUE)

                }
             })
            #declare measures
            acc <- bacc <- ce <- logloss <- 0

            #task creation
            task <- TaskClassif$new(id = "cars", backend = data, target = self$options$dep)
            learner <- lrn("classif.rpart",
                             predict_type = 'prob',
                             minsplit = options$minSplit,
                             maxcompete = options$maxCompete,
                             maxsurrogate = options$maxSurrogate,
                             maxdepth = options$maxDepth,
                             cp = options$complecity)

            #testing
            measures <- list()
            if(options$testing == "split" | options$testing == "trainSet") {
                trainSet <- sample(task$nrow)
                testSet <- trainSet
                if (options$testing == "split" & options$testSize != 0) {
                    trainSet <- sample(task$nrow, (1- options$testSize) * task$nrow)
                    testSet <- setdiff(seq_len(task$nrow), trainSet)
                }
                learner$train(task, row_ids = as.character(trainSet))
                prediction <- learner$predict(task, row_ids = as.character(testSet))
                measures <- list(
                    acc = prediction$score(msr("classif.acc")),
                    bacc = prediction$score(msr("classif.bacc")),
                    logloss = prediction$score(msr("classif.logloss")),
                    ce = prediction$score(msr("classif.ce"))
                )
            } else {
                resampling <- rsmp("cv", folds = options$noOfFolds)
                resampling$instantiate(task)
                rr <- resample(task, learner, resampling, store_models = TRUE)
                measures <- list(
                    acc = rr$aggregate(msr("classif.acc")),
                    bacc = rr$aggregate(msr("classif.bacc")),
                    logloss = rr$aggregate(msr("classif.logloss")),
                    ce = rr$aggregate(msr("classif.ce"))
                )
            }

            # prediction <- learner$predict(task, row_ids = testSet)

            #reporting
            if("confusionMatrix" %in% options$reporting) {
                # confusionMatrix <- ifelse(options$testing == 'crossValidation', rr$prediction(1)$confusion, prediction$confusion)

                if(options$testing == 'crossValidation') {
                    confusionMatrix <- rr$prediction(1)$confusion
                } else {
                    confusionMatrix <- prediction$confusion
                }
                classNames <- levels(task$truth())

                lapply(classNames, function (className) {
                    confMatrixTable$addColumn(name = as.character(className), superTitle = 'truth', title = as.character(className), type = 'integer')
                })

                lapply(classNames, function (className) {
                    rowValues <- lapply(confusionMatrix[className, ], function(row) row)
                    rowValues[['class']] <- className
                    confMatrixTable$addRow(rowKey = className, values = rowValues)
                    classifMetricesTable$addRow(rowKey = className, values = list(class = className))
                })
            }
            if ("classifMetrices" %in% options$reporting) {
                self$results[['classMeasures']]$setVisible(visible = TRUE)
                measuresTable$setRow(rowNo=1, values= measures)
            }


            plotData <- mtcars
            image <- self$results$decisionTreePlot
            image$setState(plotData)

        },
        .plot=function(image, ...) {
            plotData <- image$state

            plot <- ggplot(plotData, aes(x=2, y=2.4)) +
                geom_errorbar(aes(ymin=mpg, ymax=cyl, width=.1)) +
                geom_point(aes(x=2, y=3.5)) +
                labs(title=self$options$dep)
            print(plot)
            TRUE
        })
)

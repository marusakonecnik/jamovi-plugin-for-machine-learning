# This file is a generated template, your changes will not be overwritten

classificationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "classificationClass",
    inherit = classificationBase,
    private = list(
        .init = function() {
            preformatted <- jmvcore::Preformatted$new(self$options, 'preformatted')
            self$results$add(preformatted)
        },

        .run = function() {
            preformatted <- self$results$get('preformatted')

            # self$options$reporting <- 'classifMetrices'
            if (length(self$options$dep) == 0 ||
                length(self$options$indep) == 0)
                return()

            data <- as.data.table(self$data)

            task <- TaskClassif$new(id = "task", backend = data[complete.cases(data),], target = self$options$dep)

            learner <- lrn("classif.rpart",
                           predict_type = 'prob',
                           minsplit = self$options$minSplit,
                           maxcompete = self$options$maxCompete,
                           maxsurrogate = self$options$maxSurrogate,
                           maxdepth = self$options$maxDepth,
                           cp = self$options$complecity)

            predictions <- private$.trainModel(task, learner)
        },

        .trainModel = function(task, learner) {
            if (self$options$testing == "split" | self$options$testing == "trainSet") {
                predictions <- private$.trainTestSplit(task, learner)
                private$.setOutput(predictions, predictions, learner$model)
            } else {
                predictions <- private$.crossValidate(task, learner)
                private$.setOutput(predictions$prediction(), predictions, learner$model)
            }

            predictions
        },

        .setOutput = function(predictions, plotData, model) {
            levels <- levels(predictions$truth)
            reporting <- self$options$reporting
            freqPlot <- self$results$predictedFreqPlot
            treePlot <- self$results$decisionTreeModel

            if (any(reporting == 'confusionMatrix')) {
                private$.showConfusionMatrix(predictions$confusion)
            }

            if (any(reporting == "classifMetrices") | any(reporting == "AUC")) {
                scores <- vector()

                if (any(reporting == "classifMetrices")) {
                    scores <- c(scores, "classif.precision", "classif.recall", "classif.fbeta")
                }

                if (any(reporting == "AUC")) {
                    scores <- c(scores, "classif.auc")
                    binaryPredictions <- sapply(levels, function(level) private$.convertToBinary(level, predictions), USE.NAMES = TRUE)

                    self$results$rocCurvePlot$setVisible(TRUE)
                    self$results$rocCurvePlot$setState(binaryPredictions)
                }

                lapply(scores, function(colname) self$results$classificationMetrics$class$columns[[colname]]$setVisible(TRUE))

                private$.showClassificationMetrices(predictions, scores)
            }

            if (self$options$predictedFreq == TRUE) {
                freqPlot$setVisible(TRUE)
                freqPlot$setState(plotData)
            }

            if (self$options$plotDecisionTree == TRUE) {
                treePlot$setVisible(TRUE)
                treePlot$setState(model)
            }
        },

        .crossValidate = function(task, learner) {
            resampling <- rsmp("cv", folds = self$options$noOfFolds)
            resampling$instantiate(task)
            rr <- resample(task, learner, resampling, store_models = TRUE)
            rr
        },

        .trainTestSplit = function(task, learner) {
            trainSet <- sample(task$nrow)
            testSet <- trainSet

            if (self$options$testing == "split") {
                trainSet <- sample(task$nrow, (1 - self$options$testSize) * task$nrow)
                testSet <- setdiff(seq_len(task$nrow), as.numeric(trainSet))
            }

            learner$train(task, row_ids = trainSet)

            prediction <- learner$predict(task, row_ids = testSet)

            prediction
        },

        .showConfusionMatrix = function(confusionMatrix) {
            levels <- colnames(confusionMatrix)[!is.na(colnames(confusionMatrix))] # get levels, removes .na values if any

            self$results$confusion$matrix$setVisible(visible = TRUE)

            lapply(levels, function(level) { # add columns
                self$results$confusion$matrix$addColumn(
                    name = level,
                    superTitle = 'truth',
                    title = level,
                    type = 'integer')
            })

            lapply(levels, function(level) { # add rows
                rowValues <- as.list(confusionMatrix[level,])
                rowValues[['class']] <- level
                self$results$confusion$matrix$addRow(rowKey = level, values = rowValues)
            })

            confusionMatrix
        },

        .showClassificationMetrices = function(predictions, outputScores) {
            classifTable <- self$results$classificationMetrics
            levels <- levels(predictions$truth)

            scores <- private$.calculateScores(predictions, outputScores)
            general <- scores[['general']]
            class <- scores[['class']]

            classifTable$general$setVisible(visible = TRUE)
            classifTable$class$setVisible(visible = TRUE)

            metricesDict <- c(
                classif.acc = 'Accuracy',
                classif.bacc = 'Balanced accuracy',
                classif.ce = 'Error rate',
                classif.recall = 'Macro recall',
                classif.precision = 'Macro precision',
                classif.fbeta = 'Macro F-score',
                classif.auc = 'Macro AUC'
            )

            lapply(names(general), function(name) classifTable$general$addRow(rowKey = name, values = list(metric = metricesDict[name], value = general[[name]])))

            lapply(levels, function(level) {
                class[[level]][['class']] <- level
                self$results$classificationMetrics$class$addRow(rowKey = level, values = class[[level]])
            })
        },

        .convertToBinary = function(class, predictions) {
            transformed <- transform(as.data.table(predictions), truth = ifelse(truth != class, "negative", as.character(truth)))
            transformed <- transform(as.data.table(transformed), response = ifelse(response != class, "negative", as.character(response)))

            probName <- gsub("[[:space:]]", "", paste('prob.', class)) #name of prob column, deletes space
            prob <- as.matrix(data.frame(transformed[[probName]], 1 - transformed[[probName]]))
            colnames(prob) <- c(class, "negative")

            binaryPrediction <- PredictionClassif$new(
                truth = as.factor(transformed$truth),
                response = transformed$response,
                prob = prob,
                row_ids = 1:length(transformed$truth)
            )

            binaryPrediction
        },

        .calculateScores = function(predictions, outputScores) {
            # classScores <- list()
            macros <- numeric(0)
            levels <- levels(predictions$truth)

            binaryPredictions <- sapply(levels, USE.NAMES = TRUE, function(level) private$.convertToBinary(level, predictions))
            classScores <- lapply(levels, function(level) binaryPredictions[[level]]$score(msrs(outputScores)))
            names(classScores) <- levels

            #calculate macro scores
            for (scoreName in outputScores) {
                macros[scoreName] <- mean(sapply(levels, function(class) classScores[[class]][[scoreName]]))
            }

            list(
                general = c(predictions$score(msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))), macros),
                class = classScores
            )
        },

        .rocCurve = function(image, ...) {
            plotData <- image$state

            plotList <- lapply(names(plotData), function(class) {
                mlr3viz::autoplot(plotData[[class]], type = 'roc')
            })

            plot <- ggpubr::ggarrange(plotlist = plotList,
                                      labels = names(plotData),
                                      font.label = list(size = 8, color = "black", face = "bold", family = NULL),
                                      ncol = 2, nrow = round(length(names(plotData)) / 2))

            print(plot)
        },

        .printDecisionTree = function(image, ...) {
            plot <- rpart.plot::rpart.plot(image$state)
            print(plot)
        },

        .frequenciesPlot = function(image, ...) {
            plot <- mlr3viz::autoplot(image$state)
            print(plot)
        })
)

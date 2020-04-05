
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

                    if (length(self$options$dep)   == 0 ||
                        length(self$options$indep) == 0)
                        return()

                    data <- as.data.table(self$data)

                    data[[self$options$dep]] <- jmvcore::toNumeric(data[[self$options$dep]])

                    task <- TaskClassif$new(id = "task", backend = data[complete.cases(data), ], target = self$options$dep)

                    learner <- lrn("classif.rpart",
                                     predict_type = 'prob',
                                     minsplit = self$options$minSplit,
                                     maxcompete = self$options$maxCompete,
                                     maxsurrogate = self$options$maxSurrogate,
                                     maxdepth = self$options$maxDepth,
                                     cp = self$options$complecity)

                    predictions <- private$.trainModel(task,learner, preformatted)
                    private$.setOutput(predictions, preformatted)

                },
                .trainModel = function(task, learner, p) {
                  if(self$options$testing == "split" | self$options$testing == "trainSet") {
                     predictions <- private$.trainTestSplit(task, learner, p)
                  } else {
                     predictions <- private$.crossValidate(task, learner)
                  }
                  predictions
                },
                .setOutput = function(predictions, preformatted) {
                    scores <- c("classif.precision", "classif.recall", "classif.fbeta")
                    if("confusionMatrix" %in% self$options$reporting) {
                        private$.showConfusionMatrix(predictions$confusion, preformatted)
                    }

                    if ("classifMetrices" %in% self$options$reporting) {
                        lapply(scores, function(colname) self$results$classifMetrices$columns[[colname]]$setVisible(TRUE))

                        if("AUC" %in% self$options$reporting) {
                          scores <- c("classif.precision", "classif.recall", "classif.fbeta", "classif.auc")
                          self$results$classifMetrices$columns[["classif.auc"]]$setVisible(TRUE)
                        }
                        private$.showClassificationMetrices(predictions, scores)
                    }

                    if (self$options$plotDecisionTree == TRUE){
                         image <- self$results$decisionTreePlot
                         image$setVisible(visible = TRUE)
                         image$setState(predictions)
                    }
                },
                .crossValidate = function(task, learner) {
                     resampling <- rsmp("cv", folds = self$options$noOfFolds)
                     resampling$instantiate(task)
                     rr <- resample(task, learner, resampling, store_models = TRUE)
                     rr$prediction(1)
                },
                .trainTestSplit = function(task, learner, p) {

                    trainSet <- sample(task$nrow)
                    testSet <- trainSet
                    if (self$options$testing == "split") {
                            trainSet <- sample(task$nrow, (1- self$options$testSize) * task$nrow)
                            testSet <- setdiff(seq_len(task$nrow), as.numeric(trainSet))
                    }
                    learner$train(task, row_ids = trainSet)
                    prediction <- learner$predict(task, row_ids = testSet)

                    prediction
                },
                .showConfusionMatrix = function(confusionMatrix, p) {
                    self$results$confusionMatrix$setVisible(visible = TRUE)
                    levels <- colnames(confusionMatrix)[!is.na(colnames(confusionMatrix))] # get levels, removes .na values if any
                    lapply(levels, function (level) { # add columns
                        self$results$confusionMatrix$addColumn(
                          name = level,
                          superTitle = 'truth',
                          title = level,
                          type = 'integer')
                    })

                    lapply(levels, function (level) { # add rows
                        rowValues <- as.list(confusionMatrix[level, ])
                        rowValues[['class']] <- level
                        self$results$confusionMatrix$addRow(rowKey = level, values = rowValues)
                    })
                    confusionMatrix
                },
                .showClassificationMetrices = function(predictions, outputScores) {
                    self$results$classMeasures$setVisible(visible = TRUE)
                    self$results$classifMetrices$setVisible(visible = TRUE)
                    scores <- private$.calculateScores(predictions,outputScores)

                    metricesDict <- c(
                          classif.acc = 'Accuracy',
                          classif.bacc = 'Balanced accuracy',
                          classif.ce = 'Classif. error',
                          classif.recall = 'Macro recall',
                          classif.precision = 'Macro precision',
                          classif.fbeta = 'Macro fscore',
                          classif.auc = 'Macro AUC'
                    )
                    #fill general table scores
                    general <- scores[['general']]
                    lapply(names(general), function(name) {
                        self$results$classMeasures$addRow(rowKey = name, values = list(metric = metricesDict[name], value = general[[name]]))
                    })
                },
                .convertToBinary = function(class, predictions, p) {
                  transformed <- transform(as.data.table(predictions), truth = ifelse(truth != class, "negative", as.character(truth)))
                  transformed <- transform(as.data.table(transformed), response = ifelse(response != class, "negative", as.character(response)))

                  probName <- gsub("[[:space:]]", "", paste('prob.', class)) #name of prob column, deletes space
                  prob <- as.matrix(data.frame(transformed[[probName]],  1 - transformed[[probName]] ))

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
                   classScores <- list()
                   macros <- numeric(0)
                   #set scores for specific class
                   for (class in colnames(predictions$confusion)) {
                       binaryPrediction <- private$.convertToBinary(class, predictions)
                       scores <- as.list(binaryPrediction$score(msrs(outputScores)))
                       scores[['class']] <- class
                       classScores[[class]] <- scores
                       self$results$classifMetrices$addRow(rowKey = class, values = scores)
                   }

                   #calculate macro scores
                   for (scoreName in outputScores) {
                       macros[scoreName] <- mean(unlist(lapply(levels(predictions$truth), function(class) classScores[[class]][[scoreName]])))
                   }

                  list(
                    general = c(predictions$score(msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))), macros),
                    class = classScores
                  )
                },
                .plot=function(image, ...) {
                    plotData <- image$state
                    plot <- mlr3viz::autoplot(plotData)
                    print(plot)
                    TRUE
                })
        )

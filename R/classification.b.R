
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

                    data <- self$data
                    options <- self$options

                    data[[self$options$dep]] <- jmvcore::toNumeric(data[[self$options$dep]])

                    task <- TaskClassif$new(id = "task", backend = data, target = self$options$dep)

                    learner <- lrn("classif.rpart",
                                     predict_type = 'prob',
                                     minsplit = options$minSplit,
                                     maxcompete = options$maxCompete,
                                     maxsurrogate = options$maxSurrogate,
                                     maxdepth = options$maxDepth,
                                     cp = options$complecity)

                    predictions <- private$.trainModel(task,learner)
                    private$.setOutput(predictions, preformatted)

                    #plot
                    # if (self$options$plotDecisionTree == TRUE)
                    # {
                    #      image <- self$results$decisionTreePlot
                    #      image$setVisible(visible = TRUE)
                    #      image$setState(task)
                    # }
                },
                .trainModel = function(task, learner) {
                  if(self$options$testing == "split") {
                     predictions <- private$.trainTestSplit(task, learner)
                  } else {
                     predictions <- private$.crossValidate(task, learner)
                  }
                  predictions
                },
                .setOutput = function(predictions, preformatted) {
                    if("confusionMatrix" %in% self$options$reporting) {
                        private$.showConfusionMatrix(predictions$confusion, preformatted)
                    }

                    if("AUC" %in% self$options$reporting) {
                        self$results$classifMetrices$addColumn(name = "auc", title = "AUC", type = 'integer')
                    }

                    if ("classifMetrices" %in% self$options$reporting) {
                        private$.showScoringTables(predictions, preformatted)
                    }
                },
                .crossValidate = function(task, learner) {
                     resampling <- rsmp("cv", folds = self$options$noOfFolds)
                     resampling$instantiate(task)
                     rr <- resample(task, learner, resampling, store_models = TRUE)
                     rr$prediction(1)
                },
                .trainTestSplit = function(task, learner) {
                    trainSet <- sample(task$nrow)
                    testSet <- trainSet
                    if (self$options$testing == "split" & self$options$testSize != 0) {
                            trainSet <- sample(task$nrow, (1- self$options$testSize) * task$nrow)
                            testSet <- setdiff(seq_len(task$nrow), trainSet)
                    }

                    learner$train(task, row_ids = as.character(trainSet))
                    prediction <- learner$predict(task, row_ids = as.character(testSet))

                    prediction
                },
                .showConfusionMatrix = function(confusionMatrix, p) {
                    self$results$confusionMatrix$setVisible(visible = TRUE)
                    levels <- colnames(confusionMatrix)

                    sapply(levels, function (level) { # add columns
                        self$results$confusionMatrix$addColumn(
                          name = level,
                          superTitle = 'truth',
                          title = level,
                          type = 'integer')
                    })

                    sapply(levels, function (level) { # add rows
                        rowValues <- as.list(confusionMatrix[level, ])
                        rowValues[['class']] <- level
                        self$results$confusionMatrix$addRow(rowKey = level, values = rowValues)
                    })
                    confusionMatrix
                },
                .calculateScoresPerClass = function(predictions, p) {
                    confusionMatrix <- predictions$confusion
                    levels <- colnames(confusionMatrix)

                    recalls <- numeric(0)
                    precisions <- numeric(0)
                    fscores <- numeric(0)
                    for (class in levels) {
                        predicted <- rowSums(confusionMatrix)
                        actual <- colSums(confusionMatrix)

                        recall <- as.double(confusionMatrix[class, class] / actual[class])
                        recalls[class] <- recall

                        precision <- as.double(confusionMatrix[class, class] / predicted[class])
                        precisions[class] <- precision

                        fscore <-  as.double(2 * ((precision * recall) / (precision + recall)))
                        fscores[class] <- fscore

                        data_table <- as.data.table(predictions)
                        transformed <- transform(data_table, truth = ifelse(truth != class, "negative", as.character(truth))) #replaces values that are not the right class
                        prob_column <- gsub("[[:space:]]", "", paste('prob.', class)) #name of prob column, deletes space

                        if ("AUC" %in% self$options$reporting) {
                            row <- list(
                              class = class,
                              prec = precision,
                              rec = recall,
                              fscore = fscore,
                              auc = auc(as.factor(transformed$truth), data_table[[prob_column]], class)
                            )
                        } else {
                            row <- list(class = class, prec = precision, rec = recall, fscore = fscore)

                        }

                        self$results$classifMetrices$addRow(rowKey = class, values = row)
                        macroMetrics <- c(macPrec = mean(precisions), macRec= mean(recalls), macF1 = mean(fscores))
                    }
                    macroMetrics
                },
                .showScoringTables = function(predictions, p) {
                    self$results[['classMeasures']]$setVisible(visible = TRUE)
                    self$results$classifMetrices$setVisible(visible = TRUE)

                    scores <- private$.calculateScores(predictions,p)

                    lapply(names(scores), function(name) {
                              self$results$classMeasures$addRow(rowKey = name, values = list(metric = name, value = scores[[name]]))
                    })
                },
                .calculateScores = function(predictions, p) {
                   macro <- private$.calculateScoresPerClass(predictions, p)
                   truth <- predictions$truth
                   response <- predictions$response
                   values <- list(
                          "Accuracy" = acc(truth, response),
                          "Balanced accuracy" = bacc(truth, response),
                          "Class. error" = ce(truth, response),
                          "Log loss" = logloss(truth, predictions$prob),
                          "Macro recall" = mean(macro['macRec']),
                          "Macro precision" = mean(macro['macPrec']),
                          "Macro F1" = mean(macro['macF1'])
                    )
                    values
                },
                .plot=function(image, ...) {
                    plotData <- image$state
                    #auto plot not working
                    plot <- autoplot(plotData)
                    print(plot)
                    TRUE
                })
        )

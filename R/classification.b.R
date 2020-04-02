
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
                    confMatrixTable <- self$results$confusionMatrix
                    measuresTable <- self$results$classMeasures
                    classifMetricesTable <- self$results$classifMetrices

                    data[[self$options$dep]] <- jmvcore::toNumeric(data[[self$options$dep]])

                    #set visibility of tables

                    if("AUC" %in% options$reporting) {
                        classifMetricesTable$addColumn(name = "auc", title = "AUC", type = 'integer')
                    }

                    reportingSize <- length(options$reporting)
                    lapply(1:reportingSize, function(option) {
                        if (!is.null(options$reporting[option])) {
                            op <- options$reporting[option]
                            if (!is.na(op)){
                                self$results[[op]]$setVisible(visible = TRUE)
                            }
                        }
                     })

                    #declare measures
                    acc <- bacc <- ce <- logloss <- 0

                    task <- TaskClassif$new(id = "task", backend = data, target = self$options$dep)

                    learner <- lrn("classif.rpart",
                                     predict_type = 'prob',
                                     minsplit = options$minSplit,
                                     maxcompete = options$maxCompete,
                                     maxsurrogate = options$maxSurrogate,
                                     maxdepth = options$maxDepth,
                                     cp = options$complecity)

                    #testing
                    if(options$testing == "split" | options$testing == "trainSet") {
                        trainSet <- sample(task$nrow)
                        testSet <- trainSet
                        if (options$testing == "split" & options$testSize != 0) {
                            trainSet <- sample(task$nrow, (1- options$testSize) * task$nrow)
                            testSet <- setdiff(seq_len(task$nrow), trainSet)
                        }
                        learner$train(task, row_ids = as.character(trainSet))
                        prediction <- learner$predict(task, row_ids = as.character(testSet))

                        predictions <- prediction

                    } else {
                        resampling <- rsmp("cv", folds = options$noOfFolds)
                        resampling$instantiate(task)
                        rr <- resample(task, learner, resampling, store_models = TRUE)

                        predictions <- rr$prediction(1)
                    }
                    #reporting
                    if("confusionMatrix" %in% options$reporting) {
                        classNames <- levels(task$truth())
                        confusionMatrix <- predictions$confusion

                        lapply(classNames, function (className) {
                            confMatrixTable$addColumn(name = as.character(className), superTitle = 'truth', title = as.character(className), type = 'integer')
                        })

                        lapply(classNames, function (className) {
                            rowValues <- lapply(confusionMatrix[className, ], function(row) row)
                            rowValues[['class']] <- className

                            confMatrixTable$addRow(rowKey = className, values = rowValues)
                        })
                    }
                    if ("classifMetrices" %in% options$reporting) {
                        self$results[['classMeasures']]$setVisible(visible = TRUE)
                        for (class in levels(task$truth())) {
                            recalls <- numeric(0)
                            precisions <- numeric(0)
                            fscores <- numeric(0)

                            predicted <- rowSums(confusionMatrix)
                            actual <- colSums(confusionMatrix)

                            recall <- as.double(confusionMatrix[class, class] / actual[class])
                            recalls[class] <- recall
                            precision <- as.double(confusionMatrix[class, class] / predicted[class])
                            precisions[class] <- precision
                            fscore <-  as.double(2 * ((precision * recall) / (precision + recall)))
                            fscores[class] <- fscore
                            data_table <- as.data.table(predictions)
                            transformed <- transform(data_table, truth = ifelse(truth != class, "negative", as.character(truth)))
                            prob_column <- gsub("[[:space:]]", "", paste('prob.', class))

                            if("AUC" %in% options$reporting) {
                                row <- list(
                                  class = class,
                                  prec = precision,
                                  rec = recall,
                                  fscore = fscore,
                                  auc = auc(as.factor(transformed$truth), data_table[[prob_column]], class)
                            )
                            } else {
                                row <- list(class = class, prec = precision,rec = recall,fscore = fscore)
                            }

                            classifMetricesTable$addRow(rowKey = class, values = row)
                        }
                        metriceNames<- c(acc = 'Accuracy', bacc = 'Balanced accuracy', ce ='Classification error', logloss ='Log loss', macPrec ='Macro precision', macRec = 'Macro recall', macF1 = 'Macro F1')
                        values <- list(
                          acc = acc(predictions$truth, predictions$response),
                          bacc = bacc(predictions$truth, predictions$response),
                          ce = ce(predictions$truth, predictions$response),
                          logloss = logloss(predictions$truth, predictions$prob),
                          macRec = mean(recalls),
                          macPrec = mean(precisions),
                          macF1 = mean(fscores)
                        )
                        lapply(names(values), function(name) measuresTable$addRow(rowKey = name, values = list(metric = metriceNames[name], value = values[[name]])))
                    }

                    if(options$plotDecisionTree == TRUE)
                    {
                         image <- self$results$decisionTreePlot
                         image$setVisible(visible = TRUE)
                         image$setState(task)
                    }
                },
                .confusion = function(prediction) {

                },
                .plot=function(image, ...) {
                    plotData <- image$state
                    #auto plot not working
                    plot <- autoplot(plotData)
                    print(plot)
                    TRUE
                })
        )

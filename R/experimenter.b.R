
# This file is a generated template, your changes will not be overwritten

experimenterClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "experimenterClass",
    inherit = experimenterBase,
    private = list(
        .init = function() {
            preformatted <- jmvcore::Preformatted$new(self$options, 'preformatted')
            self$results$add(preformatted)
            private$.initOverallMetrics()
        },
        .run = function() {
            library(mlr3)
            preformatted <- self$results$get('preformatted')

            if (length(self$options$dep) == 0 || length(self$options$indep) == 0)
                return()

            data <- as.data.table(self$data)

            task <- TaskClassif$new(id = "task", backend = data[complete.cases(data), ], target = self$options$dep)

            private$.populateOverallMetrics(task)

        },
        .initOverallMetrics = function() {
            # preformatted <- self$results$get('preformatted')

            classifiers <- self$options$classifiersToUse
            tables <- self$results$overallMetrics

            for (classifier in classifiers) {
                table <- tables$get(key = classifier)

                table$setTitle(classifier)

                table$addColumn(name = 'metric', title = 'Metric', type = 'text')
                table$addColumn(name = 'value', title = 'Value', type = 'number')
            }
        },

        .populateOverallMetrics = function(task) {
            preformatted <- self$results$get('preformatted')
            classifiers <- self$options$classifiersToUse


            if(length(classifiers) == 0)
                return();

            tables <- self$results$overallMetrics

            metricesDict <- c(
                classif.acc = 'Accuracy',
                classif.bacc = 'Balanced accuracy',
                classif.ce = 'Error rate',
                classif.recall = 'Macro recall',
                classif.precision = 'Macro precision',
                classif.fbeta = 'Macro F-score',
                classif.auc = 'Macro AUC'
            )

            scoreNames <- c("classif.precision", "classif.recall", "classif.fbeta")

            for (classifier in classifiers) {
                table <- tables$get(key = classifier)

                settings <- regmatches(classifier, gregexpr("\\(.+?\\)", classifier))
                 preformatted$content <- settings

                if(!identical(settings, character(0))) {
                    settings <- private$.getOptions(substr(settings, 2, nchar(settings)-1))
                }

                learner <- private$.initLearner(classifier, settings)

                predictions <- private$.trainModel(task, learner)

                scores <- private$.calculateScores(predictions, scoreNames)
                general <- scores[['general']]

                lapply(names(general), function(scoreName) table$addRow(rowKey = scoreName, values = list(metric = metricesDict[scoreName], value = general[[scoreName]])))
            }
        },

        .getOptions = function(settings) {

          settings <- gsub("[[:space:]]", "", settings)
          splitted <- strsplit(settings, ',')[[1]]
          options <- list()

          for (option in splitted) {
            splittedOption <- strsplit(option, '=')[[1]]
            optionName <- splittedOption[1]
            optionValue <- splittedOption[2]

            if(!is.na(as.numeric((splittedOption[2])))) {
                optionValue <- as.numeric(optionValue)
            }

            options[[optionName]] <- optionValue
          }

          options
        },

        .initLearner = function (classifier, options) {
            preformatted <- self$results$get('preformatted')

          if(grepl("KNN", classifier, fixed = TRUE)) {
              learner <- lrn("classif.kknn", predict_type = 'prob')
          } else if(grepl("Decision tree", classifier, fixed = TRUE)) {
              learner <- lrn("classif.rpart", predict_type = 'prob')
          } else if(grepl("Random forest", classifier, fixed = TRUE)) {
             learner <- lrn("classif.ranger", predict_type = 'prob')
          } else if(grepl("Naive bayes", classifier, fixed = TRUE)) {
              learner <- lrn("classif.naive_bayes", predict_type = 'prob')
          } else if(grepl("Logistic regression", classifier, fixed = TRUE)) {
              learner <- lrn("classif.log_reg", predict_type = 'prob')
          }

         if(!is.na(options)) {
            learner$param_set$values <- options
              preformatted$content <- learner$param_set

         }
         return(learner)
        },

        .trainModel = function(task, learner) {
            if (self$options$testing == "split" | self$options$testing == "trainSet") {
                predictions <- private$.trainTestSplit(task, learner)
                # private$.setOutput(predictions, predictions, learner$model)
            } else {
                predictions <- private$.crossValidate(task, learner)
                # private$.setOutput(predictions$prediction(), predictions, learner$model)
            }

            predictions
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

        .calculateScores = function(predictions, outputScores) {
            macros <- numeric(0)
            levels <- levels(predictions$truth)

            binaryPredictions <- sapply(levels, USE.NAMES = TRUE, function(level) private$.convertToBinary(level, predictions))

            classScores <- lapply(levels, function(level) {
                prob <- as.data.table(binaryPredictions[[level]])[[paste('prob', level, sep = '.')]]

                c(
                    'classif.recall' = recall(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                    'classif.precision' = precision(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                    'classif.fbeta' = fbeta(binaryPredictions[[level]]$truth, binaryPredictions[[level]]$response, positive = level),
                    'classif.auc' = auc(binaryPredictions[[level]]$truth, prob = prob, positive = level)
                )
            })

            names(classScores) <- levels
            classScores <- lapply(classScores, round , 3)

            #calculate macro scores
            for (scoreName in outputScores) {
                macros[scoreName] <- mean(sapply(levels, function(class) classScores[[class]][scoreName]))
            }

            list(
                general = c(predictions$score(msrs(c('classif.acc', 'classif.ce', 'classif.bacc'))), macros),
                class = classScores
            )
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
        }
    )
)

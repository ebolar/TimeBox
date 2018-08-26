# R Project Template
cat("Time Box - Planner and task tracker\n")
cat("===================================\n")

# ===================================================================
# 0. Environment setup

     # Cleanup
     rm(list = ls())

     # Multicore parallelisation
     # -------------------------
     #library(doMC)
     #registerDoMC(cores=3)


     # Workstation farm parallelisation
     # --------------------------------
     #library(foreach)
     #library(doParallel)
     #
     # Setup the cluster
     #makeCluster(3) -> myclust
     #registerDoParallel(myclust)
     #
     # export any shared variables
     #... create the variable
     #clusterExport(myclust, variable)
     #... algorithm goes here
     #stopCluster(myclust)
     #
     
     Sys.getenv("COLUMNS") -> cols
     132 -> cols[cols<40]
     options(width=as.integer(cols))
     rm(cols)

     # helpers
     GetCSV <- function (prompt="Enter filename: ") {
        # Prompts for a CSV file then reads it into a data frame
	# would be good to be able to pass in the directory to read from
	cat("\n\nAvailable Files\n---------------------------------------\n")
        print(list.files(pattern="*.csv"))
        if (interactive()) {
          # Interactive R
          readline(prompt=prompt) -> filename
        } else {
          # Rscript
          cat(prompt)
          readLines("stdin", n=1) -> filename
        }
   
        # load the CSV file from the local directory
        read.csv(filename, header=TRUE, stringsAsFactors=FALSE)
     }

     GetInput <- function (prompt="Come come, elucidate your thoughts: ") 
     {
        if (interactive()) {
          # Interactive R
          readline(prompt=prompt) -> x
        } else {
          # Rscript
          cat(prompt)
          readLines("stdin", n=1) -> x
        }
        x
     }

     TB <- "TimeBox.R"

     rerun <- function ()
     {
	     source(TB)
     }

     SaveTasks <- function (Selection="*")
     {
        write.csv(Tasks[grep(Selection,Tasks$Name),], file="Task List.csv", row.names=FALSE)
        #write.csv(Transactions[grep(Selection,Transactions$DateTime),], file=paste("ATM",Selection,"Transactions.csv",sep="-"), row.names=FALSE)
     }

     LoadTasks <- function ()
     {
	# First check that the file exists and produce an error otherwise
        read.csv("Task List.csv", header=TRUE, stringsAsFactors=FALSE) 
        #write.csv(Transactions[grep(Selection,Transactions$DateTime),], file=paste("ATM",Selection,"Transactions.csv",sep="-"), row.names=FALSE)
     }

# ===================================================================
# 1. Prepare Problem
     cat("\n\nSetup\n=======================================\n")
# a) Load libraries

     #library(caret)
     #library(corrplot)
     #library(psych)

# b) Load dataset

     # Get the task list
     read.csv("Task List.csv", header=TRUE, stringsAsFactors=FALSE) -> Tasks

     # ===========================================================================================
     # Data preparation - depends on the source of the data
     # ===========================================================================================

     # Time to go home
     SaveTasks

#
## ===================================================================
## 2. Summarize Data
#cat("\n\nSummarize data\n---------------------------------------\n")
## a) Descriptive statistics
## b) Data visualizations
#
#     # a few useful transformations
#     trainingset[,1:4] -> x
#     trainingset[,5] -> y
#     
#     preProcess(trainingset, "scale") -> params
#     predict(params, trainingset) -> ts.scale
#     preProcess(trainingset, "range") -> params
#     predict(params, trainingset) -> ts.range
#     preProcess(trainingset, "center") -> params
#     predict(params, trainingset) -> ts.center
#     
#     # univariate visualisations
#     # boxplot for each attribute on one image
#     par(mfrow=c(1,4))
#     for(i in 1:4) {
#       boxplot(x[,i], main=names(trainingset)[i])
#     }
#
#     # multivariate visualisations
#     corrplot(cor(trainingset[,1:4]), "pie")
#     featurePlot(x, y, "ellipse")
#     featurePlot(x, y, "strip", jitter = TRUE)
#     featurePlot(x, y, "box")
#     featurePlot(x, y, "pairs")
#
#
## ===================================================================
## 3. Prepare Data
#cat("\n\nPrepare data\n---------------------------------------\n")
## a) Data Cleaning
## b) Feature Selection
## c) Data Transforms
#
## ===================================================================
## 4. Evaluate Algorithms
#cat("\n\nEvaluate Algorithms\n---------------------------------------\n")
## a) Test options and evaluation metric
##    Run algorithms using 10-fold cross-validation
#     #trainControl <- trainControl(method="cv", number=10)
#     trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
#     metric <- "Accuracy"

## b) Spot Check Algorithms
#     # LDA
#     cat("Linear Discriminant Analysis ... ")
#     set.seed(7)
#     fit.lda <- train(Species~., data=trainingset, method="lda", metric=metric,
#     trControl=trainControl)
#     # CART
#     # Did not help as a classifier for this analysis - I think I need to sort and rank scenarios - a little different
#     cat("Classification and regression trees ... ")
#     set.seed(7)
#     fit.cart <- train(IsRetract~., data=Transactions.Select, method="rpart", metric=metric,
#     trControl=trainControl)
#     # KNN
#     cat("K-nearest nodes ... ")
#     set.seed(7)
#     fit.knn <- train(Species~., data=trainingset, method="knn", metric=metric,
#     trControl=trainControl)
#     # SVM
#     cat("Support Vector Machine with Radial Basis Function kernel ... ")
#     set.seed(7)
#     fit.svm <- train(Species~., data=trainingset, method="svmRadial", metric=metric,
#     trControl=trainControl)
#     # Random Forest
#     cat("Random Forest ... ")
#     set.seed(7)
#     fit.rf <- train(Species~., data=trainingset, method="rf", metric=metric, trControl=trainControl)
#
#     cat("done\n\n")
#
     

## c) Compare Algorithms
#     # summarize accuracy of models
#     
#     cat("Summarise model accuracy\n------------------------\n")
#     results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
#     summary(results)
#     # compare accuracy of models
#     dotplot(results)
#     
#     # check for correlation between results
#     # parallelplot(results)
#     # corrplot()
#     
#     cat("\nBest result is LDA if using repeated cross validation.\nOther models are correlated and not far behind.\n")
#     
## ===================================================================
## 5. Improve Accuracy
#cat("\n\nImprove Accuracy\n---------------------------------------\n")
## a) Algorithm Tuning
## b) Ensembles
#
## ===================================================================
## 6. Finalize Model
#cat("\n\nFinalize model\n---------------------------------------\n")
## a) Predictions on validation dataset
#
#     cat("Predictions on the validation dataset.\n\n")
#     predictions <- predict(fit.lda, validationset)
#     confusionMatrix(predictions, validationset$Species)
#
#     # gives 100% accuracy on the validation dataset.  98% accuracy on the original dataset.
#
#     cat("Predictions on the original dataset.\n\n")
#     predictions <- predict(fit.lda, iris)
#     confusionMatrix(predictions, iris$Species)
#
## b) Create standalone model on entire training dataset
## c) Save model for later use
#
#     saveRDS(fit.lda, "Project_Iris.rds")

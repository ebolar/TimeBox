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

     library(stringr)

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

     commandArgs(TRUE) -> argv
     
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
     
     str_field <- function (string, width=1, side="right") {
	if(nchar(string) < width) 
	   nchar(string) -> l
	else
	   width -> l
	
	return(str_pad(substr(string,1,l), width=width, side=side))
     }
     
     LoadConfig <- function ()
     {
	# First check that the file exists and produce an error otherwise
	read.csv("Config.csv", header=TRUE, stringsAsFactors=TRUE, na.strings="") 
     }

     SaveConfig <- function ()
     {
	# First check that the file exists and produce an error otherwise
	write.csv(Config, file="Config.csv", row.names=FALSE) 
     }

     SaveTasks <- function (Selection="*")
     {
	write.csv(Tasks[grep(Selection,Tasks$Task),], file="Task List.csv", row.names=FALSE)
     }

     LoadTasks <- function ()
     {
	# First check that the file exists and produce an error otherwise
	read.csv("Task List.csv", header=TRUE, stringsAsFactors=FALSE) -> T 
	
	for (col in 1:ncol(T))
	   "" -> T[is.na(T[,col]),col]
	   
	return(T)
     }

     IndexTasks <- function (T) 
     {
	class(T) -> C
	if (C == "data.frame") 
	{
	   for (row in 1:nrow(T))
	     row -> T$ID[row]
	} else if (C == "character")
	{
	   NR=NROW(T)
	   for (row in 1:NR)
	     row -> T[row]
	} else print("Error: Unknown Task type")
	return (T)
     }
     
     CaptureTask <- function(Project="")
     {
	GetInput(prompt=sprintf("Project [%s] : ", Project)) -> Project
        # Select & Validate the project

	# Variable names for the following groups based on the project type
	GetInput(prompt="Group1      : ") -> Group1
	GetInput(prompt="Group2      : ") -> Group2
	GetInput(prompt="Group3      : ") -> Group3
	
	# What needs to be done
	GetInput(prompt="Task        : ") -> Task
	GetInput(prompt="Description : ") -> Description

	# Priority could be derived.  Some people may want to enter it.
	GetInput(prompt="Urgency     : ") -> Urgency
	GetInput(prompt="Importance  : ") -> Importance
	GetInput(prompt="Effort      : ") -> Effort
	     
	# Can we derive the status?  How do we ensure that only the valid status types are entered.
	GetInput(prompt="Status      : ") -> Status
	GetInput(prompt="Who         : ") -> Who
	GetInput(prompt="Due Date    : ") -> DateDue

	return(
	   data.frame(
	      ID="",
	      Project=Project,
	      Group1=Group1,
	      Group2=Group2,
	      Group3=Group3,
	      Task=Task,
	      Description=Description,
	      Priority="",
	      Urgency=Urgency,
	      Importance=Importance,
	      Effort=Effort,
	      Status=Status,
	      Who=Who,
	      DateAssigned="",
	      DateStarted="",
	      DateCompleted="",
	      DateDue=DateDue,
	      stringsAsFactors=FALSE
	      )
	  )
     }

     PrintTasks <- function (Project="*", Group="*", Status=c("InProgress","Next","Completed","Defect"), show="Summary", paginate=TRUE) 
     {
	# Put a box around this
	# Responsive configuration based on page width
	# Just make it work!!!
	
	# cat(sprintf("Project %s, Group %s, Status (%s), show=%s, Paginate=%s\n", toString(Project), toString(Group), toString(Status), toString(show), toString(paginate)))
	# ID,Project,Group,Task,Status
	# Implementation,Activity,Requires,
	# Priority,Urgency,Importance,
	# Effort,Who,DateAssigned,DateStarted,DateCompleted,DateDue
	
	if (paginate) 
	    sink(file=".Tasks.PrintTasks", type="output")

	sprintf("Project/Group: %s/%s", Project, Group) -> Selection
	# cat(sprintf("1.Listing tasks for %s", Selection))
	
	# Filter out the entries we want
	#   by Project and group
	T1 <- Tasks[grep(Project, Tasks$Project),]
	T1 <- T1[grep(Group, sprintf("%s-%s-%s", T1$Group1, T1$Group2, T1$Group3)),]
	  
	#   by Status
	data.frame() -> T
	for (s in Status)
	    rbind(T, T1[grep(s, T1$Status),]) -> T

	if (nrow(T) > 0) 
	    for (row in 1:nrow(T)) {
		# cat(sprintf("%s-%s-%s-%s\n", Project, row, T$Project[row], nrow(T)))

	    	if (Project != T$Project[row]) {
	    	    Project = T$Project[row]
	    	    cat(sprintf("Project: %s\n", T$Project[row]))
		    # print(summary(as.factor(T$Group1)))
		    # cat("\n")
	    	}

	    	if (Group != sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])) {
		    Group = sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])
	    	    cat(sprintf("  %s-%s-%s\n", T$Group1[row], T$Group2[row], T$Group3[row]))
	    	}

	    	if (show == "Full") {
		    cat(sprintf("    %s %s\n", str_pad(paste("[",T$ID[row],"]", sep=""),7), T$Task[row]))
		    cat(sprintf("            \"%s\"\n", T$Description[row]))
		    cat(sprintf("            Status [%s]", T$Status[row]))
		    cat(sprintf(", Priority [%s/%s]", T$Urgency[row], T$Importance[row]))
		    cat(sprintf(", Assigned to: %s. Due: %s\n", T$Who[row], T$DateDue[row]))
		    # cat(sprintf("    [%s] %s - %s\n", T$ID[row], T$Task[row], T$Description[row]))
		    # cat(sprintf("    \tStatus [%s]", T$Status[row]))
		    # cat(sprintf(", Priority [%s/%s]", T$Urgency[row], T$Importance[row]))
		    # cat(sprintf(", Assigned to: %s. Due: %s\n", T$Who[row], T$DateDue[row]))
	    	} else {
		    cat(sprintf("    %s %s %s %s\n", 
		    str_pad(paste("[",T$ID[row],"]", sep=""),7), 
		    str_field(T$Task[row],40, side="right"), 
		    paste("[",str_field(T$Status[row], width=8, side="both"),"]", sep=""), 
		    paste("\"",str_field(T$Description[row],60, side="right"),"\"", sep="")))
	    	}
	    }

	if (paginate) {
	   sink()
	   #file.show(file=".Tasks", delete.file=TRUE, title=sprintf("Listing tasks for %s", Selection))
	   file.show(file=".Tasks.PrintTasks", title=sprintf("Listing tasks for %s", Selection))
	}
     }

     SumariseTasks <- function (Project="*", Group="*")
     {
	# ID,Project,Group,Task,
	# Implementation,Activity,Requires,
	# Priority,Urgency,Importance,
	# Effort,Who,DateAssigned,DateStarted,DateCompleted,DateDue
	
	T <- Tasks[grep(Project, Tasks$Project),]
	T <- T[grep(Group, sprintf("%s-%s-%s", T$Group1, T$Group2, T$Group3)),]
		  
	for (row in 1:nrow(T)) {
	    if (Project != T$Project[row]) {
	    	Project = T$Project[row]
	    	cat(sprintf("\n===============\n"))
	    	cat(sprintf("Project: %s\n", T$Project[row]))
		P <- T[grep(Project, T$Project), ]

		# Ideally needs a matrix - count of rows by Group and by status
	    	cat("---------------\n")
		cat("By group:\n")
	    	print(summary(as.factor(P$Group1)))

	    	cat("---------------\n")
	    	cat("By status:\n")
	    	print(summary(as.factor(P$Status)))	
	    }
	}
	
	cat("\n===============\n")
     }
     
     ToDo <- function() {
	cat("Backlog\n====================\n")
	PrintTasks(Status="Next", paginate=FALSE)
	PrintTasks(Status="Defect", paginate=FALSE)
	cat("\nToDo\n====================\n")
	PrintTasks(Status="InProgress", show="Full", paginate=FALSE)
	cat("\nCompleted\n====================\n")
	PrintTasks(Status="Complete", paginate=FALSE)
     }

     Usage <- function() 
     {
	cat("Time Box\n")
        cat("---------------------------------------------\n\n")
	cat(" Time management in R\n")
	cat("\n TimeBox.R <function>\n")
	cat("\n Where function is:\n")
	cat("   ToDo       - print a to-do list\n")
	cat("   Testing123 - Run the test suite\n")
	cat("   NewTask    - Add to the task list\n")
	cat("   Status     - Summary status\n")
	cat("   ListTasks  - Print out a list of tasks\n")
	cat("\n")
     }

# ===================================================================
# 1. Prepare Problem
#     cat("\n\nSetup\n=======================================\n")
# a) Load libraries

     #library(caret)
     #library(corrplot)
     #library(psych)

     LoadConfig() -> Config

# b) Load dataset

     # Get the task list
     LoadTasks() -> Tasks
     for (row in 1:nrow(Tasks))
     row -> Tasks$ID[row]
     # read.csv("ToDo.csv", header=TRUE, stringsAsFactors=FALSE) -> ToDo

     # ===========================================================================================
     # Data preparation - depends on the source of the data
     # ===========================================================================================
     
     if (is.na(argv[1]))
     {
	Usage()
     } else if (argv[1] == "ToDo")
     {
	ToDo()
     } else if (argv[1] == "Testing123")
     {
        # Testing 123
        cat("---------------------------------------------\nSumariseTasks()\n\n")
        SumariseTasks()

        cat("---------------------------------------------\nPrintTasks(show=Full)\n\n")
        GetInput(prompt="Next") -> BitBucket
        PrintTasks(show="Full")

        cat("---------------------------------------------\nPrintTasks(Group=R functions)\n\n")
        GetInput(prompt="Next") -> BitBucket
        PrintTasks(Group="R functions", Status="*")

        cat("---------------------------------------------\nPrintTasks(Project=unknown, paginate=FALSE)\n\n")
        GetInput(prompt="Next") -> BitBucket
        PrintTasks(Project="unknown", Status="*", paginate=FALSE)

        cat("---------------------------------------------\nToDo list\n\n")
        GetInput(prompt="Next") -> BitBucket
        ToDo()
     } else if (argv[1] == "Status")
     {
	SumariseTasks()
     } else if (argv[1] == "NewTask") 
     {
	CaptureTask() -> T
        if (nrow(T) > 0)
           rbind(T, Tasks) -> Tasks
     } else if (argv[1] == "ListTasks") 
     {
	# Add flags
	#   -project <projectName>
	#   -group <groupName>
	#   -status <listOfStatuses>
	#   -show <Full|Summary>
	#   -paginate <TRUE|FALSE>
	
	PrintTasks(Status="*")
     } else Usage()

     # Time to go home
     SaveTasks()

     SaveConfig()

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

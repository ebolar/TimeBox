# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# Time management in R for busy people, and a good excuse for me to 
# learn more about R.
# 
# ===================================================================
# 

cat("Time Box - Planner and task tracker\n")
cat("===================================\n")
cat("Time management in R for busy people\n")
cat("Making it easy to track shit and get shit done\n")

# ===================================================================
# 0. Environment setup

# Cleanup
FALSE -> testing
rm(list=grep("testing", ls(), value = TRUE, invert = TRUE))

source("TimeBoxData.R")
source("HelperFunctions.R")

TRUE -> testing
source("TimeBoxUI.R")

commandArgs(TRUE) -> argv
     
TB <- "TimeBox.R"

rerun <- function ()
{
  source(TB)
}

# Parse the command line arguments
# ...

Configuration.Init() -> E
Datasource.Init(E)
UI.Init(E)
Datasource.Close(E)
Configuration.Close("E")

stop(call. = FALSE, "Thus far and no further!")


# ===========================================================================================
# Command line version
# ===========================================================================================
# needs a little work
# 
# timebox <function> <flags>
# 
# functions:
#   - ToDo: Print the ToDo list
#   - Testing123: Run the test harness
#   - 
#
if (!is.na(argv[1]))
  {
    if (argv[1] == "ToDo")
      {
        # ...
      } 
	  else if (argv[1] == "Testing123")
      {
        # Run the test cases
	       # ...
      } 
    else if (argv[1] == "Status")
      {
        SummariseTasks()
      }
    else if (argv[1] == "NewTask") 
      {
        CaptureTask() -> T
        if (nrow(T) > 0)
          rbind(T, Tasks) -> Tasks
      }
    else if (argv[1] == "ListTasks") 
      {
        # Add flags
        #   -project <projectName>
        #   -group <groupName>
        #   -status <listOfStatuses>
        #   -show <Full|Summary>
        #   -paginate <TRUE|FALSE>
      
        ListTasks(Status="*")
      } 
    else 
      {
        cat("\n TimeBox.R <function>\n")
        cat("\n Where function is:\n")
        cat("   ListTasks  - Print out a list of tasks\n")
        cat("   NewTask    - Add to the task list\n")
        cat("   Status     - Summary status\n")
        cat("   Testing123 - Run the test suite\n")
        cat("   ToDo       - print a to-do list\n")
        cat("\n")
      }
  }

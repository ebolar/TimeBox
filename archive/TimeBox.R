# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# Time management in R for busy people, and a good excuse for me to 
# learn moue about R.
# 
# :) (Am I wasting time here?) :)
#   (What am I doing this for? There are plenty of simpler ways to manage my time.)
# 
# The set of routines are a bit of a dogs breakfast.  
# Can I clean these up, 
#   - make them more functional by removing side effects.  
#   - add a layer for dealing with the environment.
# ===================================================================
# function(parameter="default"[option1|option2|...]) {environment} -> result {environment}
# 
# 1. General  
# 1.1 Environment setup
#  o argv
#  o rerun()
#  o libraries(stringr, ...)
#  o Test harness
#
# 1.1 Helper functions
#  o GetCSV(prompt) {CSVfile} -> data.frame(CSV) {CSVfile}
#  o GetInput(prompt, default) -> value
#  o str_field(string, width=1, side="right"[right|left|both]) -> StringField
# 
# 1.2 R Package
# ===================================================================
# 2.   Time Box
# 2.1   Task repository - Sources (& Sinks)
#  o LoadConfig() {ConfigurationFile} -> {ConfigurationFile x Configuration}
#  . SaveConfig() {Configuration} -> {ConfigurationFile x Configuration}
#  o LoadTasks(Source) -> {Source x Tasks}
#  o SaveTasks(Selection, Source) {Tasks} -> {Source x Tasks}
#  . SaveTasks(Source, Tasks) -> {Source x Tasks}
#  o EmptyTasks() -> Tasks "Dataframe is empty.  Data types for all fields are configured"
#  o IndexTasks(Tasks) -> IndexedTasks "Internal index numbers reset"
#  . Map2Tasks(Source, Fields) {Configuration} -> MappedTasks {Configuration} "Maps fields onto default Task columns"
# 
# 2.1.1   Spreadsheet - Source<CSVfile, ExcelFile, ...>
# 2.1.2   Calendar - Sink<Google, Outlook, ...>
# 2.1.3   Outlook - Source<task, email{folder, flags, importance, etc}>
# 2.1.4   Productivity tools - Source<Jira, Confluence {tasks}, Microsoft Planner, ...>
# 2.1.5   Database - Source<Oracle, ProgreSQL, ...>
# 2.1.6   SMTP/iMAP - Source<email address> "Listen for emails to an address, send updates to a mailing list"
# 
# 2.2   Function library
# 2.2.1   Tasks (Basic functions)
#  o SelectTasks(Selection) {Tasks} -> SelectedTasks {Tasks}
#  o SortTasks(Tasks, orderBy="Tasks"[any field]) -> Index
#  o UpdateTasks(Selection, column, value) {Tasks} -> {Tasks}
#  o NewTask(Project) {Tasks} -> {Tasks}
#  
# 2.2.2   Configuration
#  PROJECT=list(
#    Organisation=""
#    Project=""
#    Groups=list("a", "b", "c")
#    Status=list("x", "y", "z")
#  )  
#  SOURCE=list(
#    Type="CSV"
#    FieldMapping=list()
#    CSVfile="TimeBox.csv"
#  )
#  UI
#  
# 2.2.3   Projects
# 
# 2.3   User Interfaces
# 2.3.1   R functions
#  . PrintColumn(Column) -> FormattedColumn	
#  . CalculatePriority(Task) -> Priority
# 2.3.2   R Text based menu
#  . Simple menu
# 2.3.3   Rscript Text based menu
#  o Simple command line parser
#  o DOS batch files
#  . Linux bash scripts
# 2.3.4   R Shiny
# 
# 2.4   Productivity toolkit
# 2.4.1   Task manipulation
#  o CaptureTask(UI, Project) -> Task
#  o PrintTasks(UI, Tasks) -> 
#  o ListTasks(UI, Selection, show="Summary"[List|Summary|Full], paginate=TRUE[TRUE|FALSE]) {Tasks} -> {Tasks}
#  o SummariseTasks(UI, Selection, paginate=TRUE[TRUE|FALSE]) {Tasks} -> {Tasks}
#  o ToDo(UI, Selection, paginate=TRUE[TRUE|FALSE]) {Tasks} -> {Tasks}
#  . ChangeStatus(UI, Selection, NewStatus="") {Tasks} -> {Tasks}
#  . ChangeOwner(UI, Selection, NewOwner="") {Tasks} -> {Tasks}
#  . ChangeOwner(UI, Selection, NewOwner="") {Tasks} -> {Tasks}
#  . ChangeOwner(UI, Selection, NewOwner="") {Tasks} -> {Tasks}
#  . Change(UI, Selection, column, value) {Tasks} -> {Tasks}
#  . Change(UI, Tasks, column, value) -> UpdatedTasks "Not sure this is useable"
# 
# 2.4.1   Dashboards and summaries
# 2.4.2   ToDo list - what do I need to do today?
# 2.4.3   End of week report - how much progress did we make?
# 
# 
cat("Time Box - Planner and task tracker\n")
cat("===================================\n")
cat("Time management in R for busy people\n")

# ===================================================================
# 0. Environment setup

     # Cleanup
     rm(list = ls())

     library(stringr)

     # minimum 80 columns
     Sys.getenv("COLUMNS") -> cols
     80 -> cols[cols<80]
     options(width=as.integer(cols))
     rm(cols)

     commandArgs(TRUE) -> argv
     
     TB <- "TimeBox.R"

     rerun <- function (save=TRUE)
     {
	     if (save) SaveTasks()

	     source(TB)
     }
     

     # ===================================================================
     # 1. Functions 
     # 1.1 General helpers
     # 1.2 R Package

     # List CSVs and load one
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

     # Generalised input grabber
     GetInput <- function (prompt="Come come, elucidate your thoughts: ", default="") 
     {
	if (interactive()) {
	  # Interactive R
	  readline(prompt=prompt) -> x
	} else {
	  # Rscript
	  cat(prompt)
	  readLines("stdin", n=1) -> x
	}

	if (x == "")
	default
	else
	x
     }

     # Display as a fixed width field, truncating or padding as required.
     # Requires library stringr
     str_field <- function (string, width=1, side="right") {
	Field = data.frame()
        print(string)
	 
	XXX busted XXX

	for (row in string) {
	    string[row] -> s
	    print(string[row])
	    if (is.na(s)) "<NA>" -> s

	    if(nchar(s) < width) 
	        nchar(s) -> l
	    else
	        width -> l
	
	    rbind(Field, str_pad(substr(s,1,l), width=width, side=side))
	}
	return(str_pad(substr(s,1,l), width=width, side=side))
     }
     
     # Wrap(String, Width) -> String (vector)

     # ===================================================================
     # 2.   Time Box
     # 2.1   Task repository
     # ===================================================================
     # 2.1.1   CSV
     # ===================================================================
     #
     # To be rewritten
     # - Load and save are generic Task factories
     # - Operate on a polymorphic task Source class for CSV, XLS, Outlook, ...
     # - Sources are defined in the configuration (a list)
     # - Tasks include internal and externally defined ID's
     # - Source configuration includes defaults for Organisation and Project
     # - Can push reminders to your calendar
     #

     #  . LoadConfig() {ConfigurationFile} -> {ConfigurationFile x Configuration}
     #  o LoadConfig() {StaticConfiguration} -> {Configuration}
     LoadConfig <- function ()
     {
	# A temporary measure until I can store and retrieve from a file
	# I need some routines to search and get values from this list
	# Would be nice to specify the Statuses for each Project rather than globally - maybe
	list(
	    Projects=list(
	  	list(
		    Organisation="Home",
		    Project="TimeBox",
		    Groups=list(Group1="Feature", Group2="Function", Group3="Implementation")
	        ),
	        list(
		    Organisation="Home",
		    Project="Testing123"
	        )
	    ),
	    # For now only one source
	    Source=list(
		    CSVfile="TimeBox.csv",
		    Mapping=NA
	    ),
	    Status=c("Idea", "Backlog", "Next", "Waiting", "InProgress", "Weekly", "Completed", NA)
	)
     }

     SaveConfig <- function ()
     {
	# First check that the file exists and produce an error otherwise
     }

     SaveTasks <- function (Organisation="*", Project="*", Group="*", Task="*", CSVfile=Config$Source$CSVfile)
     {
	write.csv(SelectTasks(Organisation, Project, Group, Task), file=CSVfile, row.names=FALSE)
     }

     LoadTasks <- function (CSVfile=Config$Source$CSVfile)
     {
	tryCatch(
	    {
	        read.csv(CSVfile, header=TRUE, stringsAsFactors=FALSE) -> T

	        if (nrow(T) >0)
	            for (col in 1:ncol(T))
	                "" -> T[is.na(T[,col]),col]

		# Map and validate the columns read in
		# Pull this from the Project configuration list and standard default values
	        T$Urgency <- factor(T$Urgency, levels=c("High","Low",NA))
	        T$Importance <- factor(T$Importance, levels=c("High","Low",NA))
		T[T$Urgency=="High" & T$Importance=="High" & !(is.na(T$Urgency)) & !(is.na(T$Importance)), "Priority"] <- "To do"
		T[T$Urgency=="Low" & T$Importance=="High" & !(is.na(T$Urgency)) & !(is.na(T$Importance)), "Priority"] <- "Plan/Schedule"
		T[T$Urgency=="High" & T$Importance=="Low" & !(is.na(T$Urgency)) & !(is.na(T$Importance)), "Priority"] <- "Delegate"
		T[T$Urgency=="Low" & T$Importance=="Low" & !(is.na(T$Urgency)) & !(is.na(T$Importance)), "Priority"] <- "Recreation"
	        T$Status <- ordered(T$Status, levels=c(Config$Status))
	        IndexTasks(T) 
	    },
	    error = function(e) {
	        if (grepl("no lines available in input", e$message))
		    return(EmptyTasks())
	        else {
		    # if debug str(e)
	    	    stop(sprintf("LoadTasks(\"%s\") : %s", CSVfile, e))
		}
	    },
	    warning = function(w) {
		if (grepl("No such file or directory", w$message))
		    return(EmptyTasks())
		else {
		    # if debug str(w)
	    	    warning(sprintf("LoadTasks(\"%s\") : %s", CSVfile, w))
		    return(EmptyTasks())
		}
	    } 
	) 

     }

     IndexTasks <- function (T) 
     {
	class(T) -> C
	if (C == "data.frame") 
	{
	    if (nrow(T) > 0)
	        for (row in 1:nrow(T))
	            row -> T$ID[row]
	} 
	# Interesting but do I really need this?
	else if (C == "character")
	{
	   if (NROW(T) > 0)
	       for (row in 1:NROW(T))
	           row -> T[row]
	} else print("Error: Unknown Task type")
	return (T)
     }

     # Bootstrap a task list
     EmptyTasks <- function()
	return(
	   data.frame(
	      ID=character(),
	      Organisation=character(),
	      Project=character(),
	      Group1=character(),
	      Group2=character(),
	      Group3=character(),
	      Task=character(),
	      Description=character(),
	      Priority=character(),
	      Urgency=factor(levels=c("High", "Low", NA)),
	      Importance=factor(levels=c("High", "Low", NA)),
	      Effort=character(),
	      Status=ordered(character(),levels=c(Config$Status)),
	      Who=character(),
	      DateAssigned=character(),
	      DateStarted=character(),
	      DateCompleted=character(),
	      DateDue=character()
	      )
	   )

# 2.2 Basic functions
# -------------------------------------------------------------------
     SelectTasks <- function(Organisation="*", Project="*", Group="*", Task="*") 
     {
	T <- Tasks[grep(Organisation, Tasks$Organisation),]
	T <- T[grep(Project, T$Project),]
	T <- T[grep(Group, sprintf("%s-%s-%s", T$Group1, T$Group2, T$Group3)),]
	T <- T[grep(Task, T$Task),]

	return (T)
     }

     SortTasks <- function(T=Tasks, orderBy="Task")
     {
	 # Returns an index of tasks
	 # Sometimes we want this sorted by Task name and sometimes by Status
	 order(T$Organisation, T$Project, sprintf("%s-%s-%s", T$Group1, T$Group2, T$Group3), T[,orderBy])
     }

     # What makes sense for UpdateTask(s)?
     # 1. UpdateTask x Task x Field -> Task, eg UpdateTask x Task x Status = "Closed"
     # Is this the same list of fields for CaptureTask() and NewTask()
     #
     # 2. UpdateTasks Tasks x ID x Field -> Tasks
     # Is a mass update of tasks possible, eg UpdateTasks(Tasks, value, field) -> updatedTasks
     # 
     # How do you make this configurable?
     # Do I need a function at all?  How about just: Tasks[Selection, column] <- value
     UpdateTasks <- function(Selection, column, value) Tasks[Selection, column] <<- value

     # Bulk change or do this row by row
     ChangeStatus <- function(Selection, NewStatus="", byRow=FALSE) {
	# Updating status for PrintTasks(Selection)
	# if nrow(unique) Selection$Status = 0 then OldStatus = ""
	# if nrow(unique) Selection$Status > 1 then OldStatus = "mixed"
	# if nrow(unique) Selection$Status = 1 then OldStatus = "whatever the old status was"
	# Prompt for NewStatus [old status]
	# If NewStatus entered then 
	     # UpdateTasks(Selection, "Status", NewStatus)
     }

     ChangeOwner <- function(Selection) {
     }

     ChangeDescription <- function(Selection) {
     }

     ChangeDates <- function(Selection) {
     }



# -------------------------------------------------------------------
# 2.3 User Interfaces
# 2.3.1 R functions
# Is there a way to define the fields for a Task through configuration?
# How do you deal with changes in the format of a task list?
#
#  . FormatColumn(Task, Column) -> FormattedColumn	
     FormatColumn <- function(Task, Column) {
	if (Column == "ID") return (str_pad(paste("[",Task$ID,"]", sep=""),7))
	if (Column == "Task") return (str_field(Task$Task,30, side="right"))
	if (Column == "Status") return (paste("[",str_field(Task$Status, width=8, side="both"),"]", sep=""))
	if (Column == "Who") return (str_field(Task$Who, width=8, side="left"))
	if (Column == "DateDue") return (str_field(Task$DateDue, width=8, side="right"))
	if (Column == "Description") return (paste("\"",str_field(Task$Description,60, side="right"),"\"", sep=""))

	warning(sprintf("Unknown : Column=\"%s\"", Column))
	return (character())
     }
#
#  . CalculatePriority(Task) -> Priority
#
     CaptureTask <- function(Organisation="", Project="")
     {
        # Select & Validate the project
	GetInput(prompt=sprintf("Organisation [%s] : ", Organisation), default=Organisation) -> Organisation
	GetInput(prompt=sprintf("Project      [%s] : ", Project), default=Project) -> Project

	(Tasks$Organisation==Organisation & Tasks$Project==Project) -> rows
	print(unique(sort(paste(Tasks$Group1[rows],Tasks$Group2[rows],Tasks$Group3[rows], sep="-"))))
	cat("\n")

	# Variable names for the following groups based on the project type
	GetInput(prompt="Group1           : ") -> Group1
	GetInput(prompt="Group2           : ") -> Group2
	GetInput(prompt="Group3           : ") -> Group3
	
	# What needs to be done
	GetInput(prompt="Task             : ") -> Task
	GetInput(prompt="Description      : ") -> Description

	# Priority could be derived.  Some people may want to enter it.
	GetInput(prompt="Urgency          : ") -> Urgency
	GetInput(prompt="Importance       : ") -> Importance
	GetInput(prompt="Effort           : ") -> Effort
	     
	# Can we derive the status?  How do we ensure that only the valid status types are entered.
	GetInput(prompt="Status           : ") -> Status
	GetInput(prompt="Who              : ") -> Who
	GetInput(prompt="Due Date         : ") -> DateDue

	return(
	   data.frame(
	      ID="",
	      Organisation=Organisation,
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

     # Universal task capture and update
     EditTasks <- function(Tasks=EmptyTasks())
     {
	data.frame() -> T

	for (row in 0:NROW(Tasks)) {     
	    if (row == 0) {
		ID=character()
		Organisation=character()
		Project=character()
		Group1=character()
		Group2=character()
		Group3=character()
		Task=character()
		Description=character()
		Priority=character()
		Urgency=factor(levels=c("High", "Low", NA))
		Importance=factor(levels=c("High", "Low", NA))
		Effort=character()
		Status=ordered(character(),levels=c(Config$Status))
		Who=character()
		DateAssigned=character()
		DateStarted=character()
		DateCompleted=character()
		DateDue=character()
	    } else {
		ID=Tasks$ID[row]
		Organisation=Tasks$Organisation[row]
		Project=Tasks$Project[row]
		Group1=Tasks$Group1[row]
		Group2=Tasks$Group2[row]
		Group3=Tasks$Group3[row]
		Task=Tasks$Task[row]
		Description=Tasks$Description[row]
		Priority=Tasks$Priority[row]
		Urgency=Tasks$Urgency[row]
		Importance=Tasks$Importance[row]
		Effort=Tasks$Effort[row]
		Status=Tasks$Status[row]
		Who=Tasks$Who[row]
		DateAssigned=Tasks$DateAssigned[row]
		DateStarted=Tasks$DateStarted[row]
		DateCompleted=Tasks$DateCompleted[row]
		DateDue=Tasks$DateDue[row]
	    }

            # Select & Validate the project
	    GetInput(prompt=sprintf("Organisation [%s] : ", Organisation), default=Organisation) -> Organisation
	    GetInput(prompt=sprintf("Project      [%s] : ", Project), default=Project) -> Project

	    (Tasks$Organisation==Organisation & Tasks$Project==Project) -> rows
	    print(unique(sort(paste(Tasks$Group1[rows],Tasks$Group2[rows],Tasks$Group3[rows], sep="-"))))
	    cat("\n")

	    # Variable names for the following groups based on the project type
	    GetInput(prompt=sprintf("Group1      [%s] : ", Group1), default=Group1) -> Group1
	    GetInput(prompt=sprintf("Group2      [%s] : ", Group2), default=Group2) -> Group2
	    GetInput(prompt=sprintf("Group3      [%s] : ", Group3), default=Group3) -> Group3
	
	    # What needs to be done
	    GetInput(prompt=sprintf("Task        [%s] : ", Task), default=Task) -> Task
	    # OK I need a print function for each column
	    GetInput(prompt=sprintf("Description [%s] : ", Description), default=Description) -> Description

	    # Priority could be derived.  Some people may want to enter it.
	    GetInput(prompt=sprintf("Urgency     [%s] : ", Urgency), default=Urgency) -> Urgency
	    GetInput(prompt=sprintf("Importance  [%s] : ", Importance), default=Importance) -> Importance
	    GetInput(prompt=sprintf("Effort      [%s] : ", Effort), default=Effort) -> Effort
	     
	    # Can we derive the status?  How do we ensure that only the valid status types are entered.
	    GetInput(prompt=sprintf("Status      [%s] : ", Status), default=Status) -> Status
	    GetInput(prompt=sprintf("Who         [%s] : ", Who), default=Who) -> Who
	    GetInput(prompt=sprintf("Due Date    [%s] : ", DateDue), default=DateDue) -> DateDue

	    rbind(T, data.frame(
	            ID=ID,
	            Organisation=Organisation,
	            Project=Project,
	            Group1=Group1,
	            Group2=Group2,
	            Group3=Group3,
	            Task=Task,
	            Description=Description,
	            Priority=Priority,
	            Urgency=Urgency,
	            Importance=Importance,
	            Effort=Effort,
	            Status=Status,
	            Who=Who,
	            DateAssigned=DateAssigned,
	            DateStarted=DateStarted,
	            DateCompleted=DateCompleted,
	            DateDue=DateDue,
	            stringsAsFactors=FALSE
	        )
	    )
     }
	return(T)
     }

     NewTask <- function(Organisation="", Project="")
     {
   	CaptureTask(Organisation, Project) -> T
           if (nrow(T) > 0)
              rbind(Tasks, T) -> Tasks

        IndexTasks(Tasks)
     }

     PrintTasks <- function (T) {
        SortTasks(T) -> Index
        if (nrow(T) > 0)
	    for (row in Index) {
		cat(sprintf("[%s|%s|%s]", T$ID[row], T$Status[row], T$Who[row]))
		cat(sprintf(" %s/%s", T$Organisation[row], T$Project[row]))
		cat(sprintf(" %s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row]))
		cat(sprintf(" %s \"%s\"\n", T$Task[row], T$Description[row]))
	    }
     }

     ListTasks <- function (Organisation="*", Project="*", Group="*", Status=c("InProgress","Next","Completed","Defect"), show="Summary", paginate=TRUE) 
     {
	if (paginate) 
	    sink(file=".Tasks.ListTasks", type="output")

	# Filter out the entries we want
	#   by Project and group
	SelectTasks(Organisation, Project, Group) -> T1
	#   by Status
	data.frame() -> T
	for (s in Status)
	    rbind(T, T1[grep(s, T1$Status),]) -> T

	# Now printout in different formats
	if (show == "Full") {
	    SortTasks(T) -> Index

	    if (nrow(T) > 0) 
	        for (row in Index) {
	    	    if (Organisation != T$Organisation[row]) {
    	                Organisation = T$Organisation[row]
	                cat("----------------------\n")
	                cat(sprintf("Organisation: %s\n", T$Organisation[row]))
	    	    }

	    	    if (Project != T$Project[row]) {
	    	        Project = T$Project[row]
		        cat("  ......................\n")
	    	        cat(sprintf("  Project: %s\n", T$Project[row]))
	        	}

	    	    if (Group != sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])) {
		        Group = sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])
	    	        cat(sprintf("    %s-%s-%s\n", T$Group1[row], T$Group2[row], T$Group3[row]))
	    	    }

		    cat(sprintf("    %s %s\n", str_pad(paste("[",T$ID[row],"]", sep=""),7), T$Task[row]))
		    # Nice to have a formatted description - multiple rows and word wrapped.
		    cat(sprintf("            \"%s\"\n", T$Description[row]))
		    cat(sprintf("            Status [%s]", T$Status[row]))
		    cat(sprintf(", Priority [%s]", T$Priority[row]))
		    cat(sprintf(", Assigned to: %s. Due: %s\n", T$Who[row], T$DateDue[row]))
		}
	} else if (show == "Summary") {
	    SortTasks(T) -> Index

	    if (nrow(T) > 0) 
	        for (row in Index) {
	    	    if (Organisation != T$Organisation[row]) {
    	                Organisation = T$Organisation[row]
	                cat("----------------------\n")
	                cat(sprintf("Organisation: %s\n", T$Organisation[row]))
	    	    }

	    	    if (Project != T$Project[row]) {
	    	        Project = T$Project[row]
		        cat("  ......................\n")
	    	        cat(sprintf("  Project: %s\n", T$Project[row]))
	        	}

	    	    if (Group != sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])) {
		        Group = sprintf("%s-%s-%s", T$Group1[row], T$Group2[row], T$Group3[row])
	    	        cat(sprintf("    %s-%s-%s\n", T$Group1[row], T$Group2[row], T$Group3[row]))
	    	    }

		    cat(sprintf("     %s %s %s %s %s\n", 
		       str_pad(paste("[",T$ID[row],"]", sep=""),7), 
		       str_field(T$Task[row],30, side="right"), 
		       paste("[",str_field(T$Status[row], width=8, side="both"),"]", sep=""), 
		       paste("[",str_field(T$Who[row], width=8, side="left"), ":", 
			     str_field(T$DateDue[row], width=8, side="right"), "]",sep=""), 
		       paste("\"",str_field(T$Description[row],60, side="right"),"\"", sep="")))
		}
	} else if (show == "List") {
	    SortTasks(T, orderBy="Status") -> Index

	    if (nrow(T) > 0) 
	        for (row in Index) {
	            cat(sprintf("%s %s : %s %s %s %s\n", 
	            		str_pad(paste("[",T$ID[row],"]", sep=""),7),
				str_field(T$Project[row], 15, side="left"),
		       		str_field(T$Task[row],30, side="right"), 
				paste("[",str_field(T$Status[row], width=8, side="both"),"]", sep=""), 
				str_field(T$Priority[row], width=10, side="both"), 
		       		paste("\"",str_field(T$Description[row],60, side="right"),"\"", sep="")))
		}
	} else if (show == "ShortList") {
	    SortTasks(T, orderBy="Status") -> Index

	    if (nrow(T) > 0) 
	        for (row in Index) {
	            cat(sprintf("%s %s : %s %s\n", 
	            		str_pad(paste("[",T$ID[row],"]", sep=""),7),
				str_field(T$Project[row], 15, side="left"),
		       		str_field(T$Task[row],30, side="right"), 
		       		paste("\"",str_field(T$Description[row],60, side="right"),"\"", sep="")))
		}
	} else {
	    warning(sprintf("Unknown : show=\"%s\"", show))
        }

	if (paginate) {
	   sink()
	   sprintf("Project %s, Group %s, Status (%s), show=%s\n", toString(Project), toString(Group), toString(Status), toString(show)) -> Selection
	   file.show(file=".Tasks.ListTasks", delete.file=TRUE, title=sprintf("Listing tasks for %s", Selection))
	}
     }

     SummariseTasks <- function (Organisation="*", Project="*", Group="*", Task="*")
     {
	SelectTasks(Organisation, Project, Group, Task) -> T
	SortTasks(T) -> Index     

	Organisation = ""
	Project = ""
	if (nrow(T) > 0) 
	    for (row in Index) {
	        if (Organisation != T$Organisation[row]) {
	    	    Organisation = T$Organisation[row]
	    	    cat("===================================================\n")
	            cat(sprintf("Organisation: %s\n", T$Organisation[row]))
		}
	        if (Project != T$Project[row]) {
	    	    Project = T$Project[row]
	            cat("---------------------------------------------------\n")
	            cat(sprintf("Project: %s\n\n", T$Project[row]))
	    	    P <- T[grep(Project, T$Project), ]

		    print(with(P, table(Status, Group1)))
		    cat("\n")
	        }
	    }
	
	cat("\n===================================================\n")
	}
     
    ToDo <- function(Organisation="*", Project="*", Group="*", paginate=TRUE) 
    {
	# Hm... this is just a vertical Kanban sorted by project and filtered for my stuff
	if (paginate)    
	    sink(file=".Tasks.ToDo", type="output")

	cat("Backlog\n")
	cat("===================================================\n")
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status="Backlog", show="List", paginate=FALSE)
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status="Next", show="List", paginate=FALSE)
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status="Defect", show="List", paginate=FALSE)
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status="Waiting", show="List", paginate=FALSE)
	cat("\nToDo\n")
	cat("===================================================\n")
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status=c("Weekly", "InProgress"), show="Summary", paginate=FALSE)
	cat("\nCompleted\n")
	cat("===================================================\n")
	ListTasks(Organisation=Organisation, Project=Project, Group=Group, Status="Complete", show="ShortList", paginate=FALSE)

	if (paginate) {
	   sink()
	   file.show(file=".Tasks.ToDo", delete.file=TRUE, title="To Do list")
	}
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

     # ===========================================================================================
     # Data preparation - depends on the source of the data
     # ===========================================================================================
     
     if (!is.na(argv[1]))
     {
        if (argv[1] == "ToDo")
        {
   	   ToDo(Organisation="DN", paginate=FALSE)
        } 
        else if (argv[1] == "DN")
        {
   	   ToDo("DN")
        } 
        else if (argv[1] == "Home")
        {
   	   ToDo("Home")
        } 
	 else if (argv[1] == "Testing123")
        {
           # Testing 123
           cat("---------------------------------------------\nSummariseTasks()\n\n")
           SummariseTasks()
   
           cat("---------------------------------------------\nListTasks(show=Full)\n\n")
           GetInput(prompt="Next") -> BitBucket
           ListTasks(show="Full")
   
           cat("---------------------------------------------\nListTasks(Group=R functions)\n\n")
           GetInput(prompt="Next") -> BitBucket
           ListTasks(Group="R functions", Status="*")
   
           cat("---------------------------------------------\nListTasks(Project=unknown, paginate=FALSE)\n\n")
           GetInput(prompt="Next") -> BitBucket
           ListTasks(Project="unknown", Status="*", paginate=FALSE)
   
           cat("---------------------------------------------\nToDo list\n\n")
           GetInput(prompt="Next") -> BitBucket
           ToDo()

           cat("---------------------------------------------\nToDo list\n\n")
	   # test the functions : LoadTasks() Empty.csv missing.csv noHeader.csv notAValid.csv
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
     } else 
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

     # Time to go home
     SaveTasks()

     # SaveConfig()

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

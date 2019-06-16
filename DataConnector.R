# ===========================
# DataConnector.R
# ===========================
# Routines to manage data access for R
#
# "Generic" data access routines
# - Interfaces (Init, Close, CRUD) x (Configuration, DataTable)
# - Transformations - what information does the program need and in what format?
# - Access functions rules
# - Implementation (CSV, XML)
#
# Fuck the possibilities 
# - what is the minimum required to make this work?
# - Do the simple thing
# 

# ===================================================================
# 2.   Time Box
# 2.1   Data access
# ===================================================================
# 2.1.1   CSV
# ===================================================================
# Program configuration
# -------------------------------------------------------------------
#
# Configuration.Init -> Environment
Configuration.Init <- function() {
  new.env() -> e
  # Default static configuration
  "-" -> e$cfg
  "default" -> e$sourcetype
  return(e)
}

# Configuration.Close x Environment ->
Configuration.Close <- function(e) {
  # Destroy the Environment - when I work out how to do this
  rm(list=e, pos = parent.env(environment()))
}

# Data access functions
# -------------------------------------------------------------------
# Datasource.New x Environment -> Environment
# Create an empty set of data sources
Datasource.New <- function(e) {
  new.env() -> e$Cache
  
  # Idea
  # - Name
  # - Notes
  # + Bucket
  data.frame(
    Name=character(),
    Notes=character(),
    Bucket=character(),
    stringsAsFactors = FALSE
  ) -> e$Cache$Idea
  
  # Bucket / General classification from Strategy
  # - Name = Bucket-Project-ScopeItem-etc.  Only make this as deep as necessary to capture uniqueness.
  # - Description
  # - ChargeTo
  data.frame(
    Name=character(),
    Description=character(),
    ChargeTo=character(),
    stringsAsFactors = FALSE
  ) -> e$Cache$Bucket
  
  # Task - ToDo list
  # - Name = task-subtask-subtask-
  # - Description
  # + Bucket
  # - Status
  data.frame(
    Name=character(),
    Description=character(),
    Bucket=character(),
    Status=factor(levels=c("Backlog", "ToDo", "Done", NA)),
    Today=logical(),
    stringsAsFactors = FALSE
  ) -> e$Cache$Task
  
  # Work - timesheet information
  # - Date
  # - Duration
  # + Task
  # . ChargeTo - from Bucket
  data.frame(
    Name=character(),
    Description=character(),
    Date=character(),
    Duration=character(),
    ChargeTo=character(),
    stringsAsFactors = FALSE
  ) -> e$Cache$Work
}

# Datasource.Init x Environment -> Environment
Datasource.Init <- function(e) {
  new.env() -> e$Cache
  # Load all tables from the configuration into the cache

  if (e$sourcetype == "default") {
    tryCatch(
      {
        read.csv(file="XXX-Idea.csv", stringsAsFactors = FALSE) -> e$Cache$Idea
        read.csv(file="XXX-Bucket.csv", stringsAsFactors = FALSE) -> e$Cache$Bucket
        read.csv(file="XXX-Task.csv", stringsAsFactors = FALSE) -> e$Cache$Task
        read.csv(file="XXX-Work.csv", stringsAsFactors = FALSE) -> e$Cache$Work
      },
      error = function(e) {
        if (grepl("no lines available in input", e$message))
          stop(sprintf("Datasource.Init: Empty file: %s\n"), e)
          # return(EmptyTasks())
          # Ideally would return an empty dataset for each file - need to wrap each read
        stop(sprintf("Datasource.Init: %s\n"), e)
      },
      warning = function(w) {
        if (grepl("No such file or directory", w$message))
          warning(sprintf("Datasource.Init: %s\n", w))
          # return(EmptyTasks())
        else {
          # all others
          warning(sprintf("Datasource.Init: %s", w))
          # return(EmptyTasks())
        }
      }
    )
  }
}

Datasource.Close <- function(e) {
  # Save all tables in the cache
  if (e$sourcetype == "default") {
    write.csv(e$Cache$Idea, file="XXX-Idea.csv", row.names = FALSE)
    write.csv(e$Cache$Bucket, file="XXX-Bucket.csv", row.names = FALSE)
    write.csv(e$Cache$Task, file="XXX-Task.csv", row.names = FALSE)
    write.csv(e$Cache$Work, file="XXX-Work.csv", row.names = FALSE)
  }
  
  # Destroy the cache
  rm(Cache, pos = e)
}

Datasource.Sync <- function(e) {
  # Save all tables in the cache
  if (e$sourcetype == "default") {
    write.csv(e$Cache$Idea, file="XXX-Idea.csv", row.names = FALSE)
    write.csv(e$Cache$Bucket, file="XXX-Bucket.csv", row.names = FALSE)
    write.csv(e$Cache$Task, file="XXX-Task.csv", row.names = FALSE)
    write.csv(e$Cache$Work, file="XXX-Work.csv", row.names = FALSE)
  }
}

# Hm.. Don't write these until I need them.
# Datasource.Read x Env x Table x filter -> Table<dataframe>
Datasource.Read <- function(e, table) {
  if(!exists("Cache", e))
    stop(sprintf("Datasource not connected"))

  if(!exists(table, e$Cache)) {
    stop(sprintf("No such table \"%s\"", table))}

  # For now return all rows.  
  # Selective reads requires an Update function and I can do that other ways.
  Select="*"

  if(table == "Idea") 
    return(e$Cache$Idea[grep(Select, e$Cache$Idea$Name),])
  else if(table == "Bucket") 
    return(e$Cache$Bucket[grep(Select, e$Cache$Bucket$Name),])
  else if(table == "Task") 
    return(e$Cache$Task[grep(Select, e$Cache$Task$Name),])
  else if(table == "Work") 
    return(e$Cache$Work[grep(Select, e$Cache$Work$Task),])
  else
    stop(sprintf("Invalid table \"%s\"", table))
}

# Datasource.Add x Env x Table x Rows
# - adds Rows to the table
Datasource.Add <- function(e, table, rows) {
  if(!exists("Cache", e))
    stop(sprintf("Datasource not connected"))
  
  if(!exists(table, e$Cache)) {
    stop(sprintf("No such table \"%s\"", table))}
  
  if(table == "Idea") 
    rbind(rows, e$Cache$Idea) -> e$Cache$Idea
  else if(table == "Bucket") 
    rbind(rows, e$Cache$Bucket) -> e$Cache$Bucket
  else if(table == "Task") 
    rbind(rows, e$Cache$Task) -> e$Cache$Task
  else if(table == "Work") 
    rbind(rows, e$Cache$Work) -> e$Cache$Work
  else 
    stop(sprintf("Invalid table \"%s\"", table))
}

# Datasource.Replace x Env x Table x Rows
# - replaces the whole table with Rows
Datasource.Replace <- function(e, table, rows) {
  if(!exists("Cache", e))
    stop(sprintf("Datasource not connected"))
  
  if(!exists(table, e$Cache)) {
    stop(sprintf("No such table \"%s\"", table))}
  
  if(table == "Idea") 
    rows -> e$Cache$Idea
  else if(table == "Bucket") 
    rows -> e$Cache$Bucket
  else if(table == "Task") 
    rows -> e$Cache$Task
  else if(table == "Work") 
    rows -> e$Cache$Work
  else 
    stop(sprintf("Invalid table \"%s\"", table))
}

# Testing
FALSE -> testing
if (testing) {
  cat("\n--------------------\n")
  cat("DataConnector\n")
  cat("--------------------\n")
  cat("* Load configuration\n")
  Configuration.Init() -> E
  cat("Env: "); print(ls(E))
  
  cat("\n** Initialise datasource\n")
  Datasource.Init(E)
  cat("Env: "); print(ls(E))
  cat("Env$Cache: "); print(ls(E$Cache))
  cat("Env$Cache$Idea: "); print(str(E$Cache$Idea))
  cat("Env$Cache$Task: "); print(str(E$Cache$Task))
  # Read/Write/update
  print(Datasource.Read(E, "Idea"))
  print(Datasource.Read(E, "Task"))
  
  cat("\n** Closing datasource\n")
  Datasource.Close(E)
  cat("Env: "); print(ls(E))

  cat("\n** New datasource\n")
  Datasource.New(E)
  cat("Env: "); print(ls(E))
  cat("Env$Cache: "); print(ls(E$Cache))
  cat("Env$Cache$Idea: "); print(str(E$Cache$Idea))
  cat("Env$Cache$Task: "); print(str(E$Cache$Task))
  
  cat("\n*** Adding Tasks\n")
  data.frame(Name="Mow lawn", 
             Description="Only the front lawn",
             Bucket="Home-Maintenance",
             Status="ToDo",
             Today=FALSE,
             stringsAsFactors = FALSE) -> T
  Datasource.Add(E, "Task", T)
  data.frame(Name=c("Wash dishes", "Wash car"), 
             Description=c("In the dishwasher", "Neighbourhood water fight"),
             Bucket=c("Home-Chores", "Home-Maintenance"),
             Status=c("ToDo", "Backlog"),
             Today=c(TRUE, FALSE),
             stringsAsFactors = FALSE) -> T
  Datasource.Add(E, "Task", T)
  data.frame(Name="Watch TV", 
             Description="GOT time",
             Bucket="Recreation",
             Status="ToDo",
             Today=TRUE,
             stringsAsFactors = FALSE) -> T
  Datasource.Add(E, "Task", T)
  cat("Env$Cache$Task: "); print(str(E$Cache$Task))
  print(Datasource.Read(E, "Task"))
  Datasource.Sync(E)
  
  cat("\n** Closing datasource\n")
  Datasource.Close(E)
  cat("Env: "); print(ls(E))
  
  cat("\n* Closing configuration\n")
  Configuration.Close("E")
}

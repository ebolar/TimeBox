# ===========================
# TimeBoxData.R
# ===========================
# TimeBox specific data model
#

source("DataConnector.R")

# 2.2 Basic functions
# -------------------------------------------------------------------
# Generic:
# Select x Table x Selection -> rows<index>
# - Subset=FALSE, default value = give me everything
# - Selection=data.frame(Name="*", Bucket="H*", ...), filter on specific fields
# 
Table.Select <- function(Table, Selection=data.frame()) {
  logical(NROW(Table)) -> I
  TRUE -> I[] 
  
  for (N in names(Selection)) {
    grep(N, names(Table)) -> t
    grep(N, names(Selection)) -> s
    
    if (NROW(t) == 0) warning(sprintf("Invald selection %s", N), immediate. = TRUE)
    
    FALSE -> I[-(grep(Selection[,s], Table[,t]))]
  }
  return(I)
}

# Sort x Table -> Rows
#

# Constructors
# -------------------------------------------------------------------
Idea <- function(Name=character(), Notes=character(), Bucket=character(), CreatedAt=integer(), LastUpdate=integer()) {
  # - Name
  # - Notes
  # + Bucket

  if (length(Name) > 0 || length(Notes) > 0 || length(Bucket) > 0 || length(CreatedAt) > 0 || length(LastUpdate) > 0) {
    if (length(Name) == 0) "" -> Name
    if (length(Notes) == 0) "" -> Notes
    if (length(Bucket) == 0) "" -> Bucket
    if (length(CreatedAt) == 0) Sys.time() -> CreatedAt
    if (length(LastUpdate) == 0) Sys.time() -> LastUpdate
  }  
  data.frame(
    Name=Name,
    Notes=Notes,
    Bucket=Bucket,
    CreatedAt=CreatedAt,
    LastUpdate=LastUpdate,
    stringsAsFactors = FALSE
  )
}

Bucket <- function(Name=character(), Description=character(), ChargeTo=character(), CreatedAt=integer(), LastUpdate=integer()) {
  # Bucket / General classification from Strategy
  # - Name = Bucket-Project-ScopeItem-etc.  Only make this as deep as necessary to capture uniqueness.
  # - Description
  # - ChargeTo

  if (length(Name) > 0 || length(Description) > 0 || length(ChargeTo) > 0 || length(CreatedAt) > 0 || length(LastUpdate) > 0) {
    if (length(Name) == 0) "" -> Name
    if (length(Description) == 0) "" -> Description
    if (length(ChargeTo) == 0) "" -> ChargeTo
    if (length(CreatedAt) == 0) Sys.time() -> CreatedAt
    if (length(LastUpdate) == 0) Sys.time() -> LastUpdate
  }  
  data.frame(
    Name=Name,
    Description=Description,
    ChargeTo=ChargeTo,
    CreatedAt=CreatedAt,
    LastUpdate=LastUpdate,
    stringsAsFactors = FALSE
  )
}

Task <- function(Name=character(), Description=character(), Bucket=character(), Status=character(), Today=logical(), CreatedAt=integer(), LastUpdate=integer()) {
  # Task - ToDo list
  # - Name = task-subtask-subtask-
  # - Description
  # + Bucket
  # - Status

  if (length(Name) > 0 || length(Description) > 0 || length(Bucket) > 0 || length(Status) > 0 || length(CreatedAt) > 0 || length(LastUpdate) > 0) {
    if (length(Name) == 0) "" -> Name
    if (length(Description) == 0) "" -> Description
    if (length(Bucket) == 0) "" -> Bucket
    if (length(Status) == 0) "" -> Status
    if (length(Today) == 0) FALSE -> Today
    if (length(CreatedAt) == 0) Sys.time() -> CreatedAt
    if (length(LastUpdate) == 0) Sys.time() -> LastUpdate
  }  
  data.frame(
    Name=Name,
    Description=Description,
    Bucket=Bucket,
    Status=Status,
    Today=Today,
    CreatedAt=CreatedAt,
    LastUpdate=LastUpdate,
    stringsAsFactors = FALSE
  )
}

Work <- function(Date=integer(), Duration=integer(), Task=character(), ChargeTo=character()) {
  # Work - timesheet information
  # - Date
  # - Duration
  # + Task
  # . ChargeTo - from Bucket

  if (length(Date) > 0 || length(Duration) > 0 || length(Task) > 0 || length(ChargeTo) > 0) {
    if (length(Date) == 0) Sys.time() -> Date
    if (length(Duration) == 0) 0 -> Duration
    if (length(Task) == 0) "" -> Task
    if (length(ChargeTo) == 0) "" -> ChargeTo
  }  
  data.frame(
    Date=Date,
    Duration=Duration,
    Task=Task,
    ChargeTo=ChargeTo,
    stringsAsFactors = FALSE
  )
}

format.Idea <- function(idea=Idea(), as = "DT") {
  # - Name
  # - Notes
  # + Bucket
  data.frame(Name=idea$Name, Notes=idea$Notes, Bucket=idea$Bucket) 
}

format.Bucket <- function(bucket=Bucket(), as = "DT") {
  # - Name = Bucket-Project-ScopeItem-etc.  Only make this as deep as necessary to capture uniqueness.
  # - Description
  # - ChargeTo
  data.frame(Name=bucket$Name, Description=bucket$Description, ChargeTo=bucket$ChargeTo)
}

format.Task <- function(task=Task(), bucket=Bucket(), as = "DT") {
  # - Name = task-subtask-subtask-
  # - Description
  # + Bucket
  # - Status
  
  switch(as, 
    "DT" = {
      # [unsorted]
      # Task - Description - Bucket - Status
      data.frame(Name=task$Name, Description=task$Description, Bucket=task$Bucket, Status=task$Status, stringsAsFactors = FALSE)
    },
    "print" = {
      # Bucket - Project
      # [for now]
      #   Task - Who - Status
      #        - Description
      # [maybe later]
      #   Task - Description - Who - Status
      #        - Description - Who - Status [Today]
      #        - Description - Who - Status

      print(paste("Project summary:", Sys.time()))
      print("=====================================")
      for (x in unique(task$Bucket)) {
        print(sprintf("%s [ChargeTo:%s]", x, bucket[bucket$Name == x, "ChargeTo"]))
        
        task[task$Bucket == x, ] -> t

	for (y in order(t$Status, t$Name)) {
	  print(sprintf("  %s [%s]", t$Name[y], t$Status[y]))
	  for (z in strsplit(t$Description[y], "\n"))
	    print(sprintf("    %s", z))
        }
      }
    },
    "xml" = {
      # [Today]
      cat("XML\n")
      for (x in unique(task$Bucket)) {
        print(sprintf("<Bucket ChargeTo='%s'>%s", bucket[bucket$Name == x, "ChargeTo"], x))
        
        task[task$Bucket == x, ] -> t

	for (y in order(t$Status, t$Name)) {
	  print(sprintf("  %s [%s]", t$Name[y], t$Status[y]))
	  for (z in strsplit(t$Description[y], "\n"))
	    print(sprintf("    %s", z))
        }
      }
    },
    "Markdown" = {
      # and/or any other pretty formatted list
      # Bucket - Project
      #   Task - Description - Status
      #        - Description - Status [Today]
      #        - Description - Status
      cat("Markdown\n")
    },
    "csv" = {
      # [Today]
      # Bucket - Task - Description - Status.
      cat("CSV extract\n")
    },
    "Kanban" = {
      # [Today]
      cat("Kanban Board\n")
    },
    "Today" = {
      # [Today]
      # Bucket - Task - Description - Status.
      # [then the rest]
      # Bucket - Project
      #   Task - Description - Status
      #        - Description - Status
      cat("ToDo list for today\n")
    }

  )

}

format.Work <- function(work=Work(), as = "DT") {
  work
}


# Testing
# -------------------------------------------------------------------
FALSE -> testing

if (testing) {
  cat("\n--------------------\n")
  cat("TimeBoxData\n")
  cat("--------------------\n")
  
  cat("* Config environment and initialise datasource\n")
  Configuration.Init() -> E
  Datasource.Init(E) 
  
  cat("\n** Reading data\n")
  Datasource.Read(E, "Task") -> T
  print(summary(T))
  
  cat("\n** Selecting: Today's ToDo list\n")
  print(T[Table.Select(T, data.frame(Status="ToDo", Today=TRUE)),])
  
  cat("\n** Selecting: Maintenance bucket\n")
  print(T[Table.Select(T, data.frame(Bucket="Maintenance")),])
  
  cat("\n** Selecting: Starts with W\n")
  print(T[Table.Select(T, data.frame(Name="^W")),])
  
  cat("\n* Cleanup\n")
  rm(T)
  Datasource.Close(E) 
  Configuration.Close("E")
}


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


rm(list=ls())
source("TimeBoxData.R")

Configuration.Init() ->E
Datasource.Init(E)

Datasource.Read(E, "Idea") -> I
Datasource.Read(E, "Bucket") -> B
Datasource.Read(E, "Task") -> T
Datasource.Read(E, "Work") -> W

Cleanup <- function() {
  Datasource.Close(E)
  Configuration.Close("E")
}

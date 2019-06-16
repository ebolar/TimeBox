# ===========================
# TimeBoxUI.R
# ===========================
# TimeBox user interface routines
#

# Time for an inversion of the locus of control :)
UI.Init <- function(e) {
  # Basic menu item
  eval(call("menu", c("one", "two", "three"), title="Top")) -> Selection
  
  # Selecting from list of items to work on
  # Need to unpack the tasks and present as a list of formatted strings
  eval(call("menu", print(E$Cache$Task))) -> Selection
  
  # getting an option
  eval(call("menu", E$Cache$Bucket$Name, graphics = TRUE, title="Which Bucket")) -> Selection
  print(E$Cache$Bucket$Name[Selection])
  
  #select.list

}






if (FALSE)
{
  # -------------------------------------------------------------------
# 2.3 User Interfaces
# 2.3.1 R functions
# Is there a way to define the fields for a Task through configuration?
# How do you deal with changes in the format of a task list?
#
#  . FormatColumn(Task, Column) -> FormattedColumn	
FormatColumn <- function(Task, Column) {
  if (Column == "DateDue") return (str_field(Task$DateDue, width=8, side="right"))
  if (Column == "Description") return (paste("\"",str_field(Task$Description,60, side="right"),"\"", sep=""))
  
  warning(sprintf("Unknown : Column=\"%s\"", Column))
  return (character())
}
#
CaptureTask <- function()
{
  # What needs to be done
  GetInput(prompt="Task             : ") -> Task
  GetInput(prompt="Description      : ") -> Description
  
  # Can we derive the status?  How do we ensure that only the valid status types are entered.
  GetInput(prompt="Status           : ") -> Status
  GetInput(prompt="Who              : ") -> Who
  GetInput(prompt="Due Date         : ") -> DateDue
  
  return(
    data.frame(
      Task=Task,
      Description=Description,
      Bucket,
      Effort=Effort,
      Status=Status,
      Who=Who,
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

}

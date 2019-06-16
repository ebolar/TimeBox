# ===========================
# DataConnector.R
# ===========================
# Routines to manage data access for R
#
# - Interfaces (Init, Close, CRUD) x (Configuration, DataTable)
# - Transformations - what information does the program need and in what format?
# - Access functions rules
# - Implementation (CSV, XML)
#
# Fuck the possibilities - what is the minimum required to make this work!
# 

# For development only - remove
rm(list=ls())

#library(xml2)   # https://cran.r-project.org/web/packages/xml2/xml2.pdf

# Interfaces
# Configuration.Init -> Configuration
Configuration.Init <- function() {
  new.env() -> e
  # Basic configuration
  Configuration.Defaults(e)
}

Configuration.Close <- function() {}
Configuration.Defaults <- function(e) {
  # Static configuration
  "-" -> e$cfg
  "csv" -> e$sourcetype
  return(e)
}

# Datasource.Init x Configuration -> Datasource
Datasource.Init <- function(e) {
  new.env() -> Cache
  # Load all tables from the configuration into the cache
  
    
  return(Cache)
}

Datasource.Close <- function(cache) {
  # Save all tables in the cache
  # Destroy the cache
}
Datasource.Sync <- function(cache) {
  # Save all tables in the cache
}
# Hm..do I need these wrappers?
# Datasource.Read x Datasource$Table -> Table<dataframe>
Datasource.Read <- function(d) {}
# Datasource.Write x Datasource$Table x Table<dataframe> -> 
Datasource.Write <- function(d, t) {}
# Datasource.Write x Datasource$Table x Table<dataframe> -> Table<dataframe>
Datasource.Update <- function() {}

# Testing
Configuration.Init() -> Env

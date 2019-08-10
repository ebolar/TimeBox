# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# global.R
#
# V 0.1 - MVP useful application
# V 0.2 - Split into global.R, server.R and ui.R to make maintenance easier
# -------------------------------------------------------------------
# Time management in R for busy people, and a good excuse for me to 
# learn more about R.
# 
# -------------------------------------------------------------------
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TBD

rm (list = ls())

library(shiny)
library(shinyjs)
library(shinyEventLogger)
library(DT)

source("TimeBoxData.R")
source("HelperFunctions.R")

"V0.2" -> TimeBox.Version

# This might work better as a list or included in the Environment structure
whoami::username() -> TimeBox.Username
whoami::fullname() -> TimeBox.Fullname
whoami::email_address() -> TimeBox.email

set_logging(js_console=FALSE, file=FALSE)


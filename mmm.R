
library(tools);
library(gtools);
library(assertthat);
library(testthat);
library(readr);
library(RCurl);
library(rdrop2);
library(plyr);
library(dplyr);
library(lubridate);
library(stringr);
library(quantmod);
library(ggplot2);
library(googleVis);

library(shiny);
library(shinydashboard);
library(shinyjs);
library(shinyAce);

source("mmmR.R");
source("mmmString.R");
source("mmmCalendar.R");
source("mmmFileSystem.R");
source("mmmDropBox.R");
source("mmmApp.R");

source("mmmShinyParts.R")

String <- mmmStringAPI();

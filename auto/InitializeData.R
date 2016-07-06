rm(list=ls());
setwd("~/Documents/R/ShinyTrades");
source("bootstrap.R", chdir = TRUE);

library(testthat);

TestPath <- "~/Documents/R/ShinyTrades/auto/InitializeDataScripts";


TradeStation <- .TradeStation(validate = TRUE);
Scraper <- .Scraper(validate = TRUE);

if (FALSE){
  x <- test_dir(path = "~/Documents/R/ShinyTrades/auto/InitializeDataScripts", reporter = RstudioReporter);
  View(as.data.frame(x));
}

if (FALSE){
  x <- test_file(path = file.path(TestPath, "test-InitalizeTradeStationFiles.R"), reporter = RstudioReporter);
  View(as.data.frame(x));
}

if (FALSE){
  x <- test_file(path = file.path(TestPath, "test-InitializeScrapes.R"), reporter = RstudioReporter);
  View(as.data.frame(x));
}

View(as.data.frame(x));

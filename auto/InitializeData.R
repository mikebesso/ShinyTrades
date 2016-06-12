rm(list=ls());
setwd("~/Documents/R/ShinyTrades");
source("bootstrap.R", chdir = TRUE);

library(testthat);

TestPath <- "~/Documents/R/ShinyTrades/auto/InitializeDataScripts";


TradeStation <- .TradeStation(validate = TRUE);
Scraper <- .Scraper(validate = TRUE);

#x <- test_dir(path = "~/Documents/R/ShinyTrades/auto/InitializeDataScripts", reporter = RstudioReporter);

x <- test_file(path = file.path(TestPath, "test-InitalizeTradeStationFiles.R"), reporter = RstudioReporter);

#x <- test_file(path = file.path(TestPath, "test-InitializeScrapes.R"), reporter = RstudioReporter);

View(as.data.frame(x));

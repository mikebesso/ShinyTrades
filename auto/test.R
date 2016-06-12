rm(list=ls());
setwd("~/Documents/R/ShinyTrades");
source("bootstrap.R", chdir = TRUE);

library(testthat);

x <- test_dir(path = "~/Documents/R/ShinyTrades/auto/tests", reporter = RstudioReporter);


View(as.data.frame(x));

Data.XL <- function(x) {


  RowCount <- length(index(x));



  if ("xl_SD3" %in% colnames(x)) {
    return(
      x[,
        c(
          "High", "Low",
          "xl_SD3", "xl_SP3", "xl_SD2", "xl_SP2", "xl_SD1", "xl_SP1",
          "xl_DP1", "xl_DD1", "xl_DP2", "xl_DD2", "xl_DP3", "xl_DD3"
        )
      ]
    );
  } else {
    return(
      data.frame(
        xl_SD3 = rep(0, RowCount), xl_SP3 = rep(0, RowCount),
        xl_SD2 = rep(0, RowCount), xl_SD1 = rep(0, RowCount),
        xl_SD1 = rep(0, RowCount), xl_SP1 = rep(0, RowCount),
        xl_DP1 = rep(0, RowCount), xl_DD1 = rep(0, RowCount),
        xl_DP2 = rep(0, RowCount), xl_DD2 = rep(0, RowCount),
        xl_DP3 = rep(0, RowCount), xl_DD3 = rep(0, RowCount)
      )
    )
  }
}

addXL <- newTA(
  Data.XL,
  on = NA,
  col = c('black', 'black', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue')
);




# source("bootstrap.R");
#
# Scraper$ProcessMM();
#
# TradeStationFolders <- FileSystem$Folders(TradeStation$OHLCFolder)
#
# TradeStationFiles <- ldply(
#   FileSystem$Folders(TradeStation$OHLCFolder),
#   .fun = function(folder) {
#     files <- FileSystem$Files(folder);
#     interval <- sapply(str_split(basename(files), "[-.]"), function(x) x[2]);
#     ticker <- basename(folder);
#     df <- data.frame(
#       Symbol = ticker,
#       TradeStationFile = files,
#       Ticker = ticker,
#       Interval = interval,
#       stringsAsFactors = FALSE
#     );
#
#     df <- subset(df, !interval %in% c("W", "M"))
#
#   }
# );
#
#
# LevelFiles <- FileSystem$Files(Scraper$LevelsFolder);
#
# LevelFiles <- data.frame(Symbol = FileSystem$RemoveExtension(basename(LevelFiles)), LevelFile = LevelFiles, stringsAsFactors = FALSE);
#
# TradeStationLevelFiles <- merge(TradeStationFiles, LevelFiles);
#
#
# MergeLevelIndicator <- function(TradeStationFile, Ticker, Interval) {
#
#
#   TradeStation$LoadDataFile(TradeStationFile);
#
#   TradeStationZoo <- TradeStation$Data(Ticker, Interval);
#   LevelsZoo <- Scraper$Data(Ticker);
#
#
#   merge.zoo(tail(TradeStationZoo,20), LevelsZoo, fill = TRUE);
#
# }
#
#
#
# EndPointsDaily <- function(INDEX, DayStartTime = '00:00') {
#
#   if (is.zoo(INDEX) | is.xts(INDEX)) {
#     INDEX <- index(INDEX);
#   }
#
#   if (DayStartTime != '00:00'){
#     Hours_Minutes <- as.integer(unlist(str_split(DayStartTime, ':')));
#     INDEX <- INDEX - (3600L * Hours_Minutes[1]) - (60L * Hours_Minutes[2]);
#   }
#
#   endpoints(INDEX, 'days');
#
# }
#
# ApplyDaily <- function(x, INDEX, DayStartTime = '00:00', FUN, ...){
#
#   EndPoints <- EndPointsDaily(INDEX, DayStartTime);
#
#   apply.daily(x, EndPoints, FUN, ...);
#
# }
#
#
# TradeStationFile <- '/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/OHLC/@AD/@AD-5M.csv';
# Ticker <- '@AD'
# Interval <- '5M';
#
#
# TradeStation$LoadDataFile(TradeStationFile);
#
# xtsAttributes(ChartData) <- list( indicators = c('xlevels', 'ma'));
#
# AddXLevelsToChartData <- function(ChartData, Interval) {
#
#   Interval <- xtsAttributes(ChartData)$interval;
#   Symbol <- xtsAttributes(ChartData)$ticker;
#
#   LevelsZoo <- Scraper$Data(Ticker);
#
#   ep <- EndPointsDaily(index(TradeStationZoo), "07:00");
#   sp <- (ep + 1)[-length(ep)];
#   ep <- ep[-1];
#   n <- 0;
#
#   df <- ldply(
#     1:length(ep),
#     function(x) {
#       tsz <- TradeStationZoo[sp[x]:ep[x]];
#       n <- nrow(tsz);
#
#       df <- data.frame(
#         timestamp = index(tsz),
#         SD3 = NA, SP3 = NA, SD2 = NA, SP2 = NA, SD1 = NA, SP1 = NA,
#         DP1 = NA, DD1 = NA, DP2 = NA, DD2 = NA, DP3 = NA, DD3 = NA
#       );
#       date_str <- paste0(as.Date(index(TradeStationZoo)[ep[x]]));
#       ti <- LevelsZoo[date_str];
#
#       if (nrow(ti) > 0) {
#         df$SD3 <- rep(LevelsZoo[date_str]$SD3, n);
#         df$SP3 <- rep(LevelsZoo[date_str]$SP3, n);
#         df$SD2 <- rep(LevelsZoo[date_str]$SD2, n);
#         df$SP2 <- rep(LevelsZoo[date_str]$SP2, n);
#         df$SD1 <- rep(LevelsZoo[date_str]$SD1, n);
#         df$SP1 <- rep(LevelsZoo[date_str]$SP1, n);
#         df$DP1 <- rep(LevelsZoo[date_str]$DP1, n);
#         df$DD1 <- rep(LevelsZoo[date_str]$DD1, n);
#         df$DP2 <- rep(LevelsZoo[date_str]$DP2, n);
#         df$DD2 <- rep(LevelsZoo[date_str]$DD2, n);
#         df$DP3 <- rep(LevelsZoo[date_str]$DP3, n);
#         df$DD3 <- rep(LevelsZoo[date_str]$DD3, n);
#       }
#
#       df
#     }
#   );
#
#   df_zoo <- read.zoo(df, format = "%Y-%m-%d %H:%M", tz = indexTZ(TradeStationZoo), index.column = "timestamp");
#
#   df_zoo
# }
#




#
# TradeStationZoo <- TradeStation$Data(Ticker, Interval);
# LevelsZoo <- Scraper$Data(Ticker);
#
#
#
#
#
# ep <- EndPointsDaily(index(TradeStationZoo), "07:00");
# sp <- (ep + 1)[-length(ep)];
# ep <- ep[-1];
# n <- 0;
#
# df <- ldply(
#   1:length(ep),
#   function(x) {
#     tsz <- TradeStationZoo[sp[x]:ep[x]];
#     n <- nrow(tsz);
#
#     df <- data.frame(
#       timestamp = index(tsz),
#       SD3 = NA, SP3 = NA, SD2 = NA, SP2 = NA, SD1 = NA, SP1 = NA,
#       DP1 = NA, DD1 = NA, DP2 = NA, DD2 = NA, DP3 = NA, DD3 = NA
#     );
#     date_str <- paste0(as.Date(index(TradeStationZoo)[ep[x]]));
#     ti <- LevelsZoo[date_str];
#
#     if (nrow(ti) > 0) {
#       df$SD3 <- rep(LevelsZoo[date_str]$SD3, n);
#       df$SP3 <- rep(LevelsZoo[date_str]$SP3, n);
#       df$SD2 <- rep(LevelsZoo[date_str]$SD2, n);
#       df$SP2 <- rep(LevelsZoo[date_str]$SP2, n);
#       df$SD1 <- rep(LevelsZoo[date_str]$SD1, n);
#       df$SP1 <- rep(LevelsZoo[date_str]$SP1, n);
#       df$DP1 <- rep(LevelsZoo[date_str]$DP1, n);
#       df$DD1 <- rep(LevelsZoo[date_str]$DD1, n);
#       df$DP2 <- rep(LevelsZoo[date_str]$DP2, n);
#       df$DD2 <- rep(LevelsZoo[date_str]$DD2, n);
#       df$DP3 <- rep(LevelsZoo[date_str]$DP3, n);
#       df$DD3 <- rep(LevelsZoo[date_str]$DD3, n);
#     }
#
#     df
#   }
# );
#
# df_zoo <- read.zoo(df, format = "%Y-%m-%d %H:%M", tz = indexTZ(TradeStationZoo), index.column = "timestamp");
#
# tail(merge(TradeStationZoo, df_zoo), 20)
#
# head(TradeStationZoo);
# head(df_zoo);
#
# nrow(TradeStationZoo)
# nrow(df_zoo)
#
#
# TradeStationZoo[!(index(TradeStationZoo) %in% index(df_zoo))];
# df_zoo[!(index(df_zoo) %in% index(TradeStationZoo))];


#
# IndicatorFile <-
#   ddply(
#     TradeStationLevelFiles,
#     .(TradeStationFile),
#     .fun = function(f) {
#
#       browser();
#       MergeLevelIndicator(f$TradeStationFile, f$Ticker, f$Interval)
#
#     }
#   )

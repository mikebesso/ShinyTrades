
# .TradeStationFile <- function(filename, time_zone = App$GetTimeZone(), validate = FALSE){
#
#   Symbol <- NULL;
#   Interval <- NULL;
#
#   SetSymbolAndInterval <- function(filename){
#     symbol_and_interval = unlist(str_split(basename(filename), "[-.]"));
#
#     Symbol <<- toupper(symbol_and_interval[1]);
#     Interval <<- symbol_and_interval[2];
#
#   }
#
#   SetSymbolAndInterval(filename);
#
#   Validate <- validate;
#   TimeZone <- time_zone;
#
#
# }


.TradeStation <- function(path = file.path("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/ohlc"), time_zone = App$GetTimeZone(), validate = FALSE){

  ColumnTypes = "cddddd";
  ColumnNames = c("timestamp", "Close", "High", "Low", "Open", "Volume");

  LocalOHLCFolder <- '/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/OHLC';
  DropBoxFolder <- 'ohlc';
  DropBoxCache <- 'DropBoxCache/ohlc';

  FileSystem$CreateFolder(DropBoxCache);

  PreCachedFiles <- FileSystem$Files(path = DropBoxCache, recursive = TRUE);
  FileSystem$DeleteFiles(PreCachedFiles);

  Validate <- validate;

  TimeZone <- time_zone;

    # SetSymbolAndInterval <- function(filename){
    #   symbol_and_interval = unlist(str_split(basename(filename), "[-.]"));
    #   Symbol <<- toupper(symbol_and_interval[1]);
    #   Interval <<- symbol_and_interval[2];
    #
    # }


  Zoo2XTS2Cache <- function(z, Symbol, Interval) {
    x <- as.xts(z);
    indexTZ(x) <- App$GetTimeZone();
    xtsAttributes(x) <- list(ticker = Symbol, interval = Interval);
    assign(Symbol, x, envir = DetermineIntervalEnvironment(Interval));
    x
  }


  ReadTradeStationCSV2Zoo2XTS <- function(Filename, Symbol, Interval){

     df <- read_csv(
      Filename,
      col_types = ColumnTypes
    );
    names(df) <- ColumnNames;

    format = Calendar$DetermineDateTimeFormat(df$timestamp);

    if (Validate) {
      cat(deparse(match.call()), "\n");
      assert_that(!is.null(format));
      assert_that(sum(duplicated(df$timestamp)) == 0);
    }


    x <- strptime(df$timestamp, format = format, tz = App$GetTimeZone());
    z <- zoo(df[2:6], x);

    return(Zoo2XTS2Cache(z, Symbol, Interval));
  }

  # Initialize Environments
  if (!exists("TradeStationEnvironments")){

    TradeStationEnvironments <<- new.env(parent = .GlobalEnv);

    assign("Interval5Minute", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("Interval4Hour", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("IntervalMonth", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("IntervalWeek", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("IntervalDay", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("IntervalHour", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);
    assign("IntervalQuarterHour", new.env(parent = TradeStationEnvironments), envir = TradeStationEnvironments);

  }

  DetermineIntervalEnvironment <- function(interval){

    env <- TradeStationEnvironments$IntervalMonth$IntervalDay;

    interval <- toupper(interval);

    if (interval %in% c('HOUR', 'H', 'HOURLY')) {
      env <- TradeStationEnvironments$IntervalHour;
    } else if (interval %in% c('D', 'DAY', 'DAILY')) {
      env <- TradeStationEnvironments$IntervalDay;
    } else if (interval %in% c('5M', '5Minute', '5m')) {
      env <- TradeStationEnvironments$Interval5Minute;
    } else if (interval %in% c('4H', '4Hour', '4HR')) {
      env <- TradeStationEnvironments$Interval4Hour;
    } else if (interval %in% c('W', 'Week', 'Wk')) {
      env <- TradeStationEnvironments$IntervalWeek;
    } else if (interval %in% c('M', 'Month', 'Mth')) {
      env <- TradeStationEnvironments$IntervalMonth;
    }

    return(env);
  }


  tradestation_path <- path;


  Data <- function(ticker, interval) {

    series <- get(ticker, envir = DetermineIntervalEnvironment(interval));
    return(series);

  }




  ProcessRawFilesForSymbolAndInterval <- function(Symbol, Interval){

    ReadTradeStationCSV2DF <- function(file){
      test <- read_csv(file, "cccccc", n_max = 1, col_names = FALSE);
      if (test$X1 == 'timestamp') {
        df <- read_csv(file, ColumnTypes);
        names(df) <- ColumnNames;
      } else {
        df <- read_csv(file, col_types = "cddddd", col_names = ColumnNames)
      }
      return(df);
    }

    FilePath <- file.path(path, Symbol, Interval);
    files <- FileSystem$Files(FilePath, pattern = "*.csv", use.regex = FALSE);

    if (length(files) > 0) {
      files <- files[order(files)];

      df <- ldply(files, ReadTradeStationCSV2DF);


      if (Validate){
        cat(deparse(match.call()), "\n");
        assert_that(sum(duplicated(df$timestamp)) == 0);
      }

      filename <- file.path(path, Symbol, paste0(Symbol, "-", Interval, ".csv"));

      write_csv(df, filename);

      # To validate, all we need to do is read the file back and see
      # if it succeeded
      if (Validate){
        ReadTradeStationCSV2Zoo2XTS(filename, Symbol, Interval);
      }


    }

    TRUE;

  }


  # ProcessAllRawFiles will be called by automated data initialization
  # and if successful, must return TRUE

  ProcessAllRawFiles <- function(){


    symbols <- FileSystem$Folders(path, recursive = FALSE, full.names = FALSE);
    intervals <- FileSystem$Folders(file.path(path, symbols[1]), full.names = FALSE);

    df <- expand.grid(symbols = symbols, interval = intervals, stringsAsFactors = FALSE);

    results <- alply(
      df,
      1,
      .fun = function(df){
        ProcessRawFilesForSymbolAndInterval(df$symbol, df$interval);
      }
    )

    return(!any(!unlist(results)));
  }


  CreateIntervalFolders <- function(){


    symbols <- basename(list.dirs(LocalOHLCFolder, recursive = FALSE));

    df <- expand.grid(symbols = symbols, interval = c("W", "M", "D", "4H", "H", "5m"));

    a_ply(df, 1, .fun = function(df){
      suppressWarnings(dir.create(file.path(LocalOHLCFolder, df$symbol, df$interval)));
    })


  }



  LoadDataFromLocal <- function(Symbol, Interval){

    file <- file.path(LocalOHLCFolder, Symbol, paste0(Symbol, "-", Interval, ".csv"));

    ReadTradeStationCSV2Zoo2XTS(file, Symbol, Interval);

  }


  LoadDataFromDropBox <- function(Symbol, Interval){

    FileSystem$CreateFolder(file.path(DropBoxCache, Symbol));

    filename <- file.path(Symbol, paste0(Symbol, "-", Interval, ".csv"));

    CachedFile <- file.path(DropBoxCache, filename);

    if(!FileSystem$FileExists(CachedFile)){
      DropBoxFile <- file.path(DropBoxFolder, filename);
      DropBox$Download(DropBoxFile, CachedFile);
    }


    ReadTradeStationCSV2Zoo2XTS(CachedFile, Symbol, Interval);

  }




  return(
    list(
      Data = Data,
      DetermineIntervalEnvironment = DetermineIntervalEnvironment,
      ProcessAllRawFiles = ProcessAllRawFiles,
      CreateIntervalFolders = CreateIntervalFolders,
      LoadDataFromDropBox = LoadDataFromDropBox,
      LoadDataFromLocal = LoadDataFromLocal,
      LocalOHLCFolder = LocalOHLCFolder,
      Validate = Validate
    )
  );

}


TradeStation <- .TradeStation();

#TradeStation$LoadDataFromLocal("AUDUSD", "4H")


#TradeStation$CreateIntervalFolders()

# test_data <- TradeStation$LoadDataFolder("AUDUSD", "D");
#
#TradeStation$ProcessAllRawFiles();



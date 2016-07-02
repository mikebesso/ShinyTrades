
if(!exists("App")){
  source("App.R");
}



#
# if (FALSE){
#   drop_auth(key = 'w5979dojsxoee50', secret = 'upm8kfmrmo9b5eb')
#   token <- drop_auth()
#   saveRDS(token, "droptoken.RDS");
#
# }
#




.DropBox <- function(){


  DropBoxAPI <- mmmDropBoxAPI(App$DropBoxRoot, App$DropBoxTokenFile);

  BuildScraperClipboardFilename <- function(AsOfDate){
    file.path("scrapes", "clipboard", paste0("scrape-clip-", AsOfDate, ".txt"));

  }

  BuildScraperRawFilename <- function(AsOfDate){
    file.path("scrapes", "raw", paste0("scrape-raw-", AsOfDate, ".txt"));
  }

  BuildScraperCleanFilename <- function(AsOfDate){
    file.path("scrapes", "clean", paste0("scrape-clean-", AsOfDate, ".csv"));
  }

  ClipboardFiles <- function(){
    files <- DropBoxAPI$Files("scrapes/clipboard");
    files <- files[order(files)];
  }

  GetClipboardFile <- function(AsOfDate = Calendar$CurrentWeekday()){
    filename <- BuildScraperClipboardFilename(AsOfDate);
    lines <- NULL;
    if (DropBoxAPI$Exists(filename)){
      lines <- DropBoxAPI$ReadLines(filename);
    }
    return(lines);
  }


  LevelFiles <- function(){
    files <- DropBoxAPI$Files("scrapes/clean");
  }

  BuildLevelFilename <- function(AsOfDate){
    file.path("scrapes", "clean", paste0("scrape-clean-", AsOfDate, ".csv"));
  }


  GetLevelFile <- function(AsOfDate = Calendar$CurrentWeekday()){
    filename <- BuildLevelFilename(AsOfDate);
    df <- NULL;
    if (DropBoxAPI$Exists(filename)){
      df <- DropBoxAPI$ReadCSV(filename);
    }
    return(df);
  }


  WriteScrapedRawFile <- function(lines, AsOfDate = Calendar$CurrentWeekday()){
    filename <- BuildScraperRawFilename(AsOfDate);
    DropBoxAPI$WriteLines(lines, filename);

    filename <- BuildScraperClipboardFilename(AsOfDate);
    if (!DropBoxAPI$Exists(filename)) {
      DropBoxAPI$WriteLines(lines, filename);
    }

  }


  WriteScrapedCleanFile <- function(df, AsOfDate = Calendar$CurrentWeekday()){


    DropBox$WriteCSV(df, BuildScraperCleanFilename(AsOfDate));


    if (AsOfDate == Calendar$CurrentWeekday()){

      mml <- df[,c(-1,-2)];

      timestamp <- now();
      newrow <- nrow(mml) + 1;

      mml[newrow, ] <- rep(NA, ncol(mml));
      mml[newrow, 1] = "EOF";
      mml[newrow, 2] = year(timestamp);
      mml[newrow, 3] = month(timestamp);
      mml[newrow, 4] = day(timestamp);
      mml[newrow, 5] = hour(timestamp);
      mml[newrow, 6] = minute(timestamp);
      mml[newrow, 7] = round(second(timestamp), 0);

      mml[newrow, 8:ncol(mml)] <- 0;

      DropBox$WriteCSV(mml, "mml.csv");
    }


  }



  return(
    append(
      list(
        DropBoxAPI = DropBoxAPI,
        ClipboardFiles = ClipboardFiles,
        GetClipboardFile = GetClipboardFile,
        GetLevelFile = GetLevelFile,
        WriteScrapedRawFile = WriteScrapedRawFile,
        WriteScrapedCleanFile = WriteScrapedCleanFile
      ),
      DropBoxAPI
    )

  )

}


DropBox <- .DropBox();



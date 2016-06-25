if (FALSE){
  devtools::install_github('rstudio/DT')
}


Sys.setenv(TZ = "America//Chicago");


LOCAL_MODE <- file.exists("rsconnect");





source("mmm.R");


source("App.R");
source("Calendar.R");



source("metadata.R");

source("data.R")


source("Console.R");

#source("LevelEditor.R");
#source("LevelViewer.R");

source("Indicators.R")
source("FuturesMetaData.R");
source("Scraper.R");
source("TradeStation.R");
source("Ticker.R");
source("Strategy.R");
source("StrategyXL.R");
source("Chart.R");
source("BackTest.R");
source("Calculators.R");

source("SideBar.R");



IDs = list(
  SideBar = SideBar$IDs,
  FuturesCalculator = FuturesCalculator$IDs,
  ForexCalculator = ForexCalculator$IDs,
  EquitiesCalculator = EquitiesCalculator$IDs
);




#source("server_level_editor.R");

#
#
# token <- readRDS("droptoken.RDS");
# dropbox_account <- drop_acc(token);
#
# dropbox_root <- "/Apps/ShinyTrades"



#LevelEditor <- CreateLevelEditor(LevelData$MetaData());

FuturesMetaData <- CreateFuturesMetaData();

if (interactive()){

  #Scraper$ProcessClipboard()

  # LevelsMetaData <- LevelData$MetaData();
  #
  # default_data_frame <- LevelsMetaData$DefaultAsDataFrame();
  #
  # default_data_list <- LevelsMetaData$DefaultAsList();
  #
  # md <- LevelsMetaData$MetaData()
  #
  # LevelsMetaData$GetAttributes("AssetClass")
  # LevelsMetaData$GetAttributes("AsOfDate")
  #
  # Defaults <- LevelsMetaData$Defaults()
  #
  # LevelEditor$EditorTab()


}

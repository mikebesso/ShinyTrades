source("bootstrap.R");

TradestationFolders <- FileSystem$Folders(TradeStation$OHLCFolder)

TradestationFiles <- ldply(
  FileSystem$Folders(TradeStation$OHLCFolder),
  .fun = function(f) {
    data.frame(Symbol <- basename(f), FileSystem$Files(f))
    }
);



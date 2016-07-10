.ForexMetaData <- function(file = "MetaData/ForexMetaData.csv"){


  # Initialize Everything

  message("Reading Forex Meta Data from: ", file);
  myMetaData <- read_csv(file);


  Symbols <- sort(myMetaData$Symbol);



  ##############
  # Helper Functions



  PipsPerPoint <- function(Symbol = "AUDUSD"){
    as.double(myMetaData$Pips.Per.Point[myMetaData$Symbol == Symbol]);
  }

  DefaultValue <- function(Symbol = "AUDUSD"){
    as.double(myMetaData$Default.Value[myMetaData$Symbol == Symbol]);
  }



  return(
    list(
      Symbols = Symbols,

      DefaultValue = DefaultValue,

      # These are synomous
      TicksPerPoint = PipsPerPoint,
      PipsPerPoint = PipsPerPoint


    )
  )

}


ForexMetaData <- .ForexMetaData();

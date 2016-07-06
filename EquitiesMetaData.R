.EquitiesMetaData <- function(file = "MetaData/EquitiesMetaData.csv"){


  # Initialize Everything

  message("Reading Equities Meta Data from: ", file);
  myMetaData <- read_csv(file);


  Symbols <- sort(myMetaData$Symbol);



  ##############
  # Helper Functions



  TicksPerPoint <- function(Symbol = "SPY") return(100);

  TickValue <- function(Symbol = "SPY") return(0.01);
  DefaultValue <- function(Symbol = "SPY") return(50.0);

  return(
    list(
      Symbols = Symbols,
      TicksPerPoint = TicksPerPoint,
      TickValue = TickValue,
      DefaultValue = DefaultValue

    )
  )

}


EquitiesMetaData <- .EquitiesMetaData();

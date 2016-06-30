.FuturesMetaData <- function(file = "MetaData/FuturesMetaData.csv"){


  # Initialize Everything

  message("Reading Futures Meta Data from: ", file);
  myMetaData <- read_csv(file);

  ColumnCount <- ncol(myMetaData);
  myMetaDataMonthColumns <- seq(ColumnCount-11, ColumnCount);


  myMonthSymbols <- unlist(
    lapply(
      str_split(
        names(myMetaData)[myMetaDataMonthColumns],
        fixed('.')
      ),
      head,
      1
    )
  );

  Symbols <- sort(myMetaData$Symbol);


  ##############
  # Helper Functions



  TickValue <- function(Symbol = "@ES"){
    myMetaData$Amount.Per.Tick[myMetaData$Symbol == Symbol];
  }

  TicksPerPoint <- function(Symbol = "@ES"){
    myMetaData$Ticks.Per.Point[myMetaData$Symbol == Symbol];
  }

  DefaultValue <- function(Symbol = "@ES"){
    myMetaData$Default.Value[myMetaData$Symbol == Symbol];
  }

  ChartSymbol <- function(Symbol = "@ES"){

    unlist(
      lapply(
        Symbol,
        FUN = function(Symbol){
          retVal <- Symbol;
          thisSymbol <- myMetaData[myMetaData$Symbol == Symbol,, drop = FALSE];
          if (nrow(thisSymbol) == 1){
            retVal <- thisSymbol$Charting.Symbol[thisSymbol$Symbol == Symbol];
          }
          return(retVal);
        }
      )
    )

  }


  TradeSymbol <- function(Symbol = "@ES", onDate = Sys.Date()){
    onDate <- as.Date(onDate);

    unlist(
      lapply(
        Symbol,
        FUN = function(Symbol){

          retVal <- Symbol;

          thisSymbol <- myMetaData[myMetaData$Symbol == Symbol, myMetaDataMonthColumns, drop = FALSE];

          if (nrow(thisSymbol) == 1){

            nextMonth = month(onDate)  %% 12 + 1;
            expMonth = nextMonth;

            RollOverDays <- myMetaData$Roll.Over.Days.Prior.to.Expiration[myMetaData$Symbol == Symbol];

            while (is.na(thisSymbol[, expMonth])){
              if (expMonth == 12){
                expMonth <- 1;
              } else {
                expMonth <- expMonth + 1;
              }
            }

            expDay <- as.integer(thisSymbol[, expMonth]);

            if (RollOverDays < expDay){

              if ((nextMonth == expMonth) & (day(onDate) >= expDay - RollOverDays)){

                expMonth <- expMonth %% 12 + 1;

                while (is.na(thisSymbol[, expMonth])){
                  if (expMonth == 12){
                    expMonth <- 1;
                  } else {
                    expMonth <- expMonth + 1;
                  }
                }

              }

              expYear <- year(onDate);
              if (expMonth < nextMonth){
                expYear <- expYear + 1;
              }

              retVal <- paste0(substring(Symbol, 2), myMonthSymbols[expMonth], expYear %% 100)


            } else {
              # When this happens, there is no good trade.
              retVal <- Symbol;
            }

          }

          return(retVal);
        }
      )
    );
  }




  return(
    list(
      Symbols = Symbols,
      TickValue = TickValue,
      TicksPerPoint = TicksPerPoint,
      DefaultValue = DefaultValue,
      TradeSymbol = TradeSymbol,
      ChartSymbol = ChartSymbol

    )
  )

}


FuturesMetaData <- .FuturesMetaData();




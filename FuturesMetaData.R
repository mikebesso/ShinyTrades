CreateFuturesMetaData <- function(file = "FuturesMetaData.csv"){

  myMetaData <- NULL;

  myMonthSymbols <- NULL;

  myMetaDataMonthColumns <- NULL;

  Init <- function(){

    message("Reading Futures Meta Data from: ", file);
    myMetaData <<- read.csv(file, stringsAsFactors = FALSE);

    ColumnCount <- ncol(myMetaData);
    myMetaDataMonthColumns <<- seq(ColumnCount-11, ColumnCount);


    myMonthSymbols <<- unlist(
      lapply(
        str_split(
          names(myMetaData)[myMetaDataMonthColumns],
          fixed('.')
        ),
        head,
        1
      )
    );


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




  Init();

  return(
    list(
      TradeSymbol = TradeSymbol,
      ChartSymbol = ChartSymbol

    )
  )

}



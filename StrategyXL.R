

IndicatorDataToZoo <- function(Indicator, IndexTZ = App$GetTimeZone()){
  #read.zoo(Indicator, format = "%Y-%m-%d %H:%M", tz = IndexTZ, index.column = "timestamp")

  x <- strptime(Indicator$timestamp, format = "%Y-%m-%d %H:%M", tz = App$GetTimeZone());
  df <- Indicator[2:ncol(Indicator)];
  z <- zoo(df, x);

  z
}

AddStandardIndicatorsToData <- function(Data){

  Index <- index(Data);
  IndexTZ <- indexTZ(Data);

  RangeLowToHigh <- as.vector(Data$High - Data$Low);
  RangeOpenToClose <- as.vector(abs(Data$Open - Data$Close));
  PercentBody <- ifelse(RangeLowToHigh != 0, RangeOpenToClose / RangeLowToHigh, 0);
  Boring <- PercentBody < 0.6;
  Exciting <- PercentBody > 0.6;

  Indicator <- data.frame(
    timestamp = Index,
    BarNumber = seq(1, length(Index)),
    Boring = Boring,
    Exciting = Exciting
  );

  IndicatorDataToZoo(Indicator, IndexTZ);

}

AddXLToData <- function(Data, Symbol) {

  Index <- index(Data);
  IndexTZ <- indexTZ(Data);

  MaxHigh <- max(Data[, "High"]) * 1.2;
  MinLow <- min(Data[, "Low"]) * 0.8;

  LevelsZoo <- Scraper$Data(Symbol);

  ep <- Calendar$EndPointsDaily(Index, "07:00");
  sp <- (ep + 1)[-length(ep)];
  ep <- ep[-1];
  n <- 0;

  Indicator <- ldply(
    1:length(ep),
    function(x) {
      tsz <- Data[sp[x]:ep[x]];
      n <- nrow(tsz);

      df <- data.frame(
        timestamp = index(tsz),
        xl_SD3 = MaxHigh, xl_SP3 = MaxHigh, xl_SD2 = MaxHigh, xl_SP2 = MaxHigh, xl_SD1 = MaxHigh, xl_SP1 = MaxHigh,
        xl_DP1 = MinLow, xl_DD1 = MinLow, xl_DP2 = MinLow, xl_DD2 = MinLow, xl_DP3 = MinLow, xl_DD3 = MinLow
      );
      date_str <- paste0(as.Date(Index[ep[x]]));
      ti <- LevelsZoo[date_str];

      if (nrow(ti) > 0) {
        df$xl_SD3 <- rep(LevelsZoo[date_str]$SD3, n);
        df$xl_SP3 <- rep(LevelsZoo[date_str]$SP3, n);
        df$xl_SD2 <- rep(LevelsZoo[date_str]$SD2, n);
        df$xl_SP2 <- rep(LevelsZoo[date_str]$SP2, n);
        df$xl_SD1 <- rep(LevelsZoo[date_str]$SD1, n);
        df$xl_SP1 <- rep(LevelsZoo[date_str]$SP1, n);
        df$xl_DP1 <- rep(LevelsZoo[date_str]$DP1, n);
        df$xl_DD1 <- rep(LevelsZoo[date_str]$DD1, n);
        df$xl_DP2 <- rep(LevelsZoo[date_str]$DP2, n);
        df$xl_DD2 <- rep(LevelsZoo[date_str]$DD2, n);
        df$xl_DP3 <- rep(LevelsZoo[date_str]$DP3, n);
        df$xl_DD3 <- rep(LevelsZoo[date_str]$DD3, n);
      }

      df;
    }
  );



  retval <- list(
    zoo = IndicatorDataToZoo(Indicator, IndexTZ),
    df = Indicator
  );

  return(
    retval
  )


}


AddPrximityBandsToData <- function(Data){

  Index <- index(Data);
  IndexTZ <- indexTZ(Data);

  BB <- BBands(Data[,c("High", "Low", "Close")], n = 20, sd = 0.5);

  Indicator <- data.frame(
    timestamp = Index,
    ProximityUp = BB$up,
    ProximityDown = BB$dn
  );

  IndicatorDataToZoo(Indicator, IndexTZ);

}

CreateOpenTransaction <- function(Price, ProfitTarget, StopLoss){

  Risk = abs(StopLoss - Price);
  Reward = abs(ProfitTarget - Price);

  if (Risk > 0) {
    RewardRiskRatio <- round(Reward / Risk, 1);
  } else {
    Risk <- 0;
  }

  # Need to calculate size based on risk
  Size = 100;


  list(
    Direction = sign(ProfitTarget - StopLoss),
    Size = Size,
    Price = Price,
    ProfitTarget = ProfitTarget,
    StopLoss = StopLoss,
    Risk = Risk,
    Reward = Reward,
    RewardRiskRatio = RewardRiskRatio,

    CurrentPositionSize = Size,
    CurrentPositionPrice = Price
  )

}


CreateCloseTransaction <- function(Size, Price){


  list(
    Direction = sign(Size),
    Size = Size,
    Price = Price,
    ProfitTarget = 0.0,
    StopLoss = 0.0,
    Risk = 0.0,
    Reward = 0.0,
    RewardRiskRatio = 0.0,

    CurrentPositionSize = 0,
    CurrentPositionPrice = 0.0
  )

}


AddTransactionsToData <- function(Data, XLs){

  Index <- index(Data);
  IndexTZ <- indexTZ(Data);
  BarCount <- length(Index);

  Highs <- as.vector(Data[, "High"]);
  Lows <- as.vector(Data[, "Low"]);



  df <- data_frame(
    timestamp = Index,

    xl_D1_Busted = rep(FALSE, BarCount),
    xl_D2_Busted = rep(FALSE, BarCount),
    xl_D3_Busted = rep(FALSE, BarCount),

    xl_S1_Busted = rep(FALSE, BarCount),
    xl_S2_Busted = rep(FALSE, BarCount),
    xl_S3_Busted = rep(FALSE, BarCount),

    CurrentPositionSize = rep(0.0, BarCount),
    CurrentPositionPrice = rep(0.0, BarCount),

    Trans_Direction = rep(0, BarCount),
    Trans_Size = rep(0, BarCount),
    Trans_Price = rep(0.0, BarCount),
    Trans_ProfitTarget = rep(0,0, BarCount),
    Trans_StopLoss = rep(0.0, BarCount),

    Trans_Risk = rep(0.0, BarCount),
    Trans_Reward = rep(0.0, BarCount),
    Trans_RewardRiskRatio = rep(0.0, BarCount)
  );


  for(Bar in 2:BarCount){

    # Have our daily levels expired?
    ExpiresAt <- Index[Bar];
    hour(ExpiresAt) <- 7
    minute(ExpiresAt) <- 0;

    Expired <- ((Index[Bar - 1] < ExpiresAt) && (Index[Bar] > ExpiresAt));


    # If yesterday's levels are not yet expired, carry over "busted"

    if (!Expired){
      df$xl_D1_Busted[Bar] <- df$xl_D1_Busted[Bar - 1] || Lows[Bar] < XLs$xl_DD1[Bar];
      df$xl_D2_Busted[Bar] <- df$xl_D2_Busted[Bar - 1] || Lows[Bar] < XLs$xl_DD2[Bar];
      df$xl_D3_Busted[Bar] <- df$xl_D3_Busted[Bar - 1] || Lows[Bar] < XLs$xl_DD3[Bar];
      df$xl_S1_Busted[Bar] <- df$xl_S1_Busted[Bar - 1] || Highs[Bar] > XLs$xl_SD1[Bar];
      df$xl_S2_Busted[Bar] <- df$xl_S2_Busted[Bar - 1] || Highs[Bar] > XLs$xl_SD1[Bar];
      df$xl_S3_Busted[Bar] <- df$xl_S3_Busted[Bar - 1] || Highs[Bar] > XLs$xl_SD1[Bar];
    }


    # Carry over current position
    df$CurrentPositionSize[Bar] <- df$CurrentPositionSize[Bar - 1];
    df$CurrentPositionPrice[Bar] <- df$CurrentPositionPrice[Bar - 1];


    # Can we enter a trade?

    BarHour <- hour(Index);
    CanEnterTrade <- df$CurrentPositionSize[Bar - 1] == 0 && (BarHour > 8 && BarHour < 14);


    # If we can enter a trade, then see if we have a good one
    # Else if we are in a trade, see if we should get out

    Trans <- NA;

    if (CanEnterTrade) {

      if (!df$xl_D3_Busted[Bar] && (Lows[Bar] < XLs$xl_DP3[Bar])) {
        Trans <- CreateOpenTransaction(Price = Lows[Bar], ProfitTarget = Lows[Bar] + (3.0 * (Lows[Bar] - XLs$xl_DD3[Bar])), StopLoss = XLs$xl_DD3[Bar]);
      } else if (!df$xl_D3_Busted[Bar] && (Lows[Bar] < XLs$xl_DP2[Bar])) {
        Trans <- CreateOpenTransaction(Price = Lows[Bar], ProfitTarget = Lows[Bar] + (3.0 * (Lows[Bar] - XLs$xl_DD2[Bar])), StopLoss = XLs$xl_DD2[Bar]);
      } else if (!df$xl_D1_Busted[Bar] && (Lows[Bar] < XLs$xl_DP1[Bar])) {
        Trans <- CreateOpenTransaction(Price = Lows[Bar], ProfitTarget = Lows[Bar] + (3.0 * (Lows[Bar] - XLs$xl_DD2[Bar])), StopLoss = XLs$xl_DD2[Bar]);
      }

    }

    # If we cannot enter a trade, we might be in a trade.
    # Check to see if we should close any current trades

    if (!CanEnterTrade) {

      if (df$CurrentPositionSize[Bar] < 0) {
        if (Lows[Bar] < df$Trans_ProfitTarget[Bar]) {
          Trans <- CreateCloseTransaction(-df$CurrentPositionSize[Bar], Lows[Bar]);
        } else if (Highs[Bar] < df$Trans_StopLoss[Bar]) {
          Trans <- CreateCloseTransaction(-df$CurrentPositionSize[Bar], Highs[Bar]);
        }
      }

      if (df$CurrentPositionSize[Bar] > 0) {
        if (Highs[Bar] > df$Trans_ProfitTarget[Bar]) {
          Trans <- CreateCloseTransaction(-df$CurrentPositionSize[Bar], Highs[Bar]);
        } else if (Lows[Bar] < df$Trans_StopLoss[Bar]) {
          Trans <- CreateCloseTransaction(-df$CurrentPositionSize[Bar], Lows[Bar]);
        }
      }
    }

    if (HasNonFalseValue(Trans)){

      df$CurrentPositionSize[Bar] <- Trans$CurrentPositionSize;
      df$CurrentPositionPrice[Bar] <- Trans$CurrentPositionPrice;

      df$Trans_Direction[Bar] <- Trans$Direction;

      df$Trans_Size[Bar] <- Trans$Size;
      df$Trans_Price[Bar] <- Trans$Price;

      df$Trans_ProfitTarget[Bar] <- Trans$ProfitTarget;
      df$Trans_StopLoss[Bar] <- Trans$StopLoss;

      df$Trans_Risk[Bar] <- Trans$Risk;
      df$Trans_Reward[Bar] <- Trans$Reward;
      df$Trans_RewardRiskRatio[Bar] <- Trans$RewardRiskRatio;

    }

    # Is this still needed?
    if (df$CurrentPositionSize[Bar] == 0){
      df$Trans_ProfitTarget[Bar] <- 0;
      df$Trans_StopLoss[Bar] <- 0;
    }

  }

  df$xl_D1_Busted <- as.double(df$xl_D1_Busted);
  df$xl_D2_Busted <- as.double(df$xl_D2_Busted);
  df$xl_D3_Busted <- as.double(df$xl_D3_Busted);

  df$xl_S1_Busted  <- as.double(df$xl_S1_Busted);
  df$xl_S2_Busted  <- as.double(df$xl_S2_Busted);
  df$xl_S3_Busted  <- as.double(df$xl_S3_Busted);


  IndicatorDataToZoo(df, IndexTZ);

}

CreateTransactionLog <- function(StrategyData){

  TransactionData <- StrategyData[StrategyData$Trans_Size != 0];

  if (length(index(TransactionData) > 0)){

    TransactionLog <- data_frame(
      ID = 1:length(index(TransactionData)),
      timestamp = as.character(index(TransactionData)),
      OpenOrClose = ifelse(as.vector(TransactionData$Trans_Direction) > 0, "Open", "Close"),
      BuyOrSell = ifelse(as.vector(TransactionData$Trans_Size) > 0, "Buy", "Sell"),
      Size = as.vector(TransactionData$Trans_Size),
      Price = as.vector(TransactionData$Trans_Price)
    )
  } else {
    TransactionLog <- data_frame(
      ID = as.integer(),
      timestamp = character(),
      OpenOrClose = character(),
      BuyOrSell = character(),
      Size = as.double(),
      Price = as.double()
    )
  }

  return(TransactionLog);
}


StrategyXL <- function(symbol = "SPY", interval = "H", provider = "TradeStation", source = "Local"){

  StrategyName <- "XL";

  Symbol <- symbol;
  Interval <- interval;

  StrategyTicker <- Ticker(Symbol, Interval, provider = provider, source = source);


  StrategyData <- StrategyTicker$Data;

  Standard <- AddStandardIndicatorsToData(StrategyData);
  XLs <- AddXLToData(StrategyData, Symbol);
  ProximityBands <- AddPrximityBandsToData(StrategyData);
  Trans <- AddTransactionsToData(StrategyData, XLs$df);





  StrategyData <- merge(
    StrategyData,
    Standard,
    XLs$zoo,
    ProximityBands,
    Trans,
    all = TRUE
  );



  TransactionLog <- CreateTransactionLog(StrategyData);


  return(
    list(
      Data = StrategyData,
      Transactions = TransactionLog
    )
  );
}



if (FALSE){
  source("bootstrap.R")

  LOCAL_MODE <- FALSE;

  XL <- StrategyXL("SPY", interval = "H");



  View(tail(XL$Data, 500));

  Chart(
    XL$Data,
    start_date = ymd("2016-04-01"),
    indicators = list(
      volume = ArgsVolume(),
      xl = ArgsXL(),
      macd = ArgsMACD(fast = 12, slow = 26, signal = 9)
    )
  );
}




Ticker <- function(symbol, interval, provider = "tradestation", source = "local") {


  Symbol <- str_to_upper(symbol);
  Interval <- interval;
  Provider <- str_to_lower(provider);
  Source <- str_to_lower(source);

  Indicators <- list();
  Data <- NULL;

  Data.OHLC <- function() {
    Data[, 1:4]
  }

  Data.XL <- function(x) {

    subset(
      Data,
      c(
        "xl_SD3", "xl_SP3", "xl_SD2", "xl_SP2", "xl_SD1", "xl_SP1",
        "xl_DP1", "xl_DD1", "xl_DP2", "xl_DD2", "xl_DP3", "xl_DD3"
      )
    );
  }

  # Indicators

  XL <- NULL;


  # AddXL <- newTA(
  #   Data.XL,
  #   col = c('purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue')
  # );


  AddXLToChartData <- function(data) {


    High <- max(data[, "High"]) * 1.2;
    Low <- min(data[, "Low"]) * 0.8;

    LevelsZoo <- Scraper$Data(Symbol);

    ep <- Calendar$EndPointsDaily(index(data), "07:00");
    sp <- (ep + 1)[-length(ep)];
    ep <- ep[-1];
    n <- 0;

    df <- ldply(
      1:length(ep),
      function(x) {
        tsz <- data[sp[x]:ep[x]];
        n <- nrow(tsz);

        df <- data.frame(
          timestamp = index(tsz),
          xl_SD3 = High, xl_SP3 = High, xl_SD2 = High, xl_SP2 = High, xl_SD1 = High, xl_SP1 = High,
          xl_DP1 = Low, xl_DD1 = Low, xl_DP2 = Low, xl_DD2 = Low, xl_DP3 = Low, xl_DD3 = Low
        );
        date_str <- paste0(as.Date(index(data)[ep[x]]));
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

        df
      }
    );

    df_zoo <- read.zoo(df, format = "%Y-%m-%d %H:%M", tz = indexTZ(data), index.column = "timestamp");


    # XL  <<- merge(Data, df_zoo, all.x = TRUE);
    # Data <<- XL;

    Indicators <<- append(Indicators, list('xl'));

    df_zoo;
  }


  AddStandardIndicators <- function() {


    if (!('xl' %in% Indicators) & (Interval %in% c('D', 'H', '4H', '5M', '5m'))) {
      AddXLToChartData();
    }



  }

  ZoomString <- function(start_date = NULL, end_date = NULL){

    start_date <- iif(is.null(start_date), min(index(Data)), start_date);
    end_date <- iif(is.null(end_date), max(index(Data)), end_date);

    retval <- NULL;
    if (HasValue(start_date)) {
      retval <- paste0(start_date, "::")
      if (HasValue(end_date)) {
        retval <- paste0(retval, end_date)
      }
    } else if (HasValue(end_date)) {
      retval <- paste0("::", end_date);
    }
    retval
  }

  Zoom <- function(start_date = NULL, end_date = NULL) {

    ZoomSubset <- ZoomString(start_date, end_date);

    if (HasValue(ZoomSubset)) {
      zoomChart(ZoomSubset);
    }

  }


  BuildAddBBands <- function(n, sd, show = TRUE){
    if (show){
      return(paste0("addBBands(n = ", n, ", sd = ", sd, ")"));
    } else {
      return(NULL);
    }
  }


  BuildAddMCAD <- function(fast, slow, signal, show = TRUE){

    if (show){
      return(paste0("addMACD(fast = ", fast, ", slow = ", slow, ", signal = ", signal, ")"));
    } else {
      return(NULL);
    }
  }




  Chart <- function(start_date = NULL, end_date = NULL, indicators = list()) {


    ## need to use subset parameter so that we get complete time frame

    ## need to add XL data to chart series

    # TA = paste(
    #   "add(
    #     XL[,c('xl_SD3', 'xl_SP3', 'xl_SD2', 'xl_SP2', 'xl_SD1', 'xl_SP1', 'xl_DP1', 'xl_DD1', 'xl_DP2', 'xl_DD2', 'xl_DP3', 'xl_DD3')],
    #     col = c('purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue', 'purple', 'blue')
    #   )",
    #   "addVo()",
    #   sep = ";"
    # )

    TA <-
      paste(
        iif(indicators$xl$show, "addXL()", NULL),
        iif(indicators$volume$show, "addVo()", NULL),
        BuildAddBBands(indicators$bb$n, indicators$bb$sd, indicators$bb$show),
        BuildAddMCAD(indicators$mcad$fast, indicators$mcad$slow, indicators$mcad$signal, indicators$mcad$show),
        sep = "; "
    );

    myCHOB <- chartSeries(Data, name = Symbol, plot = FALSE, TA = TA, subset = ZoomString(start_date, end_date));



    myCHOB@passed.args$TA[[1]]@params$legend <- NULL;
    myCHOB@passed.args$TA[[1]]@params$legend.name <- NULL;

    quantmod:::chartSeries.chob(myCHOB);


  }


  AddTransactions <- function(Buys, Sells){

    if (length(Buys$Bar) > 0) {
      show(
        addPoints(
          y = Buys$Price,
          x = Buys$Bar,
          pch='B',
          col='red'
        )
      );
    }

    if (length(Sells$Bar) > 0) {
      show(
        addPoints(
          y = Sells$Price,
          x = Sells$Bar,
          pch='S',
          col='blue'
        )
      );
    }
  }


  AddCumulativeReturn <- function(result){

    cr <- tail(result, 1)[[1]];
    cr <- round(100 * (cr - 1), 0);

    legend <- paste0('Cumulative Return: ', cr, '%');

    show(
      addTA(
        result,
        legend = legend
      )
    )

  }




  if (Provider == "tradestation") {
    if (Source == "dropbox") {
      Data <- TradeStation$LoadDataFromDropBox(Symbol, Interval)
    } else {
      Data <- TradeStation$LoadDataFromLocal(Symbol, Interval)
    }
  }


  return(
    list(
      Chart = Chart,
      Data = Data,
      AddTransactions = AddTransactions,
      AddCumulativeReturn = AddCumulativeReturn
    )
  );

}



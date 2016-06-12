
ZoomString <- function(Data, start_date = NULL, end_date = NULL){

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


ArgsVolume <- function(show = TRUE){
  TA <- "addVo()";
  Show <- show;
  return(
    list(
      Show = Show,
      TA = TA
    )
  )
}

ArgsXL <- function(show = TRUE){
  TA <- "addXL()";
  Show <- show;
  return(
    list(
      Show = Show,
      TA = TA
    )
  )
}


ArgsBB <- function(n = 20, sd = 2, show = TRUE){
  N <- n;
  SD <- sd;
  Show <- show;

  TA <- paste0("addBB(n = ", N, ", sd = ", SD, ")");

  return(
    list(
      N = N,
      SD = SD,
      Show = Show,
      TA = TA
    )
  )
}

ArgsMACD <- function(fast = 12, slow = 26, signal = 9, show = TRUE){
  Fast <- fast;
  Slow <- slow;
  Signal <- signal;
  Show <- show;

  TA <- paste0("addMACD(fast = ", fast, ", slow = ", slow, ", signal = ", signal, ")");

  return(
    list(
      Fast = Fast,
      Slow = Slow,
      Signal = Signal,
      Show = Show,
      TA = TA
    )
  )
}

BuildAddMACD <- function(fast, slow, signal, show = TRUE){
  if (show){
    return(paste0("addMACD(fast = ", fast, ", slow = ", slow, ", signal = ", signal, ")"));
  } else {
    return(NULL);
  }
}


AppendTA <- function(TAs, Indicator){
  if (HasValue(Indicator)){
    if (HasNonFalseValue(Indicator$Show))
    {
      if (str_length(TAs) > 0){
        TAs <- paste(TAs, Indicator$TA, sep = ";");
      } else {
        TAs <- Indicator$TA;
      }
    }
  }
  return(TAs);
}

BuildTAs <- function(indicators){
  TAs <- "";

  TAs <- AppendTA(TAs, indicators$xl);
  TAs <- AppendTA(TAs, indicators$volume);

  TAs <- AppendTA(TAs, indicators$bb);
  TAs <- AppendTA(TAs, indicators$macd);

  return(TAs);

}


Chart <- function(Data, start_date = NULL, end_date = NULL, indicators = list()) {


  Symbol <- "Symbol";


  TA <- BuildTAs(indicators);







  myCHOB <- chartSeries(Data, name = Symbol, plot = FALSE, TA = TA, subset = ZoomString(Data, start_date, end_date), theme = chartTheme("white"));


  for (ta in 1:length(myCHOB@passed.args$TA)){
    myCHOB@passed.args$TA[[ta]]@params$legend <- NULL;
    myCHOB@passed.args$TA[[ta]]@params$legend.name <- NULL;

    if (myCHOB@passed.args$TA[[ta]]@call == "addXL()"){
      myCHOB@passed.args$TA[[ta]]@params$yrange <- c(
        min(myCHOB@passed.args$TA[[ta]]@TA.values[, "Low"]) * 0.98,
        max(myCHOB@passed.args$TA[[ta]]@TA.values[, "High"]) * 1.02
      )
    }
  }

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







BB_Strategy_Generate<- function(
  ticker,
  strategy_args = list(enabled = FALSE, use_macd = FALSE, max_hold_days = 30, stop_profit = TRUE, trailing_stop_percent = 0),
  bb_args = list(n = 14, sd = 2),
  macd_args = list(fast = 12, slow = 26, signal = 9)
){

  assert_that(bb_args$n > 3, bb_args$sd > 0.5);

  ################## Checking Validity #############################################
  if(strategy_args$max_hold_days<=0){stop("The maximum holding days has to be positive")}
  if(strategy_args$use_macd & strategy_args$trailing_stop_percent == 0){stop("Specify stop sell trig for modifying with MACD")}
  if(strategy_args$trailing_stop_percent==0){strategy_args$trailing_stop_percent = NA}
  ###################################################################################

  # If we have an adjusted closing price, use it instead
  if ("adjusted" %in% names(ticker$Data)){
    Close = ticker$Data[,"adjusted"];
  }
  else {
    Close = ticker$Data[, "close"];
  }



  # Get BB
  Data <- merge(
    Close,
    BBands(Close, n = bb_args$n, sd = bb_args$sd)$pctB
  );

  Data$Holding <- FALSE;
  Data$Buy <- FALSE;
  Data$Sell <- FALSE;


  # Get MACD
  ti_macd <- MACD(Close[,1], nFast = macd_args$fast, nSlow = macd_args$slow, nSig = macd_args$signal);
  ti_macd$hist <- ti_macd$macd - ti_macd$signal;
  ti_macd$hist[which(is.na(ti_macd$hist))] <- 0;

  if (strategy_args$use_macd) {
    macd = macd_idx1(ti_macd$hist);
  } else {
    macd <- NA;
    macd_id <- NA;
  }

  BuyThisBar <- 0;
  SellThisBar <- 0;

  if (strategy_args$enabled & strategy_args$use_macd){
    start = max((macd_slow + macd_signal-1), bb_args$n)
  } else if(strategy_args$enabled) {
    start <- bb_args$n
  } else {
    start <- 1;
  }
  end <- length(index(Data));

  Buy <- LogiclVector(length = end);
  Sell <- LogiclVector(length = end);

  Data$Holding <- FALSE;



  cost <- Data[[start,1]];
  days <- 0;
  for(i in start:end){
    pctB <- Data$pctB[[i]];

    if (strategy_args$use_macd){
      macd_id <- Data$macd[[i]];
    }


    if(!Data$Holding[[i]]){
      SellThisBar <- FALSE;
      BuyThisBar  <- sig2Buy1(pctB, macd_id);

      ############pctB is NA at the begginning;
      ##Same thing doesn't need in SellThisBar because no holding before BuyThisBar == TRUE!!!!
      if(is.na(BuyThisBar)) {
        BuyThisBar <- FALSE
      }
      ############

      if (BuyThisBar) {

        cost <- Data[[i,1]];
        days <- days + 1;
        Buy[[i]] <- Data[[i,1]]
        if (i < end) {
          Data$Holding[[i+1]] <- TRUE
        }
      }
    }
    else {
      BuyThisBar <- FALSE;
      Sell_bb <- sig2Sell1(pctB, macd_id)
      current <- Data[[i,1]]
      Sell_stop <- sig2Stop(
        current=current,
        cost=cost,
        trailing_stop_percent = strategy_args$trailing_stop_percent,
        macd_id= macd_id,
        days=days,
        max_hold_days = strategy_args$max_hold_days
      );

      if(strategy_args$stop_profit){
        SellThisBar <- Sell_bb | Sell_stop
      } else {
        SellThisBar <- Sell_stop
      }

      if (SellThisBar) {
        Sell[[i]] <- Data[[i,1]];
        days <- 0;
        cost <- 0;
      } else if(!SellThisBar) {
        days <- days + 1;
        cost <- max(Data[[i,1]], cost);
        if (i < end) {
          Data$Holding[[i+1]] <- TRUE
        }
      }
    }




  }


  # Calculate cumulative return


  bar_count <- length(index(Data))
  penultimate_bar <- bar_count - 1;

  M <- cbind(
    # Price
    as.vector(Data[,1][-bar_count]),

    # Holding, shifted right
    as.vector(Data$Holding[-1]),

    #Holding
    as.vector(Data$Holding[-bar_count])
  );

  M[,2] <- M[,1] * M[,2];
  M[,3] <- M[,1] * M[,3];

  B <- M[,2];
  B <- B[-penultimate_bar];
  B <- c(0, B);
  M[,2] <- B
  M[which(M[,2] == 0), 2] <- NA;
  M[which(M[,3] == 0), 3] <- NA;
  M[,1] <- (M[,3] - M[,2]) / M[,2];
  B <- M[,1];
  B[which(is.na(B))] <- 0;
  M[,1] <- B;
  d_ret <- c(0, M[,1]) + 1;
  c_rst <- rep(1, bar_count);

  for(i in 2:bar_count){
    c_rst[i] <- c_rst[i-1] * d_ret[i]
  }

  d_ret= xts(d_ret, order.by= index(Data))
  result= xts(c_rst, order.by= index(Data))

  BuyBars <- which(Buy != 0);
  Buys <- data.frame(Bar = BuyBars, Price = as.vector(Data$close[BuyBars]));

  SellBars <- which(Sell != 0);
  Sells <- data.frame(Bar = SellBars, Price = as.vector(Data$close[SellBars]));


  Strategy <-
    list(
      Data = Data,
      Buys = Buys,
      Sells = Sells,
      result = result
    );

  return(Strategy);




}

############## Function used in generating order ##########################


macd_idx1 <- function(MACD){
  cut_off1 <- function(num){
    max(min(num, 1), -1)
  };

  A <- apply(MACD, 1, cut_off1);

  A <- xts(A, order.by= index(MACD));

  colnames(A) <- 'hist';

  return(A)
}

# With BB and MACD at a particular day, the following function generate buying signal
sig2Buy1 <- function(pctB, macd_id){

  Buy <- FALSE;

  if (!is.na(pctB)){
    Buy <- ifelse(is.na(macd_id), pctB < 0, pctB < macd_id * 0.3)
  }

  return(Buy);

}

# With BB and MACD at a particular day, the following function generate selling signal
sig2Sell1 <- function(pctB, macd_id){

  Sell <- FALSE;

  if (!is.na(pctB)){
    Sell <- ifelse(is.na(macd_id), pctB > 1, pctB > (1 + (macd_id * 0.3)));
  }

  return(Sell);
}
## combine all stopping rule
sig2Stop <- function(current, cost, trailing_stop_percent, macd_id, days, max_hold_days){
  return(
    sig2Stop1(current, cost, trailing_stop_percent, macd_id) | sig2Stop2(days, max_hold_days)
  )
}

## Stopping rule 1 ####
sig2Stop1 <- function(current, cost, trailing_stop_percent, macd_id){

  change <- (current - cost) / cost;

  if (is.na(trailing_stop_percent)){
    return(FALSE)
  } else if (is.na(macd_id)) {
    ss = -trailing_stop_percent
  } else {
    ss= -((trailing_stop_percent / 0.02) * (0.02 * macd_id + 0.03) * (2^macd_id))
  }
  return (change < ss)
}

## Stopping rule 2 ####
sig2Stop2 <- function(days, max_hold_days){
  return(days >= max_hold_days)
}

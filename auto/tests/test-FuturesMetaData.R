

test_that(
  "FuturesMetaData TradeSymbol", {


    symbols <- c("@ES", "@NQ", "@CL");

    trade_symbol <- FuturesMetaData$TradeSymbol("@CL", ymd("2016-04-20"));

    trade_symbols <- FuturesMetaData$TradeSymbol(symbols);

    expect_that(
      length(trade_symbols),
      equals(length(symbols))
    )

  }
)

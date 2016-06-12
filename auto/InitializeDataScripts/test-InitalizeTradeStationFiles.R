




test_that(
  "TradeStationFiles", {

    expect_that(
      TradeStation$ProcessAllRawFiles(),
      equals(TRUE)
    )

  }
)





#
# test_that(
#   "TradeStation Unique Indices", {
#
#     Symbols <- c("SPY", "IWB", "USDCAD");
#     Intervals <- c("5m", "H", "4H", "D", "W", "M");
#     df <- expand.grid(Symbol = Symbols, Interval = Intervals);
#
#
#
#     LoadDataFromLocal()
#
#   }
#)

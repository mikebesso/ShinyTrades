.ForexCalculator <- function(){


  PageName <- "FOREX Calculator";
  TabName <- "ForexCalculator";

  myIDs <- list(
    "idForexCalculatorInputs",
    "idForexLotSize",
    "idForexMaxRisk",
    "idForexEntry",
    "idForexStopLoss",
    "idForexStopLossPips",
    "idForexProfitTarget",
    "idForexProfitTargetPips",
    "idForexPipValue",
    "idForexPositionSize"

  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(

        Box$Input(
          title = "What",
          width = 12,

          selectInput(
            myIDs$idForexPair,
            "Currency Pair",
            choices = list("EURUSD", "CADUSD")
          ),
          selectInput(
            myIDs$idForexLotSize,
            "Lot Size",
            choices = list(
              Standard = "100,000",
              Mini = "10,000",
              Micro = "1,000"
            )
          )
        )
      ),

      fluidRow(

        Box$Input(
          title = "Entry",
          width = 6,

          selectInput(myIDs$idForexBuyOrSell, "Buy Or Sell", choices = list("Buy", "Sell")),
          numericInput(myIDs$idForexEntry, "Entry", 0),
          numericInput(myIDs$idForexMaxRisk, "Max Risk ($)", 0)
        ),

        Box$Input(
          title = "Exit",
          width = 6,

          numericInput(myIDs$idForexStopLoss, "Stop Loss", 0),
          numericInput(myIDs$ForexStopLossPips, "Stop Loss (PIPs)", 10),

          numericInput(myIDs$idForexProfitTarget, "Profit Target", 0),
          numericInput(myIDs$idForexProfitTargetPips, "Profit Target (PIPs)", 0)
        )

      ),

      fluidRow(
        Box$Result(
          id = myIDs$idForexPositionSize
        )

      )

    );



  Server <- function(input, output, session){


    output[[myIDs$idForexPositionSize]] <-
      Box$Render(
        {
          browser();
          Box$ResultHTML(value = 10, subtitle = "lots");
        }
      );

    # dataInput <- reactive({
    #
    #   result = list();
    #
    #   #data = Ticker(input$symb, "H", provider = input[[IDs$SideBar$idTickerProvider]], source = input[[IDs$SideBar$idTickerSource]]);
    #
    #   result$data <- StrategyXL(
    #     "SPY",
    #     interval = "H",
    #     provider = input[[IDs$SideBar$idTickerProvider]],
    #     source = input[[IDs$SideBar$idTickerSource]]
    #   );
    #
    #
    #   return(result);
    # });
    #
    #
    # Calculators_refresh <- eventReactive(
    #   input$Calculators_refresh,
    #   {
    #     list(
    #       symbol = input$symb,
    #       bb_args =  list(n = input$BB_win, sd = input$sd, show = input$addBB),
    #       mcad_args = list(fast = input$macd_fast, slow = input$macd_slow, signal = input$macd_signal, show = input$addMACD),
    #       strategy_args = list(
    #         enabled = input$processed,
    #         use_macd = input$modi_macd,
    #         max_hold_days = input$stop_day,
    #         stop_profit = input$stop_profit,
    #         trailing_stop_percent = input$stop_trig
    #       )
    #     )
    #   }
    # );
    #
    # output$plot <- renderPlot({
    #
    #   args <- Calculators_refresh();
    #
    #   Symbol <- args$symbol;
    #   data <- dataInput()$data;
    #
    #
    #   Chart(
    #     data$Data,
    #     start_date = input$zoom[1],
    #     end_date = input$zoom[2],
    #     indicators = list(
    #       volume = ArgsVolume(),
    #       xl = ArgsXL(),
    #       macd = ArgsMACD(fast = args$mcad_args$fast, slow = args$mcad_args$slow, signal= args$mcad_args$signal, show = args$mcad_args$show),
    #       bb = ArgsBB(show = args$bb_args$show, n = args$bb_args$n, sd = args$bb_args$sd)
    #     )
    #   );
    #
    #
    #
    # });
    #
    #
    # output$idTransLog <- renderGvis({
    #   data <- dataInput()$data;
    #
    #   Log <- data$Transactions;
    #
    #   gvisTable(Log, formats = list(Price = "#,###.####")) ;
    #
    #
    # })

  };


  return(
    list(
      IDs = myIDs,
      UI.Page = UI.Page,
      UI.MenuItem = UI.MenuItem,
      Server = Server
    )
  );


}



.FuturesCalculator <- function(){


  PageName <- "Futures Calculator";
  TabName <- "FuturesCalculator";

  myIDs <- list(
    "idFuturesCalculatorInputs",
    "idFuturesLotSize",
    "idFuturesMaxRisk",
    "idFuturesEntry",
    "idFuturesStopLoss",
    "idFuturesStopLossPips",
    "idFuturesProfitTarget",
    "idFuturesProfitTargetPips",
    "idFuturesPipValue"

  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(
        tabBox(
          width = 12,
          id = myIDs$idCalculatorInputs,

          tabPanel(
            title = "Futures",
            value = "FuturesCalculator",

            Panel$Input(
              title = "What",
              # width = 4,

              selectInput(myIDs$idFuturesPair, "Currency Pair", choices = list("EURUSD", "CADUSD")),
              selectInput(myIDs$idFuturesLotSize, "Lot Size", choices = list(Standard = "100,000", Mini = "10,000", Micro = "1,000"))
            ),

            Panel$Input(
              title = "Entry",
              # width = 4,

              selectInput(myIDs$idFuturesBuyOrSell, "Buy Or Sell", choices = list("Buy", "Sell")),
              numericInput(myIDs$idFuturesEntry, "Entry", 0),
              numericInput(myIDs$idFuturesMaxRisk, "Max Risk ($)", 0)
            ),

            Panel$Input(
              title = "Exit",
              # width = 4,

              numericInput(myIDs$idFuturesStopLoss, "Stop Loss", 0),
              numericInput(myIDs$FuturesStopLossPips, "Stop Loss (PIPs)", 10),

              numericInput(myIDs$idFuturesProfitTarget, "Profit Target", 0),
              numericInput(myIDs$idFuturesProfitTargetPips, "Profit Target (PIPs)", 0)
            )
            #  ,
            #
            # infoBox("Position Size", value = 100, width = 4)

          ),

          tabPanel(
            title = "Equities",
            value = "EquitiesCalculator",
            box(
              title = "Account"
            )
          ),
          tabPanel(
            title = "Futures",
            value = "FuturesCalculator",
            box(
              title = "Account"
            )
          )

        )
      )
    );


  Server <- function(input, output, session){


    # dataInput <- reactive({
    #
    #   result = list();
    #
    #   #data = Ticker(input$symb, "H", provider = input[[IDs$SideBar$idTickerProvider]], source = input[[IDs$SideBar$idTickerSource]]);
    #
    #   result$data <- StrategyXL(
    #     "SPY",
    #     interval = "H",
    #     provider = input[[IDs$SideBar$idTickerProvider]],
    #     source = input[[IDs$SideBar$idTickerSource]]
    #   );
    #
    #
    #   return(result);
    # });
    #
    #
    # Calculators_refresh <- eventReactive(
    #   input$Calculators_refresh,
    #   {
    #     list(
    #       symbol = input$symb,
    #       bb_args =  list(n = input$BB_win, sd = input$sd, show = input$addBB),
    #       mcad_args = list(fast = input$macd_fast, slow = input$macd_slow, signal = input$macd_signal, show = input$addMACD),
    #       strategy_args = list(
    #         enabled = input$processed,
    #         use_macd = input$modi_macd,
    #         max_hold_days = input$stop_day,
    #         stop_profit = input$stop_profit,
    #         trailing_stop_percent = input$stop_trig
    #       )
    #     )
    #   }
    # );
    #
    # output$plot <- renderPlot({
    #
    #   args <- Calculators_refresh();
    #
    #   Symbol <- args$symbol;
    #   data <- dataInput()$data;
    #
    #
    #   Chart(
    #     data$Data,
    #     start_date = input$zoom[1],
    #     end_date = input$zoom[2],
    #     indicators = list(
    #       volume = ArgsVolume(),
    #       xl = ArgsXL(),
    #       macd = ArgsMACD(fast = args$mcad_args$fast, slow = args$mcad_args$slow, signal= args$mcad_args$signal, show = args$mcad_args$show),
    #       bb = ArgsBB(show = args$bb_args$show, n = args$bb_args$n, sd = args$bb_args$sd)
    #     )
    #   );
    #
    #
    #
    # });
    #
    #
    # output$idTransLog <- renderGvis({
    #   data <- dataInput()$data;
    #
    #   Log <- data$Transactions;
    #
    #   gvisTable(Log, formats = list(Price = "#,###.####")) ;
    #
    #
    # })

  };


  return(
    list(
      IDs = myIDs,
      UI.Page = UI.Page,
      UI.MenuItem = UI.MenuItem,
      Server = Server
    )
  );


}



.EquitiesCalculator <- function(){


  PageName <- "Equities Calculator";
  TabName <- "EquitiesCalculator";

  myIDs <- list(
    "idEquitiesCalculatorInputs",
    "idEquitiesLotSize",
    "idEquitiesMaxRisk",
    "idEquitiesEntry",
    "idEquitiesStopLoss",
    "idEquitiesStopLossPips",
    "idEquitiesProfitTarget",
    "idEquitiesProfitTargetPips",
    "idEquitiesPipValue"

  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      PageName,

      fluidRow(
        tabBox(
          width = 12,
          id = myIDs$idCalculatorInputs,

          tabPanel(
            title = "Equities",
            value = "EquitiesCalculator",

            Panel$Input(
              title = "What",
              # width = 4,

              selectInput(myIDs$idEquitiesPair, "Currency Pair", choices = list("EURUSD", "CADUSD")),
              selectInput(myIDs$idEquitiesLotSize, "Lot Size", choices = list(Standard = "100,000", Mini = "10,000", Micro = "1,000"))
            ),

            Panel$Input(
              title = "Entry",
              # width = 4,

              selectInput(myIDs$idEquitiesBuyOrSell, "Buy Or Sell", choices = list("Buy", "Sell")),
              numericInput(myIDs$idEquitiesEntry, "Entry", 0),
              numericInput(myIDs$idEquitiesMaxRisk, "Max Risk ($)", 0)
            ),

            Panel$Input(
              title = "Exit",
              # width = 4,

              numericInput(myIDs$idEquitiesStopLoss, "Stop Loss", 0),
              numericInput(myIDs$EquitiesStopLossPips, "Stop Loss (PIPs)", 10),

              numericInput(myIDs$idEquitiesProfitTarget, "Profit Target", 0),
              numericInput(myIDs$idEquitiesProfitTargetPips, "Profit Target (PIPs)", 0)
            )
            #  ,
            #
            # infoBox("Position Size", value = 100, width = 4)

          ),

          tabPanel(
            title = "Equities",
            value = "EquitiesCalculator",
            box(
              title = "Account"
            )
          ),
          tabPanel(
            title = "Futures",
            value = "FuturesCalculator",
            box(
              title = "Account"
            )
          )

        )
      )
    );


  Server <- function(input, output, session){


    # dataInput <- reactive({
    #
    #   result = list();
    #
    #   #data = Ticker(input$symb, "H", provider = input[[IDs$SideBar$idTickerProvider]], source = input[[IDs$SideBar$idTickerSource]]);
    #
    #   result$data <- StrategyXL(
    #     "SPY",
    #     interval = "H",
    #     provider = input[[IDs$SideBar$idTickerProvider]],
    #     source = input[[IDs$SideBar$idTickerSource]]
    #   );
    #
    #
    #   return(result);
    # });
    #
    #
    # Calculators_refresh <- eventReactive(
    #   input$Calculators_refresh,
    #   {
    #     list(
    #       symbol = input$symb,
    #       bb_args =  list(n = input$BB_win, sd = input$sd, show = input$addBB),
    #       mcad_args = list(fast = input$macd_fast, slow = input$macd_slow, signal = input$macd_signal, show = input$addMACD),
    #       strategy_args = list(
    #         enabled = input$processed,
    #         use_macd = input$modi_macd,
    #         max_hold_days = input$stop_day,
    #         stop_profit = input$stop_profit,
    #         trailing_stop_percent = input$stop_trig
    #       )
    #     )
    #   }
    # );
    #
    # output$plot <- renderPlot({
    #
    #   args <- Calculators_refresh();
    #
    #   Symbol <- args$symbol;
    #   data <- dataInput()$data;
    #
    #
    #   Chart(
    #     data$Data,
    #     start_date = input$zoom[1],
    #     end_date = input$zoom[2],
    #     indicators = list(
    #       volume = ArgsVolume(),
    #       xl = ArgsXL(),
    #       macd = ArgsMACD(fast = args$mcad_args$fast, slow = args$mcad_args$slow, signal= args$mcad_args$signal, show = args$mcad_args$show),
    #       bb = ArgsBB(show = args$bb_args$show, n = args$bb_args$n, sd = args$bb_args$sd)
    #     )
    #   );
    #
    #
    #
    # });
    #
    #
    # output$idTransLog <- renderGvis({
    #   data <- dataInput()$data;
    #
    #   Log <- data$Transactions;
    #
    #   gvisTable(Log, formats = list(Price = "#,###.####")) ;
    #
    #
    # })

  };


  return(
    list(
      IDs = myIDs,
      UI.Page = UI.Page,
      UI.MenuItem = UI.MenuItem,
      Server = Server
    )
  );


}

EquitiesCalculator <- .EquitiesCalculator();
FuturesCalculator <- .FuturesCalculator();
ForexCalculator <- .ForexCalculator();

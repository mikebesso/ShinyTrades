.ForexCalculator <- function(){


  PageName <- "FOREX Calculator";
  TabName <- "ForexCalculator";

  myIDs <- list(
    "idForexCalculatorInputs",
    "idForexPair",
    "idForexBuyOrSell",
    "idForexLotSize",
    "idForexMaxRisk",
    "idForexEntry",
    "idForexStopLoss",
    "idForexStopLossPips",
    "idForexProfitTarget",
    "idForexProfitTargetPips",
    "idForexPipValue",
    "idForexPositionSize",
    "idForexPositionSizeBox",
    "idForexInPips",
    "idForexPipValueBox",
    "idForexRisk"
  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(

        Box$Input(
          title = "What",
          width = 6,

          selectInput(
            myIDs$idForexPair,
            "Currency Pair",
            choices = list("EURUSD", "USDCAD", "AUDUSD", "GBPUSD"),
            selected = "EURUSD"
          ),
          selectInput(
            myIDs$idForexLotSize,
            "Lot Size",
            choices = list(
              Standard = "100000",
              Mini = "10000",
              Micro = "1000"
            ),
            selected = "1000"
          )
        ),
        Box$Input(
          title = "Account",
          width = 6,

          numericInput(myIDs$idForexMaxRisk, label = "Max Risk ($)", value = 200, step = 25, min = 50, width = "50%"),
          checkboxInput(myIDs$idForexInPips, "In PIPS", value = TRUE)
        )

      ),

      fluidRow(


        Box$Input(
          title = "Entry",
          width = 4,

          selectInput(myIDs$idForexBuyOrSell, "Buy Or Sell", choices = list(Buy = 1, Sell = -1), width = "50%"),
          numericInput(myIDs$idForexEntry, "Price", 1.0, width = "50%", step = 0.0005)
        ),

        Box$Input(
          title = "Exit (PIPS)",
          width = 4,

          numericInput(myIDs$idForexStopLossPips, label = "Stop Loss (PIPs)", value = 25, width = "50%"),
          numericInput(myIDs$idForexProfitTargetPips, label = "Profit Target (PIPs)", value = 75, width = "50%")
        ),


        Box$Input(
          title = "Exit",
          width = 4,

          disabled(numericInput(myIDs$idForexStopLoss, label = "Stop Loss", value = 25, width = "50%")),
          disabled(numericInput(myIDs$idForexProfitTarget, label = "Profit Target", value = 75, width = "50%"))
        )

      ),

      fluidRow(
        Box$Result(
          id = myIDs$idForexPositionSizeBox
        ),
        Box$Result(
          id = myIDs$idForexPipValueBox
        )
      ),

      fluidRow(
        conditionalPanel(
          condition = myIDs$idDebug,
          Box$Input(
            title = "Debug Variables",
            numericInput(myIDs$idForexPipValue, label = "PIP Value", value = 10, width = "50%"),
            numericInput(myIDs$idForexPositionSize, label = "Position Size", value = 10, width = "50%"),
            numericInput(myIDs$idForexRisk, label = "Risk", value = 10, width = "50%")

          )
        )
      )

    );



  Server <- function(input, output, session){

    reUsePips <- reactive({
        input[[myIDs$idForexInPips]];
    });

    reCalculator <- reactive({

        UsePips <- reUsePips();

        Pip <-  parse_double(isolate(input[[myIDs$idForexPipValue]]));
        BuyOrSell = parse_integer(isolate(input[[myIDs$idForexBuyOrSell]]));

        Entry <- parse_double(input[[myIDs$idForexEntry]]);

        MaxRisk <- parse_double(input[[myIDs$idForexMaxRisk]]);

        if (UsePips) {
          StopLossPips <-  parse_double(input[[myIDs$idForexStopLossPips]]);
          ProfitTargetPips <- parse_double(input[[myIDs$idForexProfitTargetPips]]);
        } else {
          StopLoss <-  parse_double(input[[myIDs$idForexStopLoss]]);
          ProfitTarget <- parse_double(input[[myIDs$idForexProfitTarget]]);
        }


        if (UsePips) {
          if ((StopLossPips > 0) || (ProfitTargetPips > 0)) {
            StopLoss <- Entry - (BuyOrSell * StopLossPips / 1000);
            ProfitTarget <- Entry + (BuyOrSell * ProfitTargetPips / 1000);
            Risk <- StopLossPips * Pip;

          } else {
            StopLoss <- -1;
            ProfitTarget <- -1;
            Risk <- -1;
          }
        } else {
          if ((StopLoss > 0) || (ProfitTarget > 0)) {
            StopLossPips <- abs(Entry - StopLoss) / Pip;
            ProfitTargetPips <- abs(Entry - ProfitTarget) / Pip;
            Risk <- abs(Entry - StopLoss) * 1000 * Pip;
          } else {
            StopLossPips <- -1;
            ProfitTargetPips <- -1;
            Risk <- -1;
          }
        }

        if ((Risk > MaxRisk) && (MaxRisk > 0)){
          PositionSize <- MaxRisk / Risk;
        } else {
          PositionSize <- 0;
        }


      return(
        list(
          UsePips = UsePips,
          Pip =  Pip,
          BuyOrSell = BuyOrSell,

          Entry = Entry,

          MaxRisk = MaxRisk,
          Risk = Risk,

          StopLossPips = StopLossPips,
          StopLoss = StopLoss,
          ProfitTargetPips = ProfitTargetPips,
          ProfitTaraget = ProfitTarget,
          PositionSize = PositionSize
        )
      )
    });



    observe(
      {
        UsePips <- reUsePips();

        if (UsePips){
          enable(myIDs$idForexStopLossPips);
          enable(myIDs$idForexProfitTargetPips);
          disable(myIDs$idForexStopLoss);
          disable(myIDs$idForexProfitTarget);
        } else {
          enable(myIDs$idForexStopLoss);
          enable(myIDs$idForexProfitTarget);
          disable(myIDs$idForexStopLossPips);
          disable(myIDs$idForexProfitTargetPips);
        }
      }
    )

    observe(
      {

        ForexPair <- input[[myIDs$idForexPair]];
        Entry <- parse_double(input[[myIDs$idForexEntry]]);
        LotSize <- parse_double(input[[myIDs$idForexLotSize]]);

        Pip <- NA;

        if (LotSize > 0) {
          if (str_detect(ForexPair, 'USD$')){
            Pip <- LotSize / 10000.0
          } else  if (Entry > 0) {
            if (str_detect(ForexPair, '^USD')){
              Pip <- (LotSize / 10000.0) / Entry;
            }
            # need to figure out how to handle non US pairs
          }

          if (!is.na(Pip)){
            updateNumericInput(session, myIDs$idForexPipValue, value = Pip);
          }

        }
      }
    );


    # Update Pip Value Box
    observe(
      {

        # Get value
        Pip <- round(input[[myIDs$idForexPipValue]], 2);

        # Put value into box
        output[[myIDs$idForexPipValueBox]] <-
          Box$Render(
            {
              Box$ResultHTML(value = Pip,
                             title = "PIP",
                             subtitle = "Value",
                             icon = Icon$thumbs$up);
            }
          );

      }
    );



    # Update Position Size Box
    observe(
      {

        # Get value
        Size <- round(input[[myIDs$idForexPositionSize]], 0);

        # Put value into box
        output[[myIDs$idForexPositionSizeBox]] <-
          Box$Render(
            {
              Box$ResultHTML(value = Size,
                             title = "Size",
                             subtitle = "Lots",
                             icon = Icon$thumbs$up);
            }
          );
      }
    )


    observe({

      Calcs <- reCalculator();

      if (!Calcs$UsePips) {
        updateNumericInput(session, myIDs$idForexStopLossPips, value = Calcs$StopLossPips);
        updateNumericInput(session, myIDs$idForexProfitTargetPips, value = Calcs$ProfitTargetPips);
      } else {
        updateNumericInput(session, myIDs$idForexStopLoss, value = Calcs$StopLoss);
        updateNumericInput(session, myIDs$idForexProfitTarget, value = Calcs$ProfitTarget);
      }
      updateNumericInput(session, myIDs$idForexPositionSize, value = Calcs$PositionSize);
      updateNumericInput(session, myIDs$idForexRisk, value = Calcs$Risk);

    });

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

              selectInput(myIDs$idFuturesPair, "Currency Pair", choices = list("EURUSD", "USDCAD", "AUDUSD", "GBPUSD")),
              selectInput(myIDs$idFuturesLotSize, "Lot Size", choices = list(Standard = "100000", Mini = "10000", Micro = "1000"))
            ),

            Panel$Input(
              title = "Entry",
              # width = 4,

              selectInput(myIDs$idFuturesBuyOrSell, "Buy Or Sell", choices = list("Buy", "Sell")),
              numericInput(myIDs$idFuturesEntry, "Entry", 1.0000),
              numericInput(myIDs$idFuturesMaxRisk, "Max Risk ($)", 200)
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
              selectInput(myIDs$idEquitiesLotSize, "Lot Size", choices = list(Standard = "100000", Mini = "10000", Micro = "1000"))
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

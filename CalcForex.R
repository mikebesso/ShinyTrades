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
    "idForexProfitTarget",

    "idForexPositionSize",
    "idForexPositionSizeBox",
    "idForexInTicks",
    "idForexTicksPerPoint",
    "idForexTickValue",
    "idForexTickValueBox",
    "idForexRewardRiskRatio",
    "idForexRewardRiskRatioBox",
    "idForexRisk",

    "idForexStopLossInternal",
    "idForexProfitTargetInternal",
    "idForexStopLossTicksInternal",
    "idForexProfitTargetTicksInternal"

  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(
        Box$Result(
          id = myIDs$idForexPositionSizeBox
        ),
        Box$Result(
          id = myIDs$idForexRewardRiskRatioBox
        ),
        Box$Result(
          id = myIDs$idForexTickValueBox
        )
      ),



      fluidRow(

        Box$Input(
          title = "What",
          width = 6,

          selectInput(
            myIDs$idForexPair,
            "Currency Pair",
            choices = ForexMetaData$Symbols,
            selected = ForexMetaData$Symbols[1]
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

          numericInput(myIDs$idForexMaxRisk, label = "Max Risk ($)", value = 200, step = 25, min = 50, width = "50%")
        )

      ),

      fluidRow(

        Box$Input(
          title = "Entry",
          width = 6,

          selectInput(myIDs$idForexBuyOrSell, "Buy Or Sell", choices = list(Buy = 1, Sell = -1), width = "50%"),
          numericInput(myIDs$idForexEntry, "Price", 1.0, width = "50%", step = 0.0005)
        ),

        Box$Input(
          title = "Exit",
          width = 6,

          checkboxInput(myIDs$idForexInTicks, "In Pips", value = TRUE),
          numericInput(myIDs$idForexStopLoss, label = "Stop Loss", value = 25, step = 5, width = "50%"),
          numericInput(myIDs$idForexProfitTarget, label = "Profit Target", value = 75, step = 5, width = "50%")

        )

      ),


      fluidRow(
        conditionalPanel(
          condition = "input.idSuperUser",
          Box$Input(
            title = "Debug Variables",
            numericInput(myIDs$idForexTickValue, label = "Tick Value", value = 10, width = "50%"),
            numericInput(myIDs$idForexTicksPerPoint, label = "Ticks Per Point", value = ForexMetaData$TicksPerPoint(), width = "50%"),
            numericInput(myIDs$idForexPositionSize, label = "Position Size", value = 10, width = "50%"),
            numericInput(myIDs$idForexRisk, label = "Risk", value = 10, width = "50%"),
            numericInput(myIDs$idForexRewardRiskRatio, label = "Risk Reward Ratio", value = 3, width = "50%"),


            numericInput(myIDs$idForexStopLossInternal, label = "Stop Loss", value = 0.9975, width = "50%"),
            numericInput(myIDs$idForexProfitTargetInternal, label = "Profit Target", value = 1.0075, width = "50%"),
            numericInput(myIDs$idForexStopLossTicksInternal, label = "Stop Loss (Ticks)", value = 25, width = "50%"),
            numericInput(myIDs$idForexProfitTargetTicksInternal, label = "Profit Target (Ticks)", value = 75, width = "50%")

          )
        )
      )

    );



  UpdateResultBoxes <- function(output, calcs){

    PositionSize <- round(calcs$PositionSize, 0);
    Entry <- round(calcs$Entry, 4);
    StopLoss <- round(calcs$StopLoss, 4);
    ProfitTarget <- round(calcs$ProfitTarget, 4);
    RewardRiskRatio <- round(calcs$RewardRiskRatio, 2);
    Tick <- round(calcs$Tick, 2);


    # Put value into box
    output[[myIDs$idForexPositionSizeBox]] <-
      Box$Render(
        {
          Box$TradeHTML(
            PositionSize = PositionSize,
            Entry = FormatForex(Entry),
            StopLoss = FormatForex(StopLoss),
            ProfitTarget = FormatForex(ProfitTarget)
          )
        }
      );

    output[[myIDs$idForexTickValueBox]] <-
      Box$Render(
        {
          Box$ResultHTML(value = Tick,
                         title = "Tick",
                         subtitle = "Value",
                         icon = Icon$currency$dollar);
        }
      );

    if (RewardRiskRatio > 2.5){
      color = "green";
      icon = Icon$thumbs$up;
    } else {
      color = "red";
      icon = Icon$thumbs$down;
    }

    # Put value into box
    output[[myIDs$idForexRewardRiskRatioBox]] <-
      Box$Render(
        {
          Box$ResultHTML(value = RewardRiskRatio,
                         title = "Risk Reward",
                         subtitle = "Ratio",
                         color = color,
                         icon = icon);
        }
      );

  }

  Server <- function(input, output, session){

    reUseTicks <- reactive({
      input[[myIDs$idForexInTicks]];
    });


    reCalculator <- reactive({

      CalculatePosition(
        UseTicks = isolate(reUseTicks()),

        BuyOrSell = parse_integer(input[[myIDs$idForexBuyOrSell]]),
        TickValue = parse_double(input[[myIDs$idForexTickValue]]),
        TicksPerPoint = parse_double(input[[myIDs$idForexTicksPerPoint]]),
        Entry = parse_double(input[[myIDs$idForexEntry]]),
        MaxRisk = parse_double(input[[myIDs$idForexMaxRisk]]),

        InputStopLoss = parse_double(input[[myIDs$idForexStopLoss]]),
        InputProfitTarget = parse_double(input[[myIDs$idForexProfitTarget]])
      );


    });


    ###################
    # Observes
    ###################


    # Update exit fields based on whether or not we are using Ticks

    observe(
      {
        UseTicks <- reUseTicks();


        if (UseTicks) {
          StopLoss <- parse_double(isolate(input[[myIDs$idForexStopLossTicksInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idForexProfitTargetTicksInternal]]));
          Step <- 5;

        } else {
          StopLoss <- parse_double(isolate(input[[myIDs$idForexStopLossInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idForexProfitTargetInternal]]));

          Step = isolate(input[[myIDs$idForexTickValue]]) * 5;

        }

        updateNumericInput(session, myIDs$idForexStopLoss, value = StopLoss, step = );
        updateNumericInput(session, myIDs$idForexProfitTarget, value = ProfitTarget, step = );

      },
      priority = 10000
    );


    observe(
      {

        ForexPair <- input[[myIDs$idForexPair]];
        Entry <- parse_double(input[[myIDs$idForexEntry]]);
        LotSize <- parse_double(input[[myIDs$idForexLotSize]]);

        TicksPerPoint <- ForexMetaData$TicksPerPoint(ForexPair);

        DefaultValue <- ForexMetaData$DefaultValue(ForexPair);
        if (abs(1 - (abs(DefaultValue - Entry) / Entry)) > 0.25){
          Entry <- DefaultValue;
          updateNumericInput(session, myIDs$idForexEntry, value = DefaultValue);
        }



        TickValue <- NA;

        if (LotSize > 0) {
          if (str_detect(ForexPair, 'USD$')){
            TickValue <- LotSize / TicksPerPoint
          } else  if (Entry > 0) {
            if (str_detect(ForexPair, '^USD')){
              TickValue <- (LotSize / TicksPerPoint) / Entry;
            }
            # need to figure out how to handle non US pairs
          }

          updateNumericInput(session, myIDs$idForexTicksPerPoint, value = TicksPerPoint);

          if (!is.na(TickValue)){
            updateNumericInput(session, myIDs$idForexTickValue, value = TickValue);
          }

        }
      },
      priority = 1000
    );






    observe(
      {

        Calcs <- reCalculator();

        # Update state from calculations

        updateNumericInput(session, myIDs$idForexPositionSize, value = Calcs$PositionSize);
        updateNumericInput(session, myIDs$idForexRisk, value = Calcs$Risk);
        updateNumericInput(session, myIDs$idForexRewardRiskRatio, value = Calcs$RiskRewardRatio);

        updateNumericInput(session, myIDs$idForexStopLossTicksInternal, value = Calcs$StopLossTicks);
        updateNumericInput(session, myIDs$idForexProfitTargetTicksInternal, value = Calcs$ProfitTargetTicks);
        updateNumericInput(session, myIDs$idForexStopLossInternal, value = Calcs$StopLoss);
        updateNumericInput(session, myIDs$idForexProfitTargetInternal, value = Calcs$ProfitTarget);


        UpdateResultBoxes(output, Calcs);

      },
      priority = 10
    );

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

ForexCalculator <- .ForexCalculator();

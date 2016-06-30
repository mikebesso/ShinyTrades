.FuturesCalculator <- function(){


  PageName <- "Futures Calculator";
  TabName <- "FuturesCalculator";

  myIDs <- list(
    "idFuturesCalculatorInputs",
    "idFuturesSymbol",
    "idFuturesBuyOrSell",
    "idFuturesMaxRisk",
    "idFuturesEntry",

    "idFuturesStopLoss",
    "idFuturesProfitTarget",

    "idFuturesPositionSize",
    "idFuturesPositionSizeBox",
    "idFuturesInTicks",
    "idFuturesTicksPerPoint",
    "idFuturesTickValue",
    "idFuturesTickValueBox",
    "idFuturesRewardRiskRatio",
    "idFuturesRewardRiskRatioBox",
    "idFuturesRisk",

    "idFuturesStopLossInternal",
    "idFuturesProfitTargetInternal",
    "idFuturesStopLossTicksInternal",
    "idFuturesProfitTargetTicksInternal"
  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(
        Box$Result(
          id = myIDs$idFuturesPositionSizeBox
        ),
        Box$Result(
          id = myIDs$idFuturesRewardRiskRatioBox
        ),
        Box$Result(
          id = myIDs$idFuturesTickValueBox
        )
      ),



      fluidRow(

        Box$Input(
          title = "What",
          width = 6,

          selectInput(
            myIDs$idFuturesSymbol,
            "Futures Symbol",
            choices = FuturesMetaData$Symbols,
            selected = FuturesMetaData$Symbols[1]
          )
        ),
        Box$Input(
          title = "Account",
          width = 6,

          numericInput(myIDs$idFuturesMaxRisk, label = "Max Risk ($)", value = 200, step = 25, min = 50, width = "50%")
        )

      ),

      fluidRow(

        Box$Input(
          title = "Entry",
          width = 6,

          selectInput(myIDs$idFuturesBuyOrSell, "Buy Or Sell", choices = list(Buy = 1, Sell = -1), width = "50%"),
          numericInput(myIDs$idFuturesEntry, "Price", 1.0, width = "50%", step = 0.0005)
        ),

        Box$Input(
          title = "Exit",
          width = 6,

          checkboxInput(myIDs$idFuturesInTicks, "In Ticks", value = TRUE),
          numericInput(myIDs$idFuturesStopLoss, label = "Stop Loss", value = 25, width = "50%"),
          numericInput(myIDs$idFuturesProfitTarget, label = "Profit Target", value = 75, width = "50%")

        )

      ),


      fluidRow(
        conditionalPanel(
          condition = "input.idSuperUser",
          Box$Input(
            title = "Debug Variables",
            numericInput(myIDs$idFuturesTickValue, label = "Tick Value", value = 10, width = "50%"),
            numericInput(myIDs$idFuturesTicksPerPoint, label = "Ticks Per Point", value = FuturesMetaData$TicksPerPoint(), width = "50%"),
            numericInput(myIDs$idFuturesPositionSize, label = "Position Size", value = 10, width = "50%"),
            numericInput(myIDs$idFuturesRisk, label = "Risk", value = 10, width = "50%"),
            numericInput(myIDs$idFuturesRewardRiskRatio, label = "Risk Reward Ratio", value = 3, width = "50%"),


            numericInput(myIDs$idFuturesStopLossInternal, label = "Stop Loss", value = 0.9975, width = "50%"),
            numericInput(myIDs$idFuturesProfitTargetInternal, label = "Profit Target", value = 1.0075, width = "50%"),
            numericInput(myIDs$idFuturesStopLossTicksInternal, label = "Stop Loss (Ticks)", value = 25, width = "50%"),
            numericInput(myIDs$idFuturesProfitTargetTicksInternal, label = "Profit Target (Ticks)", value = 75, width = "50%")

          )
        )
      )

    );



  UpdateResultBoxes <- function(output, calcs){

    PositionSize <- round(calcs$PositionSize, 2);
    Entry <- round(calcs$Entry, 2);
    StopLoss <- round(calcs$StopLoss, 2);
    ProfitTarget <- round(calcs$ProfitTarget, 2);
    RewardRiskRatio <- round(calcs$RewardRiskRatio, 2);
    TickValue <- round(calcs$TickValue, 2);


    # Put value into box
    output[[myIDs$idFuturesPositionSizeBox]] <-
      Box$Render(
        {
          Box$TradeHTML(
            PositionSize = PositionSize,
            Entry = FormatFutures(Entry),
            StopLoss = FormatFutures(StopLoss),
            ProfitTarget = FormatFutures(ProfitTarget)
          )
        }
      );

    output[[myIDs$idFuturesTickValueBox]] <-
      Box$Render(
        {
          Box$ResultHTML(value = TickValue,
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
    output[[myIDs$idFuturesRewardRiskRatioBox]] <-
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
      input[[myIDs$idFuturesInTicks]];
    });


    reCalculator <- reactive({

      CalculatePosition(
        UseTicks = isolate(reUseTicks()),

        BuyOrSell = parse_integer(input[[myIDs$idFuturesBuyOrSell]]),
        TickValue = parse_double(input[[myIDs$idFuturesTickValue]]),
        TicksPerPoint = parse_double(input[[myIDs$idFuturesTicksPerPoint]]),
        Entry = parse_double(input[[myIDs$idFuturesEntry]]),
        MaxRisk = parse_double(input[[myIDs$idFuturesMaxRisk]]),

        InputStopLoss = parse_double(input[[myIDs$idFuturesStopLoss]]),
        InputProfitTarget = parse_double(input[[myIDs$idFuturesProfitTarget]])
      )

    });


    ###################
    # Observes
    ###################


    # Update exit fields based on whether or not we are using Ticks

    observe(
      {
        UseTicks <- reUseTicks();

        if (UseTicks) {
          StopLoss <- parse_double(isolate(input[[myIDs$idFuturesStopLossTicksInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idFuturesProfitTargetTicksInternal]]));
        } else {
          StopLoss <- parse_double(isolate(input[[myIDs$idFuturesStopLossInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idFuturesProfitTargetInternal]]));
        }

        updateNumericInput(session, myIDs$idFuturesStopLoss, value = StopLoss);
        updateNumericInput(session, myIDs$idFuturesProfitTarget, value = ProfitTarget);

      },
      priority = 10000
    );


    observe(
      {
        FuturesSymbol <- input[[myIDs$idFuturesSymbol]];
        Entry <- isolate(parse_double(input[[myIDs$idFuturesEntry]]));

        TickValue <- FuturesMetaData$TickValue(FuturesSymbol);
        TicksPerPoint <- FuturesMetaData$TicksPerPoint(FuturesSymbol);

        DefaultValue <- FuturesMetaData$DefaultValue(FuturesSymbol);

        if (abs(1 - (abs(DefaultValue - Entry) / Entry)) > 0.25){
          Entry <- DefaultValue;
          updateNumericInput(session, myIDs$idFuturesEntry, value = DefaultValue);
        }

        updateNumericInput(session, myIDs$idFuturesTicksPerPoint, value = TicksPerPoint);

        if (!is.na(TickValue)){
          updateNumericInput(session, myIDs$idFuturesTickValue, value = TickValue);
        }

      },
      priority = 1000
    );




    observe(
      {

        Calcs <- reCalculator();

        # Update state from calculations

        updateNumericInput(session, myIDs$idFuturesPositionSize, value = Calcs$PositionSize);
        updateNumericInput(session, myIDs$idFuturesRisk, value = Calcs$Risk);
        updateNumericInput(session, myIDs$idFuturesRewardRiskRatio, value = Calcs$RiskRewardRatio);

        updateNumericInput(session, myIDs$idFuturesStopLossTicksInternal, value = Calcs$StopLossTicks);
        updateNumericInput(session, myIDs$idFuturesProfitTargetTicksInternal, value = Calcs$ProfitTargetTicks);
        updateNumericInput(session, myIDs$idFuturesStopLossInternal, value = Calcs$StopLoss);
        updateNumericInput(session, myIDs$idFuturesProfitTargetInternal, value = Calcs$ProfitTarget);


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

FuturesCalculator <- .FuturesCalculator();


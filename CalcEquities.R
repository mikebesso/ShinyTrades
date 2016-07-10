.EquitiesCalculator <- function(){


  PageName <- "Equities Calculator";
  TabName <- "EquitiesCalculator";

  myIDs <- list(
    "idEquitiesCalculatorInputs",
    "idEquitiesSymbol",
    "idEquitiesBuyOrSell",
    "idEquitiesMaxRisk",
    "idEquitiesEntry",

    "idEquitiesStopLoss",
    "idEquitiesProfitTarget",

    "idEquitiesPositionSize",
    "idEquitiesPositionSizeBox",
    "idEquitiesInTicks",
    "idEquitiesTicksPerPoint",
    "idEquitiesTickValue",
    "idEquitiesTickValueBox",
    "idEquitiesRewardRiskRatio",
    "idEquitiesRewardRiskRatioBox",
    "idEquitiesRisk",

    "idEquitiesStopLossInternal",
    "idEquitiesProfitTargetInternal",
    "idEquitiesStopLossTicksInternal",
    "idEquitiesProfitTargetTicksInternal"
  );
  names(myIDs) <- myIDs;


  UI.MenuItem <- menuSubItem(text = PageName, tabName = TabName);

  UI.Page <-
    tabItem(
      tabName = TabName,

      fluidRow(
        Box$Result(
          id = myIDs$idEquitiesPositionSizeBox
        ),
        Box$Result(
          id = myIDs$idEquitiesRewardRiskRatioBox
        ),
        Box$Result(
          id = myIDs$idEquitiesTickValueBox
        )
      ),



      fluidRow(

        Box$Input(
          title = "What",
          width = 6,

          selectInput(
            myIDs$idEquitiesSymbol,
            "Equities Symbol",
            choices = EquitiesMetaData$Symbols,
            selected = EquitiesMetaData$Symbols[1]
          )
        ),
        Box$Input(
          title = "Account",
          width = 6,

          numericInput(myIDs$idEquitiesMaxRisk, label = "Max Risk ($)", value = 200, step = 25, min = 50, width = "50%")
        )

      ),

      fluidRow(

        Box$Input(
          title = "Entry",
          width = 6,

          selectInput(myIDs$idEquitiesBuyOrSell, "Buy Or Sell", choices = list(Buy = 1, Sell = -1), width = "50%"),
          numericInput(myIDs$idEquitiesEntry, "Price", 50.0, width = "50%", step = 0.0005)
        ),

        Box$Input(
          title = "Exit",
          width = 6,

          checkboxInput(myIDs$idEquitiesInTicks, "In Ticks", value = FALSE),
          numericInput(myIDs$idEquitiesStopLoss, label = "Stop Loss", value = 49, width = "50%"),
          numericInput(myIDs$idEquitiesProfitTarget, label = "Profit Target", value = 55, width = "50%")

        )

      ),


      fluidRow(
        conditionalPanel(
          condition = "input.idSuperUser",
          Box$Input(
            title = "Debug Variables",
            numericInput(myIDs$idEquitiesTickValue, label = "Tick Value", value = 10, width = "50%"),
            numericInput(myIDs$idEquitiesTicksPerPoint, label = "Ticks Per Point", value = EquitiesMetaData$TicksPerPoint(), width = "50%"),
            numericInput(myIDs$idEquitiesPositionSize, label = "Position Size", value = 10, width = "50%"),
            numericInput(myIDs$idEquitiesRisk, label = "Risk", value = 10, width = "50%"),
            numericInput(myIDs$idEquitiesRewardRiskRatio, label = "Risk Reward Ratio", value = 3, width = "50%"),


            numericInput(myIDs$idEquitiesStopLossInternal, label = "Stop Loss", value = 0.9975, width = "50%"),
            numericInput(myIDs$idEquitiesProfitTargetInternal, label = "Profit Target", value = 1.0075, width = "50%"),
            numericInput(myIDs$idEquitiesStopLossTicksInternal, label = "Stop Loss (Ticks)", value = 25, width = "50%"),
            numericInput(myIDs$idEquitiesProfitTargetTicksInternal, label = "Profit Target (Ticks)", value = 75, width = "50%")

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
    output[[myIDs$idEquitiesPositionSizeBox]] <-
      Box$Render(
        {
          Box$TradeHTML(
            PositionSize = PositionSize,
            Entry = FormatEquities(Entry),
            StopLoss = FormatEquities(StopLoss),
            ProfitTarget = FormatEquities(ProfitTarget)
          )
        }
      );

    output[[myIDs$idEquitiesTickValueBox]] <-
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
    output[[myIDs$idEquitiesRewardRiskRatioBox]] <-
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
      input[[myIDs$idEquitiesInTicks]];
    });


    reCalculator <- reactive({

      CalculatePosition(
        UseTicks = isolate(reUseTicks()),
        BuyOrSell = parse_integer(input[[myIDs$idEquitiesBuyOrSell]]),
        TickValue = parse_double(input[[myIDs$idEquitiesTickValue]]),
        TicksPerPoint = parse_double(input[[myIDs$idEquitiesTicksPerPoint]]),
        Entry = parse_double(input[[myIDs$idEquitiesEntry]]),
        MaxRisk = parse_double(input[[myIDs$idEquitiesMaxRisk]]),
        InputStopLoss = parse_double(input[[myIDs$idEquitiesStopLoss]]),
        InputProfitTarget = parse_double(input[[myIDs$idEquitiesProfitTarget]])
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
          StopLoss <- parse_double(isolate(input[[myIDs$idEquitiesStopLossTicksInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idEquitiesProfitTargetTicksInternal]]));
        } else {
          StopLoss <- parse_double(isolate(input[[myIDs$idEquitiesStopLossInternal]]));
          ProfitTarget <- parse_double(isolate(input[[myIDs$idEquitiesProfitTargetInternal]]));
        }

        updateNumericInput(session, myIDs$idEquitiesStopLoss, value = StopLoss);
        updateNumericInput(session, myIDs$idEquitiesProfitTarget, value = ProfitTarget);

      },
      priority = 10000
    );


    observe(
      {
        EquitiesSymbol <- input[[myIDs$idEquitiesSymbol]];
        Entry <- isolate(parse_double(input[[myIDs$idEquitiesEntry]]));

        TickValue <- EquitiesMetaData$TickValue(EquitiesSymbol);
        TicksPerPoint <- EquitiesMetaData$TicksPerPoint(EquitiesSymbol);

        DefaultValue <- EquitiesMetaData$DefaultValue(EquitiesSymbol);

        if (abs(1 - (abs(DefaultValue - Entry) / Entry)) > 0.25){
          Entry <- DefaultValue;
          updateNumericInput(session, myIDs$idEquitiesEntry, value = DefaultValue);

          UseTicks <- isolate(reUseTicks());
          if (!UseTicks){
            updateNumericInput(session, myIDs$idEquitiesStopLoss, value = DefaultValue - 0.25);
            updateNumericInput(session, myIDs$idEquitiesProfitTarget, value = DefaultValue + 0.75);
          }
        }

        updateNumericInput(session, myIDs$idEquitiesTicksPerPoint, value = TicksPerPoint);

        if (!is.na(TickValue)){
          updateNumericInput(session, myIDs$idEquitiesTickValue, value = TickValue);
        }

      },
      priority = 1000
    );



    observe(
      {

        Calcs <- reCalculator();

        # Update state from calculations

        updateNumericInput(session, myIDs$idEquitiesPositionSize, value = Calcs$PositionSize);
        updateNumericInput(session, myIDs$idEquitiesRisk, value = Calcs$Risk);
        updateNumericInput(session, myIDs$idEquitiesRewardRiskRatio, value = Calcs$RiskRewardRatio);

        updateNumericInput(session, myIDs$idEquitiesStopLossTicksInternal, value = Calcs$StopLossTicks);
        updateNumericInput(session, myIDs$idEquitiesProfitTargetTicksInternal, value = Calcs$ProfitTargetTicks);
        updateNumericInput(session, myIDs$idEquitiesStopLossInternal, value = Calcs$StopLoss);
        updateNumericInput(session, myIDs$idEquitiesProfitTargetInternal, value = Calcs$ProfitTarget);


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

EquitiesCalculator <- .EquitiesCalculator();

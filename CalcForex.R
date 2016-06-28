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

    "idForexPositionSize",
    "idForexPositionSizeBox",
    "idForexInPips",
    "idForexPipValue",
    "idForexPipValueBox",
    "idForexRewardRiskRatio",
    "idForexRewardRiskRatioBox",
    "idForexRisk"
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
          id = myIDs$idForexPipValueBox
        )
      ),



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

          checkboxInput(myIDs$idForexInPips, "In PIPS", value = TRUE),
          numericInput(myIDs$idForexStopLoss, label = "Stop Loss", value = 25, width = "50%"),
          numericInput(myIDs$idForexProfitTarget, label = "Profit Target", value = 75, width = "50%")

        )

      ),


      fluidRow(
        conditionalPanel(
          condition = myIDs$idDebug,
          Box$Input(
            title = "Debug Variables",
            numericInput(myIDs$idForexPipValue, label = "PIP Value", value = 10, width = "50%"),
            numericInput(myIDs$idForexPositionSize, label = "Position Size", value = 10, width = "50%"),
            numericInput(myIDs$idForexRisk, label = "Risk", value = 10, width = "50%"),
            numericInput(myIDs$idForexRewardRiskRatio, label = "Risk Reward Ratio", value = 3, width = "50%"),


            numericInput(myIDs$idForexStopLossInternal, label = "Stop Loss", value = 0.9975, width = "50%"),
            numericInput(myIDs$idForexProfitTargetInternal, label = "Profit Target", value = 1.0075, width = "50%"),
            numericInput(myIDs$idForexStopLossPipsInternal, label = "Stop Loss (PIPs)", value = 25, width = "50%"),
            numericInput(myIDs$idForexProfitTargetPipsInternal, label = "Profit Target (PIPs)", value = 75, width = "50%")

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
    Pip <- round(calcs$Pip, 2);


    # Put value into box
    output[[myIDs$idForexPositionSizeBox]] <-
      Box$Render(
        {
          Box$TradeHTML(
            PositionSize = PositionSize,
            Entry = Entry,
            StopLoss = StopLoss,
            ProfitTarget = ProfitTarget
          )
        }
      );

    output[[myIDs$idForexPipValueBox]] <-
      Box$Render(
        {
          Box$ResultHTML(value = Pip,
                         title = "PIP",
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

    reUsePips <- reactive({
      input[[myIDs$idForexInPips]];
    });


    reCalculator <- reactive({

      # Isolate these

      UsePips <- isolate(reUsePips());


      BuyOrSell = parse_integer(input[[myIDs$idForexBuyOrSell]]);

      Pip <-  parse_double(input[[myIDs$idForexPipValue]]);

      Entry <- parse_double(input[[myIDs$idForexEntry]]);

      MaxRisk <- parse_double(input[[myIDs$idForexMaxRisk]]);


      InputStopLoss <- parse_double(input[[myIDs$idForexStopLoss]]);
      InputProfitTarget <- parse_double(input[[myIDs$idForexProfitTarget]]);

      if (UsePips) {
        StopLossPips <- InputStopLoss;
        ProfitTargetPips <- InputProfitTarget;
      } else {
        StopLoss <- InputStopLoss;
        ProfitTarget <- InputProfitTarget;
      }


      if (UsePips) {
        if ((StopLossPips > 0) || (ProfitTargetPips > 0)) {
          StopLoss <- Entry - (BuyOrSell * StopLossPips / 10000);
          ProfitTarget <- Entry + (BuyOrSell * ProfitTargetPips / 10000);
          Risk <- StopLossPips * Pip;

        } else {
          StopLoss <- -1;
          ProfitTarget <- -1;
          Risk <- -1;
        }
      } else {
        if ((StopLoss > 0) || (ProfitTarget > 0)) {
          StopLossPips <- abs(Entry - StopLoss) * 10000;
          ProfitTargetPips <- abs(Entry - ProfitTarget) * 10000;
          Risk <- abs(Entry - StopLoss) * 10000 * Pip;
        } else {
          StopLossPips <- -1;
          ProfitTargetPips <- -1;
          Risk <- -1;
        }
      }

      Reward <- ProfitTargetPips * Pip;

      if ((Risk < MaxRisk) && (Risk > 0)){
        PositionSize <- MaxRisk / Risk;
        RewardRiskRatio <- Reward / Risk;
      } else {
        PositionSize <- 0;
        RewardRiskRatio <- 0;
      }


      return(
        list(
          UsePips = UsePips,
          Pip =  Pip,
          BuyOrSell = BuyOrSell,

          Entry = Entry,

          MaxRisk = MaxRisk,
          Risk = Risk,
          RewardRiskRatio = RewardRiskRatio,

          StopLossPips = StopLossPips,
          StopLoss = StopLoss,
          ProfitTargetPips = ProfitTargetPips,
          ProfitTarget = ProfitTarget,
          PositionSize = PositionSize
        )
      )
    });



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




    # Update exit fields based on whether or not we are using Pips

    observe({
      UsePips <- reUsePips();

      Calcs <- isolate(reCalculator());

      if (Calcs$UsePips) {
        StopLoss <- Calcs$StopLossPips;
        ProfitTarget <- Calcs$ProfitTargetPips;
      } else {
        StopLoss <- Calcs$StopLoss;
        ProfitTarget <- Calcs$ProfitTarget;
      }

      updateNumericInput(session, myIDs$idForexStopLossPips, value = StopLoss);
      updateNumericInput(session, myIDs$idForexProfitTargetPips, value = ProfitTarget);


    })


    observe({

      Calcs <- reCalculator();

      # Update state from calculations

      updateNumericInput(session, myIDs$idForexPositionSize, value = Calcs$PositionSize);
      updateNumericInput(session, myIDs$idForexRisk, value = Calcs$Risk);
      updateNumericInput(session, myIDs$idForexRewardRiskRatio, value = Calcs$RiskRewardRatio);

      updateNumericInput(session, myIDs$idForexStopLossPipsInternal, value = Calcs$StopLossPips);
      updateNumericInput(session, myIDs$idForexProfitTargetPipsInternal, value = Calcs$ProfitTargetPips);
      updateNumericInput(session, myIDs$idForexStopLossInternal, value = Calcs$StopLoss);
      updateNumericInput(session, myIDs$idForexProfitTargetInternal, value = Calcs$ProfitTarget);


      UpdateResultBoxes(output, Calcs);

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

ForexCalculator <- .ForexCalculator();

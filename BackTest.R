.BackTest <- function(){


  PageName <- "backtest";

  UI.MenuItem <-
    menuItem(
      text = "Back Testing",
      menuSubItem(
        text = "Docs",
        icon = Icon$ui$help,
        href = "http://mikebesso.github.io/ShinyTrades/backtesting.html"
      ),
      menuSubItem(
        text = "Data Set",
        tabName = "btDataSet"
      ),
      menuSubItem(
        text = "Parameters",
        tabName = "btParameters"
      ),
      menuSubItem(
        text = "Chart",
        tabName = "btChart"
      ),
      menuSubItem(
        text = "Parameters",
        tabName = "btTrades"
      )
    );


  UI.Page <- function(){

    list(
      DocTabs$Markdown("BackTest"),
      tabItem(
        tabName = "btDataSet",
        fluidRow(
          box(
            title = "Data Set",
            width = 6,
            selectInput("symb", "Symbol", list("@ES", "@GC", "@NQ", "AUDUSD", "EURJPY", "EURUSD", "GBPUSD", "USDCAD", "USDJPY", "GLD", "IWM", "QQQ", "TLT", "USO", "SPY", "XLE", "XLF", "XLV"), selected = "SPY")
          ),

          box(
            title = "Show",
            width = 6,
            checkboxInput("processed", "Orders", value = FALSE),
            checkboxInput("addVo", "Volume", value = TRUE),
            checkboxInput("addXL", "XL", value = TRUE),
            checkboxInput("addBB", "Bollinger Bands", value = TRUE),
            checkboxInput("addMACD", "MACD", value = FALSE)
          )
        )
      ),


      tabItem(
        tabName = "btChart",
        fluidRow(
          box(
            title = 'Chart',
            width = 12,
            actionButton('backtest_refresh', 'Build Chart'),
            plotOutput('plot', height = "700px"),
            dateRangeInput(
              "zoom",
              label=h5("Zoom Chart"),
              start = "2016-01-01",
              end = as.character(Sys.Date())
            )
          )
        )
      ),


      tabItem(
        tabName = "btParameters",
          fluidRow(
            box(
              title = "Strategy Parameters",
              width = 12,
              helpText(
                "Use the box on the left below to set strategy parameters."
              )
            )
          ),
          fluidRow(
            tabBox(
              width = 6,
              id = "idParams",

              tabPanel(
                title = "Bollinger Bands",
                value = "BB",

                sliderInput("BB_win", label=h6("Bollinger's Window Size:"), min = 14, max = 60, value = 20, step=1),
                sliderInput("sd", label=h6("Multiple of Standard Deviation:"), min = 1.0, max = 3.2, value = 2.0, step=0.1)

              ),
              tabPanel(
                title = "MACD",
                value = "MACD",

                sliderInput("macd_fast", label=h6("Fast Indicator:"), min = 7, max = 60, value = 12, step=1),
                sliderInput("macd_slow", label=h6("Slow Indicator:"), min = 20, max = 150, value = 26, step=1),
                sliderInput("macd_signal", label=h6("Signal:"), min = 5, max = 60, value = 9, step=1),
                checkboxInput("modi_macd", "Modify the order by MACD (Stop sell trigger in the left column is necessary when modifying with MACD).", value = FALSE)
            ),
            tabPanel(
              title = "Exit",
              value = "Exit",
              checkboxInput("stop_profit", "Stop profit?", value = TRUE),
              numericInput("stop_day", label=h6("Max Days of Holding:"), value=1000, min =1, step = 1),
              sliderInput("stop_trig", label=h6("Stop Sell Trigger:"), min = 0, max = 0.15, value = 0.0, step=0.001)
            )
          ),
          box(
            title = "Help",
            width = 6,
            conditionalPanel(
              "input.idParams == 'MACD'",
              helpText("Our assumption of bouncing price may be invalid when trend is strong. MACD helps to
                        identify such trend and to adjust strategy accordingly. Tune the parameters below
                       to catch trends with different period.")
            ),
            conditionalPanel(
              "input.idParams == 'BB'",
              helpText("Visualize Bollinger bands and the orders generated by it. The order are generated
                       based on the assumption that the price bouncing back when it touches the edge of the
                       Bollinger band.")
              ),
            conditionalPanel(
              "input.idParams == 'Exit'",
              helpText("Explain exit strategies.")
              )
          )
        )
      ),


    tabItem(
      tabName = "btTrades",
          fluidRow(
          box(
            title = "Log",
            width = 12,

            htmlOutput("idTransLog", container = tags$div, style = "overflow:scroll;height:500px")
          )

        )


      )
    )
  };


  Server <- function(input, output, session){


    dataInput <- reactive({

      result = list();

      #data = Ticker(input$symb, "H", provider = input[[IDs$SideBar$idTickerProvider]], source = input[[IDs$SideBar$idTickerSource]]);

      result$data <- StrategyXL(
        "SPY",
        interval = "H",
        provider = input[[IDs$SideBar$idTickerProvider]],
        source = input[[IDs$SideBar$idTickerSource]]
      );


      return(result);
    });


    backtest_refresh <- eventReactive(
      input$backtest_refresh,
      {
        list(
          symbol = input$symb,
          bb_args =  list(n = input$BB_win, sd = input$sd, show = input$addBB),
          mcad_args = list(fast = input$macd_fast, slow = input$macd_slow, signal = input$macd_signal, show = input$addMACD),
          strategy_args = list(
            enabled = input$processed,
            use_macd = input$modi_macd,
            max_hold_days = input$stop_day,
            stop_profit = input$stop_profit,
            trailing_stop_percent = input$stop_trig
          )
        )
      }
    );

    output$plot <- renderPlot({

      args <- backtest_refresh();

      Symbol <- args$symbol;
      data <- dataInput()$data;
      validate(
        need(str_length(input$zoom[1]) == 10, "Date format invalid"),
        need(str_length(input$zoom[2]) == 10, "Date format invalid"),
        need(input$zoom[2] > input$zoom[1], "End date is earlier than start date"),
        need(difftime(input$zoom[2], input$zoom[1], "days") > 14, "date range less the 14 days")
      );



      Chart(
        data$Data,
          start_date = input$zoom[1],
          end_date = input$zoom[2],
          indicators = list(
            volume = ArgsVolume(),
            xl = ArgsXL(),
            macd = ArgsMACD(fast = args$mcad_args$fast, slow = args$mcad_args$slow, signal= args$mcad_args$signal, show = args$mcad_args$show),
            bb = ArgsBB(show = args$bb_args$show, n = args$bb_args$n, sd = args$bb_args$sd)
          )
      );



    });


    output$idTransLog <- renderGvis({
      data <- dataInput()$data;

      Log <- data$Transactions;

      gvisTable(Log, formats = list(Price = "#,###.####")) ;


    })

  };





  return(
    list(

      UI.Page = UI.Page,
      UI.MenuItem = UI.MenuItem,
      Server = Server
    )
  );


}



BackTest <- .BackTest();

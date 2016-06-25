.SideBar <- function(){

  IDs <-list(
    idSuperUser = "idSuperUser",
    idDebug = "idDebug",
    idTickerProvider = "idTickerProvider",
    idTickerSource = "idTickerSource",
    idSideBarMenu = "idSideBarMenu",
    idSearchButton = "idSearchButton",
    idSearchText = "idSearchText"
  );
  names(IDs) <- IDs;

  UI <-

     dashboardSidebar(
      shinyjs::useShinyjs(),
      sidebarSearchForm(textId = IDs$idSearchText, buttonId = IDs$idSearchButton, label = "Search or Command..."),

      sidebarMenu(
        id = IDs$idSideBarMenu,

        menuItem(
          text = "Position Size Calculators",
          ForexCalculator$UI.MenuItem,
          FuturesCalculator$UI.MenuItem,
          EquitiesCalculator$UI.MenuItem
        ),

        menuItem(
          text = "Back Testing",
          BackTest$UI.MenuItem
        ),

        menuItem(
          text = "Maintenance",
          Scraper$UI.MenuItem
        )
      ),

      conditionalPanel(
        "1 == 2",
        checkboxInput(IDs$idDebug, "Debug Mode", value = LOCAL_MODE)
      ),

      conditionalPanel(
        paste0("input.", IDs$idDebug),
        checkboxInput(IDs$idSuperUser, "Super User", value = FALSE),
        selectInput(IDs$idTickerProvider, "Ticker Provider", choices = c("TradeStation", "Yahoo"), selected = "TradeStation"),
        selectInput(IDs$idTickerSource, "Ticker Source", choices = c(`Drop Box` = "dropbox", `Local` = "local"), selected = ifelse(LOCAL_MODE, "local", "dropbox"))
      )
    );




  Server <- function(input, output, session){

    observeEvent(
      input[[IDs$idSearchButton]],
      {
         SearchText <- input[[IDs$idSearchText]];

        Command <- tolower(SearchText);
        if (Command == "debug"){
          updateCheckboxInput(session, IDs$idDebug, value = TRUE);
        } else if (Command == "m2mm"){
          updateCheckboxInput(session, IDs$idSuperUser, value = TRUE);
        } else if (Command == "reset"){
          updateCheckboxInput(session, IDs$idDebug, value = FALSE);
          updateCheckboxInput(session, IDs$idSuperUser, value = FALSE);
        }


        updateTextInput(session, IDs$idSearchText, value = "");
      }
    )

  }


  return(
    list(
      IDs = IDs,
      UI = UI,
      Server = Server
    )
  )

}


SideBar <- .SideBar();

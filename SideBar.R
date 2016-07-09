.SideBar <- function(){

  myIDs <-list(
    "idSuperUser",
    "idDebug",
    "idTickerProvider",
    "idTickerSource",
    "idSideBarMenu",
    "idSearchButton",
    "idSearchText"
  );
  names(myIDs) <- myIDs;

  UI <-

     dashboardSidebar(
      shinyjs::useShinyjs(),

      sidebarMenu(
        id = myIDs$idSideBarMenu,

        Calcs$UI.MenuItem,
        BackTest$UI.MenuItem,


        menuItem(
          text = "Maintenance",
          Scraper$UI.MenuItem
        ),

        Docs$UI.MenuItem

      ),

      sidebarSearchForm(textId = myIDs$idSearchText, buttonId = myIDs$idSearchButton, label = "Search or Command..."),

      conditionalPanel(
        "1 == 2",
        checkboxInput(myIDs$idDebug, "Debug Mode", value = LOCAL_MODE)
      ),

      conditionalPanel(
        paste0("input.", myIDs$idDebug),
        checkboxInput(myIDs$idSuperUser, "Super User", value = FALSE),
        selectInput(myIDs$idTickerProvider, "Ticker Provider", choices = c("TradeStation", "Yahoo"), selected = "TradeStation"),
        selectInput(myIDs$idTickerSource, "Ticker Source", choices = c(`Drop Box` = "dropbox", `Local` = "local"), selected = ifelse(LOCAL_MODE, "local", "dropbox"))
      )
    );




  Server <- function(input, output, session){

    observeEvent(
      input[[myIDs$idSearchButton]],
      {
         SearchText <- input[[myIDs$idSearchText]];

        Command <- tolower(SearchText);
        if (Command == "debug"){
          updateCheckboxInput(session, myIDs$idDebug, value = TRUE);
        } else if (Command == "m2mm"){
          updateCheckboxInput(session, myIDs$idSuperUser, value = TRUE);
        } else if (Command == "reset"){
          updateCheckboxInput(session, myIDs$idDebug, value = FALSE);
          updateCheckboxInput(session, myIDs$idSuperUser, value = FALSE);
        }


        updateTextInput(session, myIDs$idSearchText, value = "");
      }
    )

  }


  return(
    list(
      IDs = myIDs,
      UI = UI,
      Server = Server
    )
  )

}


SideBar <- .SideBar();

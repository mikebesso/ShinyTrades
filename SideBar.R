.SideBar <- function(){


  UI <-

     dashboardSidebar(
      shinyjs::useShinyjs(),
      sidebarSearchForm(textId = IDs$SideBar$idSearchText, buttonId = IDs$SideBar$idSearchButton, label = "Search or Command..."),

      sidebarMenu(
        id = IDs$SideBar$idSideBarMenu,
        BackTest$UI.MenuItem,
        Scraper$UI.MenuItem
      ),

      conditionalPanel(
        "1 == 2",
        checkboxInput(IDs$SideBar$idDebug, "Debug Mode", value = LOCAL_MODE)
      ),

      conditionalPanel(
        paste0("input.", IDs$SideBar$idDebug),
        checkboxInput(IDs$SideBar$idSuperUser, "Super User", value = FALSE),
        selectInput(IDs$SideBar$idTickerProvider, "Ticker Provider", choices = c("TradeStation", "Yahoo"), selected = "TradeStation"),
        selectInput(IDs$SideBar$idTickerSource, "Ticker Source", choices = c(`Drop Box` = "dropbox", `Local` = "local"), selected = ifelse(LOCAL_MODE, "local", "dropbox"))
      )
    );




  Server <- function(input, output, session){

    observeEvent(
      input[[IDs$SideBar$idSearchButton]],
      {
         SearchText <- input[[IDs$SideBar$idSearchText]];

        Command <- tolower(SearchText);
        if (Command == "debug"){
          updateCheckboxInput(session, IDs$SideBar$idDebug, value = TRUE);
        } else if (Command == "m2mm"){
          updateCheckboxInput(session, IDs$SideBar$idSuperUser, value = TRUE);
        } else if (Command == "reset"){
          updateCheckboxInput(session, IDs$SideBar$idDebug, value = FALSE);
          updateCheckboxInput(session, IDs$SideBar$idSuperUser, value = FALSE);
        }


        updateTextInput(session, IDs$SideBar$idSearchText, value = "");
      }
    )

  }


  return(
    list(
      UI = UI,
      Server = Server
    )
  )

}


SideBar <- .SideBar();

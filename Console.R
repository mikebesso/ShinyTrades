.Console <- function(){


  idPage <- "pg_console";
  idPassword = "pg_console_pw";
  idSubmit = "pg_console_submit";

  IsSuperUser <- function(input){
    input[[idPassword]] == 'm2mm' | input[["idSuperUser"]];
  }

  UI.MenuItem <- menuItem("Console", tabName = idPage);

  UI.Page <-
    tabItem(
      idPage,
      fixedRow(
        column(
          width = 12,
          passwordInput(idPassword, "Password"),
          actionButton(idSubmit, "Submit")
        )
      )
    );

  Server <- function(input, output, session){

    Data <- reactive(
      {
        list(
          Password = input[[idPassword]]
        );
      });

    observeEvent(
      input[[idSubmit]],
      {
        #probably want to hide and or unhide stuff here
      }
    )
  }


  return(
    list(
      UI.Page = UI.Page,
      UI.MenuItem = UI.MenuItem,
      Server = Server,
      IsSuperUser = IsSuperUser
    )
  )

}


Console <- .Console();

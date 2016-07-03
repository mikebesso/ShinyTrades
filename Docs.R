.Docs <- function(){


  TabName <- "Docs";

  myIDs <- list(
    "idDocs"
  );
  names(myIDs) <- myIDs;


  UI.MenuItem <-
    menuItem(
      text = "Documentation",
      menuSubItem(text = "Welcome", tabName = "Welcome", selected = TRUE)

    )

  UI.Page <- function(){
    DocTabs$Markdown("Welcome");

  }
    # tabItem(
    #   DocTabs$Markdown("Welcome")
    #
    #   tabName = "Welcome",
    #   fluidRow(
    #     column(
    #       width = 12,
    #       includeMarkdown(file.path("docs", "Welcome.md"))
    #     )
    #   )

  Server <- function(input, output, session){


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

Docs <- .Docs();

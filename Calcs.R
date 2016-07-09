source("Calculators.R");
source("CalcForex.R");
source("CalcFutures.R");
source("CalcEquities.R");



.Calcs <- function(){


  TabName <- "Calcs";

  myIDs <- list(
    "idCalcs"
  );
  names(myIDs) <- myIDs;


  UI.MenuItem <-
    menuItem(
      text = "Position Size Calculators",
      menuSubItem(
        text = "Docs",
        icon = Icon$ui$help,
        href = "http://mikebesso.github.io/ShinyTrades/positionsizecalculators.html"
      ),
      ForexCalculator$UI.MenuItem,
      FuturesCalculator$UI.MenuItem,
      EquitiesCalculator$UI.MenuItem
    );

  UI.Page <- function(){
    list(
      DocTabs$Markdown("PositionCalculators"),
      FuturesCalculator$UI.Page,
      ForexCalculator$UI.Page,
      EquitiesCalculator$UI.Page
    )

  }


  Server <- function(input, output, session){

    FuturesCalculator$Server(input, output, session);
    ForexCalculator$Server(input, output, session);
    EquitiesCalculator$Server(input, output, session);

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

Calcs <- .Calcs();

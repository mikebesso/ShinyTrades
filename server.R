


#EventTableRowsSelected <- LevelEditor$TableRowsSelected();




shinyServer(
  function(input, output, session) {


    SideBar$Server(input, output, session);
    Scraper$Server(input, output, session);
    BackTest$Server(input, output, session);

    Console$Server(input, output, session);
    FuturesCalculator$Server(input, output, session);
    ForexCalculator$Server(input, output, session);
    EquitiesCalculator$Server(input, output, session);


  }




)


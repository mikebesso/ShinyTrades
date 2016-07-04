
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


Header <- dashboardHeader(
  title = "Shiny Trades"
);
#
# Sidebar <- dashboardSidebar(
#   shinyjs::useShinyjs(),
#   sidebarSearchForm(textId = "idSearch", buttonId = "idSearchButton", label = "Search or Command..."),
#
#   sidebarMenu(
#     id = "tabs",
#     menuItem("Back Test", tabName = BackTest$TabName),
#     Scraper$UI.MenuItem,
# #    menuItem("Level Viewer", tabName="level_viewer"),
# #    menuItem("Levels", tabName = "levels")
#     Console$UI.MenuItem
#   ),
#
#   conditionalPanel(
#     "1 == 2",
#     checkboxInput("idDebug", "Debug Mode", value = FALSE)
#   ),
#
#   conditionalPanel(
#     "input.idDebug",
#     checkboxInput("idSuperUser", "Super User", value = FALSE)
#
#   )
# );


Body = dashboardBody(
  withMathJax(),

  #this used to be tabItems... but the assert fails when you pass in a list.
  div(
    class = "tab-content",
    Calcs$UI.Page(),
    Scraper$UI.Page,
    BackTest$UI.Page,
    Docs$UI.Page(),
    Console$UI.Page
  )
);


shinyUI(
  dashboardPage(
    title = "ShinyTrades",
    header = Header,
    sidebar = SideBar$UI,
    body = Body,
    skin = "blue"
  )
);




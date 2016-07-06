.LevelViewer <- function(MetaData, ID_Prefix = "LevelViewer"){

  myMetaData <- MetaData;

  myPrefix <- ID_Prefix;

  ID <- function(id){
    paste(ID_Prefix, id, sep="_");
  }

  TableID <- ID("tbl");
  TableRowsSelectedEventID <- ID("rows_selected");

  uiDecorate <- function(.ui, .hidden = FALSE, .disabled = FALSE){

    .hidden <- .disabled | .hidden;
    if (.hidden){
      .ui <- shinyjs::hidden(.ui);
    };
    if (.disabled){
      .ui <- shinyjs::disabled(.ui);
    }
    .ui;
  }

  uiTextInput <- function(field, .hidden = FALSE, .disabled = FALSE){

    .attributes <- myMetaData$GetAttributes(field);

    if(!is.null(.attributes))
    {
      return(
        uiDecorate(
          textInput(.attributes$DomID, .attributes$Label, .attributes$Default),
          .hidden,
          .disabled
        )
      )
    } else {
      uiMissingField(field);
    }
  }

  uiSelectInput <- function(field, .hidden = FALSE, .disabled = FALSE){

    .attributes <- myMetaData$GetAttributes(field);

    if(!is.null(.attributes))
    {
      return(
        uiDecorate(
          selectInput(.attributes$DomID, .attributes$Label, choices = .attributes$Values, selected = .attributes$Default),
          .hidden,
          .disabled
        )
      )
    } else {
      uiMissingField(field);
    }
  }


  uiNumericInput <- function(field, .hidden = FALSE, .disabled = FALSE){

    .attributes <- myMetaData$GetAttributes(field);

    if(!is.null(.attributes))
    {
      return(
        uiDecorate(
          numericInput(.attributes$DomID, .attributes$Label, value = .attributes$Default, min = .attributes$min, max = .attributes$max),
          .hidden,
          .disabled
        )
      )
    } else {
      uiMissingField(field);
    }
  }

  uiDateInput <- function(field, .hidden = FALSE, .disabled = FALSE){

    .attributes <- myMetaData$GetAttributes(field);

    if(!is.null(.attributes))
    {
      return(
        uiDecorate(
          dateInput(.attributes$DomID, .attributes$Label, value = .attributes$Default),
          .hidden,
          .disabled
        )
      )
    } else {
      uiMissingField(field);
    }
  }

  uiControlInput <- function(field, .hidden = FALSE, .disabled = FALSE){

    .attributes <- myMetaData$GetAttributes(field);

    if (.attributes$ControlType == "NumericInput") {
      uiNumericInput(field, .hidden, .disabled);
    } else if (.attributes$ControlType == "DateInput")
    {
      uiDateInput(field, .hidden, .disabled);
    } else if (.attributes$ControlType == "SelectInput"){
      uiSelectInput(field, .hidden, .disabled);
    } else {
      uiTextInput(field, .hidden, .disabled);
    }
  }





  uiTableOutput <- function(){
    DT::dataTableOutput(TableID);
  }



  ServerInitializeFormData <- function(input){
    .ids <- intersect(MetaData$DomIDs(), names(input));
    .formData <- sapply(.ids, function(x) input[[x]]);
    .formData;
  }




  Tab <- function(){

    return(


      tabItem(
        "level_viewer",
        fixedRow(
          column(
            width = 4,
            "Filter"
          ),
          column(
            width = 4,
            dateInput(ID("AsOfDate"), "Date:", value = format(Calendar$CurrentWeekday(), format="%Y-%m-%d"))
          ),
          column(
            width = 4,
            selectInput(ID("AssetClass"), "Asset Class:", list("Futures", "FOREX", "Indices"))
          )
        ),

        fixedRow(
          column(
            width = 12,
            uiTableOutput()
          )
        ),



        fluidRow(
          #action buttons
          column(
            width = 12,
            actionButton(ID("Analyze"), "Analyze")
          )
        )
      )
    )
  }


  return(
    list(
      Tab = Tab,
      ServerInitializeFormData = ServerInitializeFormData,
      TableID = TableID,
      TableRowsSelectedEventID = TableRowsSelectedEventID
    )

  )



}


LevelViewer <- .LevelViewer(LevelData$MetaData());

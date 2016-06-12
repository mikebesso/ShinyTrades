CreateLevelEditor <- function(MetaData){


  myMetaData <- MetaData;


  .TableID <- paste0(myMetaData$DataSetName(), "_tbl");
  TableID <- function() {.TableID}
  TableRowsSelected <- function() return(paste0(.TableID, "_rows_selected"));

  uiMissingField <- function(field){
    paste("Missing Field:", field);
  }

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

  serverUpdateInput <- function(session, .field, .value){

    .value <- unlist(unname(.value));

    .attributes <- myMetaData$GetAttributes(.field);

    if (!is.null(.attributes)){

      .id <- .attributes$DomID;

      if (.attributes$ControlType == "NumericInput") {
        updateNumericInput(session, .id, value = .value);
      } else if (.attributes$ControlType == "DateInput")
      {
        updateDateInput(session, .id, value = AsDate(.value));
      } else if (.attributes$ControlType == "SelectInput"){
        updateSelectInput(session, .id, selected = .value);
      } else {
        updateTextInput(session, .id, value = .value);
      }

    }
  }

  serverUpdateInputs <- function(data, session){
    sapply(
      names(data),
      function(.field){
        serverUpdateInput(session, .field, data[.field]);
      }
    );
  }



  uiTableOutput <- function(){
    DT::dataTableOutput(.TableID);
  }



  ServerInitializeFormData <- function(input){
    .ids <- intersect(MetaData$DomIDs(), names(input));
    .formData <- sapply(.ids, function(x) input[[x]]);
    .formData;
  }




  EditorTab <- function(){

    return(


      tabItem(
        "levels",
        fixedRow(
          column(
            width = 3,
            "Filter"
          ),
          column(
            width = 3,
            dateInput("LE_FilterDate", "Date:", value = format(Calendar$CurrentWeekday(), format="%Y-%m-%d"))
          ),
          column(
            width = 3,
            selectInput("LE_FilterAssetClass", "Asset Class:", list("Futures", "FOREX", "Indices"))
          ),
          column(
            width = 3,
            selectInput("LE_FilterSource", "Source:", list("MasterMind", "XLT", "Personal"))
          )
        ),

        fixedRow(
          column(
            width = 12,
            uiTableOutput()
          )
        ),

        fixedRow(
          column(
            width = 12,
            uiControlInput("ID", FALSE, FALSE),
            uiControlInput("AsOfDate", FALSE, FALSE)
          )
        ),


        fluidRow(
          column(4, uiSelectInput("AssetClass")),
          column(4, uiSelectInput("Source")),
          column(4, uiTextInput("Symbol"))
        ),

        fixedRow(
          column(
            width = 12,
            "Here",
            textOutput("validation")
          )
        ),

        fixedRow(
          column(width = 4, "Supply"),
          column(width = 4, "Distal"),
          column(width = 4, "Proximal")
        ),
        fixedRow(
          column(width = 4, "Farthest"),
          column(width = 4, uiNumericInput("SD3")),
          column(width = 4, uiNumericInput("SP3"))
        ),
        fixedRow(
          column(width = 4, "Middle"),
          column(width = 4, uiNumericInput("SD2")),
          column(width = 4, uiNumericInput("SP2"))
        ),
        fixedRow(
          column(width = 4, "Nearest"),
          column(width = 4, uiNumericInput("SD1")),
          column(width = 4, uiNumericInput("SP1"))
        ),

        fixedRow(
          column(width = 4, "Demand"),
          column(width = 4, "Proximal"),
          column(width = 4, "Distal")
        ),

        fixedRow(
          column(width = 4, "Nearest"),
          column(width = 4, uiNumericInput("DP1")),
          column(width = 4, uiNumericInput("DD1"))
        ),
        fixedRow(
          column(width = 4, "Middle"),
          column(width = 4, uiNumericInput("DP2")),
          column(width = 4, uiNumericInput("DD2"))
        ),
        fixedRow(
          column(width = 4, "Farthest"),
          column(width = 4, uiNumericInput("DP3")),
          column(width = 4, uiNumericInput("DD3"))
        ),



        fluidRow(
          #action buttons
          column(
            width = 12,
            actionButton("submit", "Submit"),
            actionButton("new", "New"),
            actionButton("delete", "Delete")
          )
        )
      )
    )
  }



  return(
    list(
      EditorTab = EditorTab,
      ServerInitializeFormData = ServerInitializeFormData,
      serverUpdateInputs = serverUpdateInputs,
      TableID = TableID,
      TableRowsSelected = TableRowsSelected
    )

  )



}




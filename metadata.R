if (interactive()){
  library(shiny);
  library(shinydashboard);
  library(shinyjs);
  library(RCurl);
  library(rdrop2);
  library(dplyr);
  library(lubridate);
  library(assertthat);
}


FormatDate <- function(d = Sys.Date()){
  format(d, format="%Y-%m-%d");
}

AsDate <- function(x) {

  retVal <- NA;
  tryCatch(
    retVal <- as.Date(ymd(x)),
    error = function(x) {},
    warning = function(x) {}
  )

  if (is.na(retVal)){
    suppressWarnings(.numeric_x <- as.numeric(x));

    if (!anyNA(is.na(.numeric_x))){
      return (as.Date(.numeric_x, origin = "1970-01-01"));
    } else {
      return (as.Date(ymd(FormatDate(x))));
    }
  } else {
    return(retVal);
  }
}



CreateMetaData <- function(.DataSetName = "data"){


  myDataSetName <- .DataSetName;
  DataSetName <- function() {myDataSetName}




  .FieldCount <- 0;

  CreateAField <- function(Field, as.type, .default, Label = paste0(Field, ":"), min = NA, max = NA, Values = NA, ControlType = NA){

    if (is.na(ControlType)){
      if (!anyNA(Values)){
        ControlType = 'SelectInput';
      } else if (is.Date(.default)) {
        ControlType = "DateInput";
      } else if (is.na(min) | is.na(max) | is.numeric(.default)) {
        ControlType = 'NumericInput';
      } else {
        ControlType = 'TextInput'
      }
    };

    Default <- as.type(.default);
    DefaultText <- as.character(as.type(.default));

    field <- list(
      Field = Field,
      DomID = paste0(tolower(myDataSetName), "_", Field),
      Cast = as.type,
      Default = Default,
      DefaultText = DefaultText,
      Label = Label,
      min = NA,
      max = NA,
      Values = Values,
      ControlType = ControlType
    );
    NewFields <- list(field);
    names(NewFields) <- Field;

    .FieldCount <<- .FieldCount + 1;
    NewFields;
  };



  .MetaData <- CreateAField("ID", as.integer, 0);


  CreateField <- function(Field, as.type, Default, Label = paste0(Field, ":"), min = NA, max = NA, Values = NA, ControlType = NA){

    NewField <- CreateAField(Field, as.type, Default, Label = Label, min = min, max = max, Values = Values, ControlType = ControlType);

    .MetaData <<- append(
      .MetaData,
      NewField
    );

    NewField;

  }



  FieldOrID <- function(name){
    f <- Fields() == name;
    i <- DomIDs() == name;

    f | i;
  }

  Field <- function(name){
    MetaData[FieldOrID(name)]$Field;
  }

  Fields <- function(){
    unname(sapply(.MetaData, `[[` , "Field"));
  }

   DomIDs <- function(){
    unname(sapply(.MetaData, `[[` , "DomID"));
  }

  Defaults <- function(){
    lapply(.MetaData, `[[` , "Default");
  }


  GetAttributes <- function(.name){
    mask <- FieldOrID(.name);
    if (any(mask)){
      .field <- .MetaData[mask][[1]];
      return(.field);
    } else {
      return(NULL);
    }
  }

  GetAttribute <- function(Attribute, .name){
    .field <- GetAttributes(.name);
    if (!is.null(.field)){
      return(unlist(unname(.field[Attribute])));
    } else {
      return(NULL);
    }
  }

  DomID <- function(name) GetAttribute("DomID", name);
  Default <- function(name) GetAttribute("Default", name);



  FieldCount <- function(){
    .FieldCount;
  }

  MetaData <- function(){
    .MetaData;
  }


  AsList <- function(data) {

    assert_that(is.data.frame(data), nrow(data) == 1);
    data <- data[,Fields()];
    names(data) <- DomIDs();

    as.list(data);
  }

  CastField <- function(.field, .value){

    .attributes <- GetAttributes(.field);
    if (!is.null(.attributes)) {
      .retVal <- .attributes$Cast(.value);
    } else {
      .retVal <- .value;
    }
    .retVal;
  }

  AsDataFrame <- function(data) {


    df <- as.data.frame(
      as.list(data),
      stringsAsFactors = FALSE
    );

    df <- df[, DomIDs()];
    names(df) <- Fields();

    dl <- lapply(
      names(df),
      function(.field) {
          CastField(.field, df[1, .field]);
      }
    )

    names(dl) <- names(df);

    df <- as.data.frame(
      as.list(dl),
      stringsAsFactors = FALSE
    );


    return (df);
  }


  DefaultAsDataFrame <- function() {
    data <- as.list(Defaults());
    names(data) <- Fields();

    df <- as.data.frame(
      data,
      stringsAsFactors = FALSE
    );

    df;
  }

  DefaultAsList <- function(){
    AsList(DefaultAsDataFrame());
  }





  return(
    list(
      DataSetName = DataSetName,
      FieldCount = FieldCount,
      MetaData = MetaData,
      CreateField = CreateField,
      DomID = DomID,
      Default = Default,
      Defaults = Defaults,
      Field = Field,
      Fields = Fields,
      DomIDs = DomIDs,

      DefaultAsDataFrame = DefaultAsDataFrame,
      AsDataFrame = AsDataFrame,
      AsList = AsList,
      DefaultAsList = DefaultAsList,
      GetAttributes = GetAttributes
    )
  )

}

# LevelsMetaData <- CreateMetaData();


# LevelsMetaData$CreateField("AssetClass", as.character, "Indices", Values = c("Indices", "FOREX", "Futures"));
# LevelsMetaData$CreateField("Symbol", as.character, "");
#
# within(
#   LevelsMetaData,
#   {
#     CreateField("AsOfDate", AsDate, Default = FormatDate(), ControlType = "DateInput");
#     CreateField("AssetClass", as.character, "Indices", Values = c("Indices", "FOREX", "Futures"));
#     CreateField("Symbol", as.character, "");
#     CreateField("Source", as.character, "Manual", Values = c("Genius", "Pro", "Mine"));
#     CreateField("SD3", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("SP3", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("SD2", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("SP2", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("SD1", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("SP1", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DP1", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DD1", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DP2", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DD2", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DP3", as.double, 0, min = 0, max = 999999, Label = NULL);
#     CreateField("DD3", as.double, 0, min = 0, max = 999999, Label = NULL);
#   }
# )








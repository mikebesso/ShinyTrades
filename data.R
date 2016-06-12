
CreateLevelDataInterface <- function(){

  .myFirstID <- 10001;
  myData <- NULL;
  myFilteredData <- NULL;



  myMetaData <- CreateMetaData();
  within(
    myMetaData,
    {
      CreateField("AsOfDate", AsDate, Default = FormatDate(), ControlType = "DateInput");
      CreateField("AssetClass", as.character, "Indices", Values = c("Indices", "FOREX", "Futures"));
      CreateField("Symbol", as.character, "");
      CreateField("Source", as.character, "Manual", Values = c("Genius", "Pro", "Mine"));
      CreateField("SD3", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("SP3", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("SD2", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("SP2", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("SD1", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("SP1", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DP1", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DD1", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DP2", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DD2", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DP3", as.double, 0, min = 0, max = 999999, Label = NULL);
      CreateField("DD3", as.double, 0, min = 0, max = 999999, Label = NULL);
    }
  )


  MetaData <- function(){
    return(myMetaData);
  }



  GetNextId <- function() {
    .next_id <- .myFirstID;

    if (is.null(myData)){
      if (nrow(myData) > 0) {
        .nextid <- as.integer(myData$ID) + 1;
      }
    }

    return(.next_id)
  }


  Load <- function(as_of_date = Calendar$CurrentWeekday()) {

#    browser();

    myData <- DropBox$GetLevelFile(as_of_date);
    rownames(myData) <- myData$ID;

    myData;

  }




  FilterData <- function(.AssetClass = "Futures", .AsOfDate = NA) {


    if (invalid(.AsOfDate)){
      .AsOfDate <- max(myData$AsOfDate);
    } else if (!(.AsOfdate %in% myData$AsOfDate)){
      .AsOfDate <- max(myData$AsOfDate);
    }

    if (invalid(.AssetClass)){
      .AssetClass = "Futures";
    }

    myFilteredData <<- subset(
      myData,
      AssetClass == .AssetClass & AsOfDate == .AsOfDate
    );

    myFilteredData;
  }


  CreateTestData <- function(){
    Indices <- c("QQQ", "SPY", "IWM");
    Futures <- c("@C", "@S");
    FOREX <- c("EURUSD", "USDCAD");

    df_symbols <- bind_rows(
      data.frame(AssetClass = rep("Indices", length(Indices)), Symbol = Indices, stringsAsFactors = FALSE),
      data.frame(AssetClass = rep("Futures", length(Futures)), Symbol = Futures, stringsAsFactors = FALSE),
      data.frame(AssetClass = rep("FOREX", length(FOREX)), Symbol = FOREX, stringsAsFactors = FALSE)
    )

    dates <- Calendar$WeekdaySequence();

    df <- merge(
      df_symbols,
      data.frame(AsOfDate = dates, stringsAsFactors = FALSE),
      all = TRUE,
      stringsAsFactors = FALSE);


    df$Source = "Master Mind";

    df$ID = seq(1, nrow(df));

    df$SD3 = as.double(100);
    df$SP3 = as.double(100);
    df$SD2 = as.double(100);
    df$SP2 = as.double(100);
    df$SD1 = as.double(100);
    df$SP1 = as.double(100);
    df$DP1 = as.double(100);
    df$DD1 = as.double(100);
    df$DP2 = as.double(100);
    df$DD2 = as.double(100);
    df$DP3 = as.double(100);
    df$DD3 = as.double(100);

    df[, myMetaData$Fields()];
  }

  TranslateID <- function(.id){

    if (!invalid(myData) & !invalid(.id)){

      if (.id < .myFirstID){
        if (.id > 0 & .id <= nrow(myFilteredData)){
          .id <- myFilteredData$ID[.id];
        } else if (!(.id %in% myData$ID)) {
          .id <- NA;
        }
      }

    } else {
      .id <- NA;
    }

    .id;
  }


  CrudCreate <- function(new_data) {

    data <- myMetaData$AsDataFrame(new_data);

    data$ID <- GetNextId();
    rownames(data) <- data$ID;

    if (!is.null(myData)) {
      myData <<- rbind(myData, data)
    } else {
      myData <<- data
    }
  }

  CrudRead <- function(.id) {

    .id <- TranslateID(.id);

    if (!invalid(.id)){
      myMetaData$AsList(subset(myData, ID == .id))
    } else {
      myMetaData$DefaultAsList();
    }
  }


  CrudUpdate <- function(.updates, .id = .updates$data_ID) {


    if (!invalid(myData)){

      .updated <- FALSE;
      .id <- TranslateID(.id);

      if (!invalid(.id)){
            data <- myMetaData$AsDataFrame(.updates);
            myData[rownames(myData) == .id, ] <<- data
            .updated <- TRUE;
        }

      if (!.updated) {
        CrudCreate(.updates);
      }
    }
  }

  CrudDelete <- function(.id) {
    myData <<- myData[ID != unname(.id), ]
  }



  #Load();


  return(
    list(
      MetaData = MetaData,
      Load = Load,
      FilterData = FilterData,

      Fields <- myMetaData$Fields,
      DefaultAsList <- myMetaData$DefaultAsList,
      DefaultAsDataFrame <- myMetaData$DefaultAsDataFrame,

      CrudCreate = CrudCreate,
      CrudRead = CrudRead,
      CrudUpdate = CrudUpdate,
      CrudDelete = CrudDelete


    )
  );

}





LevelData <- CreateLevelDataInterface();






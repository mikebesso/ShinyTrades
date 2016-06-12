


GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(responses$ID)) + 1
  } else {
    return (1)
  }
}




CreateData <- function(new_data) {

  browser();

  data <- LevelsMetaData$AsDataFrame(new_data);

  data$ID <- GetNextId();
  rownames(data) <- data$ID;

  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}


ReadData <- function(.AssetClass = "Futures", .Date = NA) {

  if (exists("responses")) {
    responses
  } else {
    responses <<- CreateTestData();
  }

  rownames(responses) <- responses$ID;

  if (is.na(.Date)){
    .Date <- max(responses$AsOfDate);
  }
  subset(
    responses,
    AssetClass == .AssetClass & AsOfDate == .Date
  );
}

UpdateData <- function(updates) {

  browser();
  data <- LevelsMetaData$AsDataFrame(updates);


  if (exists("responses")){

    if (data$ID %in% rownames(responses)){

      responses[rownames(responses) == data$ID, ] <<- data
    }
  } else {
    CreateData(updates);
  }
}

DeleteData <- function(deletes) {
  responses <<- responses[row.names(responses) != unname(deletes["id"]), ]
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

  dates <- WeekdaySequence();

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

  df[, LevelsMetaData$Fields()];
}

responses <- CreateTestData();

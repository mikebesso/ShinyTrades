

mmmStringAPI <- function(){

  ExtractDate <- function(strings, date_pattern = "[[:digit:]]{4}.[[:digit:]]{2}.[[:digit:]]{2}", date_format = "%Y_%m_%d"){
    date_strings <- str_extract(strings, date_pattern);
    dates <- as.Date(date_strings);

  }



  return(
    list(
      ExtractDate = ExtractDate
    )
  );

}

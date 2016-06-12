

.App <- function(){

  AppAPI <- mmmAppAPI(TZ = "America/Chicago");

  DropBoxRoot <- "/Apps/ShinyTrades";
  DropBoxTokenFile <- "droptoken.RDS"



  return(
    append(
      list(
        DropBoxRoot = DropBoxRoot,
        DropBoxTokenFile = DropBoxTokenFile
      ),
      AppAPI
    )
  )


}

App <- .App();


source("FileSystem.R");
source("DropBox.R");

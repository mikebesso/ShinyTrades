

mmmAppAPI <- function(TZ = Sys.getenv("TZ")){

  GetTimeZone <- function(){
    Sys.getenv("TZ")
  }


  SetTimeZone <- function(TZ){
    Sys.setenv(TZ = TZ);
  }


  if(TZ != GetTimeZone()){
    SetTimeZone(TZ);
  }

  return(
    list(
      GetTimeZone = GetTimeZone,
      SetTimeZone = SetTimeZone
    )
  )


}

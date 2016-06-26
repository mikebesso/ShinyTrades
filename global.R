

source("bootstrap.R");


IsSuperUser <- function(input){
  input[[IDs$SideBar$idSuperUser]];
}

IsDebugMode <- function(input)
{
  input[[IDs$SideBar$idDebug]];
}


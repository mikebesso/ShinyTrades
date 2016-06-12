

source("bootstrap.R");


IsSuperUser <- function(input){
  input[[IDs$SideBar$idPassword]] == 'm2mm' | input[[IDs$SideBar$idSuperUser]];
}

IsDebugMode <- function(input)
{
  input[[IDs$SideBar$idDebug]];
}


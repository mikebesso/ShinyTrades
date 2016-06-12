
.FileSystem <- function(){

  FileSystemAPI <- mmmFileSystemAPI();

  Initialize <- function(){
    FileSystemAPI$CreateFolder(c("dropbox/scrapes/clean", "dropbox/scrapes/clipboard", "dropbox/scrapes/raw"))
  }

  Initialize();

  return(
    append(
      list(
        Initialize = Initialize
      ),
      FileSystemAPI
    )
  )


}



FileSystem <- .FileSystem();

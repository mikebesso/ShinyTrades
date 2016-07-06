

mmmFileSystemAPI <- function(){

  CreateFolder <- function(folders){

    llply(
      folders,
      function(f){
        if(!dir.exists(f)){
          dir.create(f, recursive = TRUE);
        }
        f;
      }
    );

  }

  FileExists <- function(path){
    file.exists(path);
  }

  CloseConnection <- function(con){
    cat("closing connection", summary(con)$description, "\n");
    close(con);
  }



  ReadLines <- function(path){
    lines <- NULL;

    if(FileExists(path)){
      con <- file(path, "r");
      on.exit(CloseConnection(con));
      lines <- suppressWarnings(readLines(con));
    }

    lines;

  }

  ReadCSV <- function(path, ColumnTypes = NULL){
    df <- NULL;

    if(FileExists(path)){
      con <- file(path, "rb");
      on.exit(CloseConnection(con));
      df <- suppressWarnings(read_csv(con, col_types = ColumnTypes));
    }

    df;

  }





  WriteLines <- function(lines, path){
    con <- file(path, "w+");
    on.exit(CloseConnection(con));
    lines <- suppressWarnings(writeLines(lines, con));

  }

  WriteCSV <- function(df, path, postpend = NULL){
    df <- suppressWarnings(write_csv(df, path));

    if(!is.null(postpend)){
      write(postpend, path, append = TRUE);
    }

  }

  Folders <- function(path = '.', pattern = NULL, full.names = TRUE, recursive = FALSE) {
    dirs <- list.dirs(path = path, full.names = full.names, recursive = recursive);

    if (!invalid(pattern)) {
      dirs[str_detect(dirs, pattern)];
    }

    dirs;
  }

  Files <- function(path = '.', pattern = NULL, full.names = TRUE, recursive = FALSE, include.dirs = FALSE, use.regex = TRUE) {

    if (!use.regex){
      pattern = glob2rx(pattern);
    }

    files <- list.files(path = path, pattern = pattern, full.names = full.names, recursive = recursive, no.. = TRUE, ignore.case = TRUE, include.dirs = include.dirs);

    if (!include.dirs & !recursive) {
      dirs <- Folders(path);
      files <- files[!(files %in% dirs)];
    }

    files;
  }


  Extension <- function(f){
    file_ext(f);
  }

  RemoveExtension <- function(f){
    file_path_sans_ext(f);
  }


  DeleteFiles <- function(f){
    if (length(f) > 0){
      l_ply(f, file.remove);
    }
  }

  return(
    list(
      Files = Files,
      Folders = Folders,
      CreateFolder = CreateFolder,
      FileExists = FileExists,
      ReadCSV = ReadCSV,
      ReadLines = ReadLines,
      WriteCSV = WriteCSV,
      WriteLines = WriteLines,
      RemoveExtension = RemoveExtension,
      Extension = Extension,
      DeleteFiles = DeleteFiles
    )
  )

}



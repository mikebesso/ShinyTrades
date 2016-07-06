
mmmDropBoxAPI <- function(root = ".", token_file = "droptoken.RDS"){

  FileSystem <- mmmFileSystemAPI();

  Token <- readRDS(token_file);
  #Account <- drop_acc(Token);


  Root <- root;

  ExpandPath <- function(path){
    if(path == "."){
      path = Root;
    }
    else if(substr(path, 1, 1) != "/"){
      path <- file.path(Root, path);
    }
    path <- str_replace_all(path, fixed("/./"), "/");

    path;
  }

  FindFiles <- function(query, path = "."){

    path <- ExpandPath(path);

    df <- drop_search(query, path, dtoken = Token);
    files <- df$path[!is.na(df$mime_type)];

    files;
  }

  Files <- function(path = ".", query = NA){
    if (is.na(query)){

      path <- ExpandPath(path);

      # using internal version to avoid unneeded print in api
      df <- suppressMessages(rdrop2:::drop_dir_internal(path = path, locale = Sys.getlocale()), dtoken = Token);

      # get rid of folders and just return list of files
      files <- df$path[!is.na(df$mime_type)];

    } else {
      files <- FindFiles();
    }

    files;
  }

  Exists <- function(path){
    path <- ExpandPath(path);
    exists <- drop_exists(path, dtoken = Token);

    return(exists);
  }

  Upload <- function(source_file, dest = Root){
    dest <- ExpandPath(dest);
    drop_upload(source_file, dest = dest, dtoken = Token, overwrite = TRUE, verbose = TRUE);
  }

  Download <- function(path, local_file, overwrite = TRUE){

    retval <- FALSE;

    path <- ExpandPath(path);

    if (Exists(path)) {
      retval <- drop_get(path, local_file = local_file, overwrite = TRUE, dtoken = Token);
    }

    retval;

  }

  ReadLines <- function(path){
    localfile = file.path(tempdir(), basename(path));
    lines <- NULL;

    if(Exists(path)){
      Download(path, local_file = localfile);
      lines <- FileSystem$ReadLines(localfile);
    }

    lines;

  }


  ReadCSV <- function(path){
    localfile = file.path(tempdir(), basename(path));

    df <- NULL;
    if (Exists(path)){
      Download(path, local_file = localfile);
      df <- FileSystem$ReadCSV(localfile, stringsAsFactors = FALSE);
    }
    df;

  }

  ReadZoo <- function(path, header = TRUE, sep = ",", format = "%Y-%m-%dT%H:%M", tz = App$TimeZone, index.column = "timestamp"){

    localfile = file.path(tempdir(), basename(path));

    z <- NULL;
    if (Exists(path)){
      if (Download(path, local_file = localfile)){
        z <- read.zoo(localfile, header = header, sep = sep, format = format, tz = tz, index.column = index.column);
      }
    };

    z;

  }



  WriteCSV <- function(df, path, postpend = NULL){

    localfile = file.path(tempdir(), basename(path));
    path = dirname(path);

    FileSystem$WriteCSV(df, localfile, postpend = postpend);

    DropBox$Upload(localfile, path);

  }

  WriteLines <- function(lines, path){



    localfile = file.path(tempdir(), basename(path));
    path = dirname(path);

    FileSystem$WriteLines(lines, localfile)

    Upload(localfile, path);

  }



  return(
    list(
      Files = Files,
      FindFiles = FindFiles,
      ReadLines = ReadLines,
      ReadZoo = ReadZoo,
      ReadCSV = ReadCSV,
      WriteLines = WriteLines,
      WriteCSV = WriteCSV,
      Upload = Upload,
      Download = Download,
      Exists = Exists
    )
  );


}

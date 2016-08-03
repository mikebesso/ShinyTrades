.Scraper <- function(validate = FALSE){

  Validate <- validate;


  LocalScrapesFolder <- '/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/symbols';
  DropBoxFolder <- 'scrapes/symbols';
  DropBoxCache <- 'DropBoxCache/scrapes/symbols';

  LevelsFolder <- '/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/symbols';

  ReadScraperCSV2Zoo2XTS <- function(Filename, Symbol){
    df <- read_csv(
      Filename,
      col_types = "cccccdddddddddddd"
    );
    format = Calendar$DetermineDateTimeFormat(df$timestamp);

    x <- as.xts(
      zoo(
        df[-(1:5)],
        strptime(
          df$timestamp,
          format = format,
          tz = App$GetTimeZone())
        )
    );


    indexTZ(x) <- App$GetTimeZone();
    xtsAttributes(x) <- list(ticker = Symbol, Interval= "D");

    return(x);
  }

  LoadDataFromDropBox <- function(Symbol){


    FileSystem$CreateFolder(file.path(DropBoxCache));
    filename <- paste0(Symbol, ".csv");
    CachedFile <- file.path(DropBoxCache, filename);

    if(!FileSystem$FileExists(CachedFile)){
      DropBoxFile <- file.path(DropBoxFolder, filename);
      DropBox$Download(DropBoxFile, CachedFile);
    }

    ReadScraperCSV2Zoo2XTS(CachedFile, Symbol);

  }



  Data <- function(Symbol){
    cat("reading levels: ", Symbol, "\n")

    if (LOCAL_MODE) {
      LevelsFolder <- LocalScrapesFolder;
      levels_xts <- ReadScraperCSV2Zoo2XTS(file.path(LevelsFolder, paste0(Symbol, '.csv')), Symbol);
    } else {
      LevelsFolder <- 'scrapes/symbols';
      levels_xts <- LoadDataFromDropBox(Symbol);
    }


    levels_xts;
  }


  Transform <- function(scraped, AsOfDate = as.Date(Sys.Date())){

    scraped <- str_replace_all(scraped, "\n\n", "\n");
    lines <- unlist(strsplit(scraped, "\n"))

    lines <- str_trim(lines, side = 'both');


    symbol_line_number <- which(str_detect(lines, "NASDAQ"))[1];
    symbol_line <- str_replace_all(lines[symbol_line_number], "\t", "  ");

    products <- unlist(strsplit(symbol_line, "\\s\\s+"));

    symbols <- str_replace_all(str_extract(products, "([(][^(]+[)])$"), "[()]", "");
    futures <- str_detect(symbols, "^(C|S|TY|GC|NQ|ES|TF|YM|DAX|NK|SI|HG|CL|NG|EC|JY|AD)$");
    symbols[futures] <- paste0("@", symbols[futures])

    .product_count <- length(products);
    .value_count <- .product_count * 12;


    lines <- lines[-(1:symbol_line_number)];

    lines <- lines[nchar(lines) != 0]
    lines[lines %in% c("*", "<", ">")] <- NA;


    lines <- str_replace(lines, "[*]", "");
    lines <- str_trim(lines[1:.value_count], "both");


    # deal with any bad commas that are most likely decimal points
    bad_comma <- str_detect(lines, "[,][0-9]{1,2}$");
    bad_comma[is.na(bad_comma)] <- FALSE;
    if (any(bad_comma, na.rm = TRUE)) {
      lines[bad_comma] <- str_replace_all(lines[bad_comma], fixed(","), ".");
    }

    lines <- suppressWarnings(as.double(lines));


    df <- ldply(
      seq(0, .product_count - 1),
      function(x) {
        indices <-  sort(
          append(
            seq(from = ((x * 2) + 1), by = .product_count * 2, length.out = 6),
            seq(from = ((x * 2) + 2), by = .product_count * 2, length.out = 6)
          )
        );

        levels <- lines[indices];

        #cat(x, "\n", symbols[x+1], "\n")

        levels[is.na(levels) & append(rep(TRUE, 6), rep(FALSE, 6))] <- 99999;
        levels[is.na(levels) & append(rep(FALSE, 6), rep(TRUE, 6))] <- 0;

        data.frame(
          AsOfDate = AsOfDate,
          Product = products[x+1],
          Symbol = symbols[x+1],
          ChartSymbol = FuturesMetaData$ChartSymbol(symbols[x+1]),
          TradeSymbol = FuturesMetaData$TradeSymbol(symbols[x+1]),
          SD3 = levels[1],
          SP3 = levels[2],
          SD2 = levels[3],
          SP2 = levels[4],
          SD1 = levels[5],
          SP1 = levels[6],
          DP1 = levels[7],
          DD1 = levels[8],
          DP2 = levels[9],
          DD2 = levels[10],
          DP3 = levels[11],
          DD3 = levels[12],
          stringsAsFactors = FALSE

        )
      }
    )

    df;
  }



  Process <- function(AsOfDate = Calendar$CurrentWeekday()){

    Lines <- DropBox$GetClipboardFile(AsOfDate);
    df <- Transform(Lines, AsOfDate);

    DropBox$WriteScrapedCleanFile(df, AsOfDate);

  }



  ProcessClipboardFileFromDropBox <- function(file){
    cat("processing: ", file, "\n");
    lines <- DropBox$ReadLines(file);
    asofdate = String$ExtractDate(file);
    Transform(lines, asofdate);
  }



  ProcessClipboardFromDropBox <- function(){
    l_ply(
      DropBox$ClipboardFiles(),
      ProcessClipboardFile
    );
  }


  ProcessClipboardFile <- function(file){
    cat("processing: ", file, "\n");
    lines <- FileSystem$ReadLines(file);
    if(!is.null(lines)){
      asofdate = String$ExtractDate(file);
      Transform(lines, asofdate);
    }
  }




  CleanClipboardFile <- function(file){
    cat("CleanClipboardFile('", file, "')\n")
    Lines <- FileSystem$ReadLines(file);
    AsOfDate = String$ExtractDate(file);
    df <- Transform(Lines, AsOfDate);
    filename <- file.path("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/clean", paste0("scrape-clean-", AsOfDate, ".csv"));
    FileSystem$WriteCSV(df, filename)
  }

  InitializeFromClipboard <- function(){
    l_ply(
      FileSystem$Files("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/clipboard"),
      CleanClipboardFile
    );
  }

  ReadFileAsText <- function(file){
    FileSystem$ReadCSV(file, c("ccccccccccccccccc"))
  }


  InitializeAllSymbols <- function(){



    df <-
      ldply(
        FileSystem$Files("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/clean"),
        ReadFileAsText
      );

    df$AsOfDate <- paste0(df$AsOfDate, "T07:00");

    names(df)[1] <- 'timestamp';


    d_ply(
      df,
      .(Symbol),
      .fun = function(df){
        cat(df$Symbol[1],":", nrow(df), "\n");
        FileSystem$WriteCSV(
          df, file.path("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/symbols", paste0(df$Symbol[1], ".csv"))
        )
      }
    )
  }

  InitializeAll <- function(){

    InitializeFromClipboard();
    InitializeAllSymbols();

    return(TRUE);
  }




  UI.MenuItem <- menuSubItem("Scraper", tabName = "scraper");


  UI.Page <-
    tabItem(
      "scraper",

      fixedRow(
        column(
          width = 12,
          dateInput("Scraper_AsOfDate", "Date:", value = format(Sys.Date(), format="%Y-%m-%d")),
          actionButton("Scraper_Load", "Load from Drop Box")
        )
      ),

      fixedRow(
        column(
          width = 12,
          aceEditor("Scraper_Scraped", "", height = "400px")
        )
      ),

      fluidRow(
        #action buttons
        column(
          width = 12,
          actionButton("Scraper_Scrape", "Scrape"),
          actionButton("Scraper_Clear", "ClearScraper")
        )
      )
    )




  Server <- function(input, output, session){
    Scraper_AsOfDate <- reactive({
      return(
        list(
          valid = !is.null(input$Scraper_AsOfDate),
          value = input$Scraper_AsOfDate
        )
      );
    });

    observeEvent(
      input$Scraper_Load,
      {
        if (Console$IsSuperUser(input)) {
          text <- "";
          if (Scraper_AsOfDate()$valid){
            lines <- DropBox$GetClipboardFile(Scraper_AsOfDate()$value);
            if (length(lines) > 5){
              text <- paste(lines, collapse = "\n")
            }
          }
          updateAceEditor(session, "Scraper_Scraped", text);
        }
      }
    );

    observeEvent(
      input$Scraper_Scrape,
      {
        if (Console$IsSuperUser(input)) {
          AsOfDate <- input$Scraper_AsOfDate;
          DropBox$WriteScrapedRawFile(input$Scraper_Scraped, AsOfDate);
          Scraper$Process(AsOfDate);
        }
      },
      priority = 1
    );
}



  Initialize <- function(){
    FileSystem$CreateFolders(c("scrapes/raw", "scrapes/clean"));

  }

  return(
    list(
      UI.MenuItem = UI.MenuItem,
      UI.Page = UI.Page,
      Server = Server,
      Transform = Transform,
      Process = Process,
      InitializeFromClipboard = InitializeFromClipboard,
      InitializeAllSymbols = InitializeAllSymbols,
      InitializeAll = InitializeAll,
      ProcessClipboardFile = ProcessClipboardFile,
      ProcessClipboardFromDropBox = ProcessClipboardFromDropBox,
      ProcessClipboardFileFromDropBox = ProcessClipboardFileFromDropBox,
      Data = Data
    )

  )



}


Scraper <- .Scraper();


#Scraper$ProcessClipboard();
#
if(FALSE){


  source("bootstrap.R");


Scraper$ProcessClipboardFile("/Volumes/Pegasus/Users/Mike/Dropbox/Apps/ShinyTrades/scrapes/clipboard/scrape-clip-2016-08-02.txt");
}

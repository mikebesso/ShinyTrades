
Zones <- function(x){

  x <- try.xts(x, error = as.matrix);
  n <- nrow(x);

  res <- cbind(replicate(n, NA), replicate(n, NA));

  res[10:60, 1] <- 126;
  res[10:60, 2] <- 99;

  colnames(res)<- c("proximal","distal");

  return(reclass(res, x));

}


addZones <- function(on = 1, ...){



  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)
  x <- Zones(x = x)

  yrange <- NULL

  chobTA <- new("chobTA")

  if (NCOL(x) == 1) {
    chobTA@TA.values <- x[lchob@xsubset]
  } else {
    chobTA@TA.values <- x[lchob@xsubset, ]
  }

  chobTA@name <- "chartTA"
  if (any(is.na(on))) {
    chobTA@new <- TRUE
  }
  else {
    chobTA@new <- FALSE
    chobTA@on <- on
  }

  chobTA@call <- match.call()

  legend.name <- gsub("^add", "", deparse(match.call()))
  gpars <- c(list(...), list())[unique(names(c(list(), list(...))))]

  browser();

  chobTA@params <- list(
    xrange = lchob@xrange,
    yrange = yrange,
    colors = lchob@colors,
    color.vol = lchob@color.vol,
    multi.col = lchob@multi.col,
    spacing = lchob@spacing,
    width = lchob@width,
    bp = lchob@bp,
    x.labels = lchob@x.labels,
    time.scale = lchob@time.scale,
    isLogical = is.logical(x),
    legend = NULL,
    legend.name = NULL,
    pars = list(gpars)
  )



  if (is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA, chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new, 1,
                                            0)
    chartSeries.chob <- chartSeries.chob
    do.call("chartSeries.chob", list(lchob))
    invisible(chobTA)
  }

  else {
    return(chobTA)
  }

}


iif <- function(x, y, z){
  if (x) {
    return(y);
  } else {
    return(z);
  }
}

Max <- function(..., IgnoreMissing = TRUE){
  max(..., na.rm = IgnoreMissing);
}

Min <- function(..., IgnoreMissing = TRUE){
  min(..., na.rm = IgnoreMissing);
}


HasValue <- function(x) {
  !invalid(x);
}

HasValues <- function(x) {
  unlist(llply(x, HasValue));
}

AreLogicals <-function(x) {
  unlist(llply(x, is.logical));
}



#
# this one seems to work differently if we use X$Y versus X[Y];
#
HasNonFalseValue <- function(x) {

  HasValue(x) && iif(is.logical(x), x, TRUE);

}


HasNonFalseValues <- function(x) {

  HasValues(x) & unlist(ifelse(AreLogicals(x), x, TRUE));

}



LogicalVector <- function(length){
  vector("logical", length);
}


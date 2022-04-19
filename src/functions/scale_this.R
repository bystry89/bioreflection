scale_this <- function(x){
  #(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  (x - median(x, na.rm=TRUE)) / IQR(x, na.rm=TRUE)
}
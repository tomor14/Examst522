#' @export
#' @title Bootstrap
#' @usage bootstrap(n,x,y)
#' @keywords bootstrap correlation
#' @description Run a bootstrap for two random variables and estimate the correlation between them.
#' @param n is a positive integer of bootstrap replicates.
#' @param x is the first variable.
#' @param y is the second variable. The length of y has to be the same as the length of x.
#' @return Returns a list containing: correlation between the variables x and y, the bootstrap estimation of the correlation, the bootstrap estimation of the standard error, the bias, and the confidence interval.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples bootstrap(n=1000, x=cars$speed, y=cars$dist)


bootstrap <- function(n, x, y){
  if(n!=as.integer(n) || n < 1){
    stop("n has to be a positve integer")
  }
  if(length(x) != length(y)){
    stop("The length of x differs from the length of y")
  }
  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y have to be numeric")
  }
  Data <- rbind(x, y) # matrix containing data for both variables
  corr <- rep(0, n)
  for(j in 1:n){
    for(i in 1:length(x)){ # make the resample the same size as the original sample
      r <- sample(1:length(x), length(x), replace=T)
      Data[r]
    }
    Data
    Y <- Data[,r]
    corr[j] <- cor(Y[1,],Y[2,])
  }
  co <- cor(Data[1,],Data[2,]) # correlation between the two variables
  bootcorr <- mean(corr) # the bootstrap estimate of the correlation
  SE <- sd(corr) # standard error
  bias <- bootcorr-cor(Data[1,],Data[2,]) # bias
  CImin <- bootcorr-SE*1.96 # the start point of the confidence interval
  CImax <- bootcorr+SE*1.96 # the end point of the confidence interval
  hist(corr,  breaks=length(seq(from=min(corr), to=max(corr), by=.02)))
  abline(v=CImin, col="red")
  abline(v=CImax, col="red")
  return(list("Correlation " = co, "Bootstrap estimation correlation" = bootcorr, "Standard error" = SE, "Bias " = bias, "Confidence interval" = c(CImin,CImax)))
}


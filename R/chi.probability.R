#' @export
#' @title Chi-squared probability
#' @usage chi.probability(x, df, n)
#' @keywords chi-squared probability
#' @description Use Monte Carlo to generate a chi-squared distribution and calculate the p-value for the distribution.
#' @param x is the value for which the function calculate the p-value. x has to be numeric.
#' @param df is the degrees of freedom by generating n number of random numbers for each variable. df has to be a positive integer.
#' @param n is the number af random nubers for each variable. n has to be a positive integer.
#' @return Returns the p-value for the chi-squared distribution.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples chi.probability(x=15, df=19, n=10000)

chi.probability <- function(x, df = 1, n = 1){
  if(missing(x)){
    stop("x is missing")
  }
  if(!is.numeric(x)){
    stop("x has to be numeric")
  }
  if(df!=as.integer(df) || df < 1){
    stop("df has to be a positve integer")
  }
  if(n!=as.integer(n) || n < 1){
    stop("n has to be a positve integer")
  }
  chi <- rnorm(n)^2
  if(df > 1){
    for(i in 2:df){
      chi <- rnorm(n)^2 + chi
    }
  }
  cdf <- ecdf(chi)
  a <- 1-cdf(x)
  return(a)
}

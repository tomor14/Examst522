#' @export
#' @title Chi-squared goodness of fit
#' @usage chi.goodness(x, p)
#' @keywords chi-squared goodness of fit
#' @description Calculate the chi-squared goodness of fit value from a random variable and a vector with the probabilities for the outcome for the variable.
#' @param x is a categorical variable. It has to be numeric
#' @param p is the probability of all outcomes of x. If p is not given, the outcome has uniform probability. It has to be numeric
#' @return Returns the chi-squared goodness of fit value.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples chi.goodness(x=c(1,3,4,1,2),p=c(.2,.3,.1,.2,.2))

chi.goodness <- function(x,p){
  if(missing(x)){
    stop("x is missing")
  }
  if(!is.numeric(x)){
    stop("x has to be numeric")
  }
  if(missing(p)){
    e <- 1/length(x)*sum(x) # if p not given, the expected probability is uniform
  }
  if(!missing(p)){
    if(length(x) != length(p) ){
      if(length(p) != 1){
        stop("the length of x must be the same as the length of p, or p must be uniform")
      }
    }
    e <- p*sum(x) # the number of expected values
  }

  chi <- sum((((x-e)^2)/e)) # chi-squared statistic
  return(chi)
}

#' @export
#' @title Denseti plot
#' @usage dens.plot(x, n, method, from, to)
#' @keywords density plot naive kernel
#' @description Use the density.estimate() and plot the density function.
#' @param x is a numeric variable.
#' @param n is the number of points to the plot. n has to be a positive integer.
#' @param method is either the naive estimator or the Gaussian kernel estimator. The default is the naive estimator.
#' @param from is the point, you are plotting from.
#' @param to is the point, you are plotting to.
#' @return The function gives a plot of the density, either naive or Gaussian kernel.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples density.plot(x=cars$dist, n=300, method="naive", from=2, to=100)

dens.plot <- function(x, n=500, method="naive", from, to){
  if(missing(from)){
    from <- min(x)-1/3*sd(x)
  }
  if(missing(to)){
    to <- max(x)+1/3*sd(x)
  }
  if(method == "naive"){
    X <- rep(0, n)
    Y <- rep(0, n)
    j <- 1
    for(i in seq(from=from, to=to, by=(to-from)/(n-1))){
      X[j] <- i
      Y[j] <- density.estimate(x, i, method="naive")
      j <- j+1
    }
    plot(X,Y, type = "s") # plot a stair graph
  }
  if(method == "kernel"){
    X <- rep(0, n)
    Y <- rep(0, n)
    j <- 1
    for(i in seq(from=from, to=to, by=(to-from)/(n-1))){
      X[j] <- i
      Y[j] <- density.estimate(x, i, method="kernel")
      j <- j+1
    }
    plot(X,Y, type = "l") # plot a smooth graph
  }
}

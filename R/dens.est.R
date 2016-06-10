#' @export
#' @title Density estimate
#' @usage dens.est(x, d, h, method)
#' @keywords density estimate naive kernel
#' @description Estimate the density of a variable, either using the naive estimator og the Gaussian kernel estimator.
#' @param x is a numeric variable.
#' @param d is a point to obtain the density. d has to be a positive integer.
#' @param h is bandwith. h has to be a positive numeric.
#' @param method is either the naive estimator or the Gaussian kernel estimator. The default is the naive estimator.
#' @return If d is given, it returns the density for the point d. If d is not given, it returns a list containing the density to the following points: min, 1st quartile, median, mean, 3rd qurtile and max.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples density.estimate(x=cars$dist, d=4, h=.8, method="naive")

dens.est <- function(x, d, h, method = "naive"){
  if(method == "naive"){
    # if h is not given:
    if(missing(h)){
      h <- (max(x)-min(x))/(1+log2(length(x)))    # Calculated by Sturges (p. 283 in Statistical Computation with R)
    }
    # if d is not given:
    if(missing(d)){
      px <- c(min(x), quantile(x, .25), quantile(x, .50), mean(x), quantile(x, .75), max(x))
      py <- c(density.estimate(x, min(x), h), density.estimate(x, quantile(x, .25), h), density.estimate(x, quantile(x, .50), h), density.estimate(x, mean(x), h),density.estimate(x, quantile(x, .75), h),density.estimate(x, max(x), h))
      dens <- data.frame(px, py)
      row.names(dens) <- c("Min:", "1st quartile:", "Median:", "Mean:", "3rd quartile:", "Max:")
      colnames(dens) <- c("x", "y")
      cat("Bandwidth:", h, "\n")
      return(dens)
    }
    # if d is given:
    else{
      w <- rep(0, length(x))
      for(i in 1:length(x)){
        if(abs((d-x[i])/h) < 1){ # weigth function
          w[i] <- 1/2
        }
      }
      dens <- 1/length(x)*sum(1/h*w)
      return(dens)
    }
  }
  else if(method == "kernel"){
    # if h is not given:
    if(missing(h)){
      h <- .9*sd(x)*length(x)^(-1/5)    # Calculated by Silverman (p. 298 in Statistical Computation with R)
    }
    # if d is not given:
    if(missing(d)){
      px <- c(min(x), quantile(x, .25), quantile(x, .50), mean(x), quantile(x, .75), max(x))
      py <- c(density.estimate(x, min(x), h, method = "kernel"), density.estimate(x, quantile(x, .25), h, method = "kernel"), density.estimate(x, quantile(x, .50), h, method = "kernel"), density.estimate(x, mean(x), h, method = "kernel"),density.estimate(x, quantile(x, .75), h, method = "kernel"),density.estimate(x, max(x), method = "kernel"))
      dens <- data.frame(px, py)
      row.names(dens) <- c("Min:", "1st quartile:", "Median:", "Mean:", "3rd quartile:", "Max:")
      colnames(dens) <- c("x", "y")
      cat("Bandwidth:", h, "\n")
      return(dens)
    }
    w <- rep(0, length(x))
    for(i in 1:length(x)){
      w[i] <- 1/(sqrt(2*pi))*exp(-1/2*((d-x[i])/h)^2) # weigth function
    }
    dens <- 1/length(x)*sum(1/h*w)
    return(dens)
  }
}

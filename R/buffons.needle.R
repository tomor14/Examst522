#' @export
#' @title Buffons needle
#' @usage buffons.needle(N,l,d)
#' @keywords buffon needle
#' @description Estimating pi by simulation Buffons experiment.
#' @param N is number of needles to be thrown. N has to be a positive integer.
#' @param l is the length of the needles. the length has to be positive and numeric.
#' @param d is the distance between the lines on the floor. The distance has to be positive and numeric.
#' @return Returns the estimated value of pi by Buffons experiment.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples buffons.needle(N=1000, l=1, d=1)

buffons.needle <- function(N, l, d){
  if(N!=as.integer(N) || N < 1){
    stop("N has to be a positve integer")
  }
  if(!is.numeric(l) || l <= 0){
    stop("l has to be positve and numeric")
  }
  if(!is.numeric(d) || d <= 0){
    stop("d has to be positve and numeric")
  }
  hit <- 0
  x <- runif(N,0,d) # the position of the start point of the needle
  y <- runif(N,0,2*pi) # the angle between the x-axis and the needle

  for(i in 1:N){
    if(y[i] <= pi/2 || y[i] >= 3/2*pi){ # if the end point of the needle is in 1st or 4th quadrant
      if(x[i]+sin(y[i])*l >= d || x[i]+sin(y[i])*l <= 0){
        hit <- hit+1
      }
    }
    else{ # if the end point of the needle is in 2nd or 3rd quadrant
      if(x[i]+sin(pi-y[i])*l >= d || x[i]+sin(pi-y[i])*l <= 0){
        hit <- hit+1
      }
    }
  }
  l <- list("probability of hit:"=hit/N, "pi: "=2*l*N/(d*hit))
  return(l)
}


#' @export
#' @title Markov Chain simulation
#' @usage MC.sim(p, k, n)
#' @keywords Markov Chain stationary distribution state
#' @description Simulate the stationary discrete Markov Chain distribution.
#' @param p is the probability matrix for the Markov Chain. p has to be a n x n matrix.
#' @param k is the initial state.
#' @param n is number af steps to be simulated.
#' @return Returns the stationary probabilities for each state.
#' @author Tobias Mortensen \cr
#' Department of mathematics and computer science (IMADA) \cr
#' University of southern Denmark, Odense \cr
#' \email{tomor14@student.sdu.dk} \cr
#' @examples MC.sim(p=matrix(c(.6,.4,.4,.6), ncol=2), k=2, n=1000)

MC.sim <- function(p, k, n){
  if(missing(p)){
    stop("p is missing")
  }
  if(missing(k)){
    stop("k is missing")
  }
  if(k != as.integer(k) || k < 0){
    stop("n has to be a integer")
  }
  if(missing(n)){
    stop("n is missing")
  }
  if(n != as.integer(n) || n < 1){
    stop("n has to be a positive integer")
  }
  if(!is.matrix(p)){
    stop("p has to be a matrix")
  }
  state <- rep(0,length(p[,1]))
  state[k] <- 1 # initial state
  if(n > 0){ # if we have to go some steps
    for(i in 1:n){
      state <- state%*%p # probability for the new state
    }
  }
  return(state)
}

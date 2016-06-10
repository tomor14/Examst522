#' @export
#' @title Linear regression model
#' @usage density.plot(x, n, method, from, to)
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
#' @examples lin.model(cars$dist~cars$speed)

lin.model <- function(formula){
  Y <- eval(as.name(all.vars(formula)[1])) # the dependent variable
  X <- all.vars(formula)[2:length(all.vars(formula))] # the predictors
  f <- unlist(strsplit(X, " ")) # split the predictors
  W <- c()
  for(i in 1:length(f)){
    q <- as.name(f[i])
    W <- append(W, eval(as.name(f[i])))
  }

  M <- matrix(c(rep(1,length(Y)), W), ncol=length(f)+1)

  n <- dim(M)[1]
  p <- dim(M)[2]-1

  B <- solve(t(M)%*%M)%*%(t(M)%*%Y) # beta
  Yhat <- M%*%B # estimated Y
  residual <- Y-Yhat # residuals
  RSE <- sd(residual) # residuals standard error
  RSS <- sum(residual^2) # residuals sum of squares
  SST <- sum((Y-mean(Y))^2) # total corrected sum of squares
  R2 <- 1-RSS/SST # R^2
  R2adj <- 1-(RSS/(n-p-1))/(SST/(n-1)) # ajusted R^2
  SSreg <- sum((Yhat-mean(Y))^2) # regression sum of squares
  Fstat <- (SSreg/p)/(RSS/(n-1-p)) # f-statistic
  df <- n-p-1 # degrees of freedom
  pval <- 1-pf(Fstat, p, n-p-1) # p-values
  EV <- RSS/(n-p-1) # error variance
  SE <- sqrt(diag(EV*solve(t(M)%*%M)))  # standard error
  tVal <- B/SE # t-values
  pr <- 2*(1-pt(abs(tVal), df)) # p-values

  print(list("Residuales" = data.frame("Min" = mean(residual), "1st qu" = quantile(residual, .25), "Median" = quantile(residual, .5), "3st qu" = quantile(residual, .75), "Max" = max(residual), row.names=" "),
            "Coefficints" = data.frame("Estimate" = B, "Standard error" = SE, "t value" = tVal, "Pr(<|t|)" = pr)))
  cat("Residual standard error: ", RSE, "on", df, "degrees of freedom \n")
  cat("Multiple R-squared: ", R2, "Adjusted R-squares: ", R2adj, "\n")
  cat("F-statistic: ", Fstat, "on", p, "and", df, "df", "p-value: ", pval)
}


# portfolio.r
# 
# Functions for portfolio analysis
# last updated: September 25, 2016 by Ismael Jimenez
#
# Functions:
# 1. efficient.portfolio.risk    compute minimum variance portfolio
#                                subject to target risk

stopifnot("package:quadprog" %in% search()  ||  require("quadprog",quietly = TRUE) )

source("http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r")

efficient.portfolio.risk <- function(er, cov.mat, target.risk, shorts=TRUE)
{
  incr <- 0.00001
  e.port <- globalMin.portfolio(er, cov.mat, shorts)
  currentEr <- as.numeric(e.port["er"])
  currentSd <- as.numeric(e.port["sd"])
  
  while(currentSd < target.risk) {
    currentEr <- currentEr+incr
    e.port <- efficient.portfolio(er, covmat, currentEr, shorts)
    currentEr <- as.numeric(e.port["er"])
    currentSd <- as.numeric(e.port["sd"])
  }
  e.port
}

# efficient-frontier-large-small-value-growth.r
# 
# Calculate efficient portfolios of risky assets
# to be used in Investments I: Fundamentals of Performance Evaluation
# last updated: September 25, 2016 by Ismael Jimenez

options(digits=4, width=70)
source("https://raw.githubusercontent.com/ismaelJimenez/portfolio-management/master/libraries/portfolio_optimization.r")

# Model Inputs
# Source for U.S. Stock Returns and U.S. Treasury Bill Rates: Kenneth R. French Data Library
# The Asia Pacific portfolio (excluding Japan) includes Australia, Hong Kong, New Zealand, and Singapore.
# The Europe portfolio include Austria, Belgium, Denmark, Finland, France, Germany, Greece, Ireland, Italy, the Netherlands, Norway, Portugal, Spain, Sweden, Switzerland, and the United Kingdom
asset.names = c("US", "Japan", "Asia (non-Japan)", "Europe")
er = c(0.0089, 0.0021, 0.0096, 0.0074)
sd = c(0.0434, 0.0597, 0.0601, 0.0504)
corrmat = matrix(c( 1.00,	0.41,	0.71,	0.79,
                    0.41,	1.00,	0.47,	0.50,
                    0.71,	0.47,	1.00,	0.75,
                    0.79,	0.50,	0.75,	1.00),
                 nrow=4, ncol=4)
r.free = 0.0035

# Calculate Covariance Matrix
names(er) = asset.names
names(sd) = asset.names
covmat = sd %*% t(sd) * corrmat
dimnames(corrmat) = list(asset.names, asset.names)
dimnames(covmat) = list(asset.names, asset.names)

# Question 1.  Suppose that you're currently 100% invested in us stocks and you CANNOT SHORT.
# What portfolio maximizes expected return subject to having the same risk level of large stocks? 
# What's the expected return in this case? And what are the portfolio ?
# Compute efficient portfolio subject to target return
target.risk = sd["US"]
e.port.long <- efficient.portfolio.risk(er=er,cov.mat=covmat, target.risk=target.risk, shorts=FALSE)
summary(e.port.long, risk.free=r.free)
plot(e.port.long, col="blue")

stopifnot(as.numeric(e.port.long["er"]) > 0.009 && as.numeric(e.port.long["er"]) < 0.01)
stopifnot(as.numeric(e.port.long["sd"]) > 0.043 && as.numeric(e.port.long["sd"]) < 0.044)

# Question 2. What is the expected return and standard deviation of the portfolio of risky assets
# that maximizes Sharpe Ratio (with NO SHORT sales)?
# Compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er=er,cov.mat=covmat, risk.free=r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

stopifnot(as.numeric(tan.port.ns["er"]) > 0.009 && as.numeric(tan.port.ns["er"]) < 0.01)
stopifnot(as.numeric(tan.port.ns["sd"]) > 0.043 && as.numeric(tan.port.ns["sd"]) < 0.044)

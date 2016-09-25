# efficient-frontier-large-small-value-growth.r
# 
# Calculate efficient portfolios of risky assets
# to be used in Investments I: Fundamentals of Performance Evaluation
# last updated: September 25, 2016 by Ismael Jimenez

options(digits=4, width=70)
source("https://raw.githubusercontent.com/ismaelJimenez/portfolio-management/master/libraries/portfolio_optimization.r")

# Model Inputs
# Source for U.S. Stock Returns and U.S. Treasury Bill Rates: Kenneth R. French Data Library
asset.names = c("Large", "Small", "Value", "Growth")
er = c(0.112, 0.19, 0.175, 0.111)
sd = c(0.192, 0.394, 0.334, 0.225)
corrmat = matrix(c(1   , 0.69, 0.8 , 0.94,
                   0.69, 1   , 0.84, 0.65,
                   0.8 , 0.84, 1   , 0.7 ,
                   0.94, 0.65, 0.7 , 1),
                 nrow=4, ncol=4)
r.free = 0.0035

# Calculate Covariance Matrix
names(er) = asset.names
names(sd) = asset.names
covmat = sd %*% t(sd) * corrmat
dimnames(corrmat) = list(asset.names, asset.names)
dimnames(covmat) = list(asset.names, asset.names)

# Question 1-1. What is the expected return and standard deviation of a portfolio that invests 25%
# in each of the four assets (with NO SHORT sales)?
#Compute equally weighted portfolio
ew = rep(1,4)/4
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
class(equalWeight.portfolio)
names(equalWeight.portfolio)
equalWeight.portfolio
summary(equalWeight.portfolio)
plot(equalWeight.portfolio, col="blue")

# Question 1-2. Find the portfolio that maximizes expected return given the portfolio risk of 
# 25/25/25/25 portfolio (with NO SHORT sales). What is the expected return and portfolio weights 
# in this case?
# Compute efficient portfolio subject to target return
target.risk = as.numeric(equalWeight.portfolio["sd"])
e.port.long <- efficient.portfolio.risk(er=er,cov.mat=covmat, target.risk=target.risk, shorts=FALSE)
summary(e.port.long, risk.free=r.free)
plot(e.port.long, col="blue")

# Question 2. Find the portfolio that maximizes expected return given the portfolio risk of 
# 25/25/25/25 portfolio (with SHORT sales). What is the expected return and portfolio weights in 
# this case?
# Compute efficient portfolio subject to target return with short sales
target.risk = as.numeric(equalWeight.portfolio["sd"])
e.port.long.short <- efficient.portfolio.risk(er=er,cov.mat=covmat, target.risk=target.risk)
summary(e.port.long.short, risk.free=r.free)
plot(e.port.long.short, col="blue")

# Question 3. What is the expected return and standard deviation of the portfolio of risky assets
# that minimizes portfolio variance (with NO SHORT sales)?
# Compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er=er,cov.mat=covmat, shorts=FALSE)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue")

# Question 4. What is the expected return and standard deviation of the portfolio of risky assets
# that maximizes Sharpe Ratio (with NO SHORT sales)?
# Compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er=er,cov.mat=covmat, risk.free=r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

# efficient-frontier-large-small-value-growth.r
# 
# Calculate efficient portfolios of risky assets
# to be used in Investments I: Fundamentals of Performance Evaluation
# last updated: September 25, 2016 by Ismael Jimenez

options(digits=4, width=70)
source("https://raw.githubusercontent.com/ismaelJimenez/portfolio-management/master/libraries/portfolio_optimization.r")

# Model Inputs
# Source for U.S. Stock Returns and U.S. Treasury Bill Rates: Kenneth R. French Data Library
# Source for Gold Prices: Deutsche Bundesbank Data Repository
asset.names = c("Large", "Small", "Value", "Growth", "Gold")
er = c(0.0101, 0.0135, 0.0151, 0.0095, 0.0054)
sd = c(0.043, 0.061, 0.0594, 0.0513, 0.0566)
corrmat = matrix(c( 1.00,	0.65,	0.75,	0.93,	-0.01,
                    0.65,	1.00,	0.75,	0.69,	0.07,
                    0.75,	0.75,	1.00,	0.65,	0.01,
                    0.93,	0.69,	0.65,	1.00,	0.00,
                    -0.01,0.07,	0.01,	0.00,	1.00),
                 nrow=5, ncol=5)
r.free = 0.0035

# Calculate Covariance Matrix
names(er) = asset.names
names(sd) = asset.names
covmat = sd %*% t(sd) * corrmat
dimnames(corrmat) = list(asset.names, asset.names)
dimnames(covmat) = list(asset.names, asset.names)

# Question 1.  Suppose that you're currently 100% invested in large stocks and you CANNOT SHORT.
# What portfolio maximizes expected return subject to having the same risk level of large stocks? 
# What's the expected return in this case? And what are the portfolio ?
# Compute efficient portfolio subject to target return
target.risk = sd["Large"]
e.port.long <- efficient.portfolio.risk(er=er,cov.mat=covmat, target.risk=target.risk, shorts=FALSE)
summary(e.port.long, risk.free=r.free)
plot(e.port.long, col="blue")

stopifnot(as.numeric(e.port.long["er"]) > 0.011 && as.numeric(e.port.long["er"]) < 0.012)
stopifnot(as.numeric(e.port.long["sd"]) > 0.043 && as.numeric(e.port.long["sd"]) < 0.044)

# Question 2.  Suppose that you're currently 100% invested in large stocks and you CAN SHORT.
# What portfolio maximizes expected return subject to having the same risk level of large stocks? 
# What's the expected return in this case? And what are the portfolio ?
# Compute efficient portfolio subject to target return
target.risk = sd["Large"]
e.port.long <- efficient.portfolio.risk(er=er,cov.mat=covmat, target.risk=target.risk, shorts=TRUE)
summary(e.port.long, risk.free=r.free)
plot(e.port.long, col="blue")

stopifnot(as.numeric(e.port.long["er"]) > 0.012 && as.numeric(e.port.long["er"]) < 0.013)
stopifnot(as.numeric(e.port.long["sd"]) > 0.043 && as.numeric(e.port.long["sd"]) < 0.044)

# Question 3. What is the expected return and standard deviation of the portfolio of risky assets
# that maximizes Sharpe Ratio (with NO SHORT sales)?
# Compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er=er,cov.mat=covmat, risk.free=r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

stopifnot(as.numeric(tan.port.ns["er"]) > 0.013 && as.numeric(tan.port.ns["er"]) < 0.014)
stopifnot(as.numeric(tan.port.ns["sd"]) > 0.049 && as.numeric(tan.port.ns["sd"]) < 0.050)


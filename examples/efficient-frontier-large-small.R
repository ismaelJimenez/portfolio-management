# efficient-frontier-large-small.r
# 
# Calculate efficient portfolios of risky assets
# last updated: September 25, 2016 by Ismael Jimenez

options(digits=4, width=70)
source("https://raw.githubusercontent.com/ismaelJimenez/portfolio-management/master/libraries/portfolio_optimization.r")

# Model Inputs
asset.names = c("Large", "Small")
er = c(0.08, 0.15)
sd = c(0.25, 0.5)
corrmat = matrix(c(1, 0.4,
                   0.4, 1),
                 nrow=2, ncol=2)
r.free = 0.005

# Calculate Covariance Matrix
names(er) = asset.names
names(sd) = asset.names
covmat = sd %*% t(sd) * corrmat
dimnames(corrmat) = list(asset.names, asset.names)
dimnames(covmat) = list(asset.names, asset.names)

# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat, FALSE)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue")

# compute efficient portfolio subject to target return
target.risk = sd["Large"]
e.port.long <- efficient.portfolio.risk(er, covmat, target.risk, FALSE)
summary(e.port.long, risk.free=r.free)
plot(e.port.long, col="blue")

# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

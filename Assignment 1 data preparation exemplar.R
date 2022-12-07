#----------------------------------------------------------------------------
# ECMT2130 Financial Econometrics
# Assignment 1: Data preparation script
#----------------------------------------------------------------------------

#install.packages(c("xts"))
library(xts)
load("Assignment 1 dataset 300.RData")
N <- ncol(fundTotalReturnIndices)

# Make sure data is what I expect
# summary()
# plot()

# Compute rates of return from total return indices
# simple rate of return V(t) / V(t-1) - 1

laggedFundTotalReturnIndices <- lag(fundTotalReturnIndices, k = 1)
fundRatesOfReturn <- na.omit(fundTotalReturnIndices / laggedFundTotalReturnIndices - 1)

excessFundRatesOfReturn <- fundRatesOfReturn
for (fund in 1:N) {
  excessFundRatesOfReturn[,fund] <- fundRatesOfReturn[,fund] - riskFreeRateOfReturn
}

meanFundExcessReturns <- colMeans(excessFundRatesOfReturn)

varCovForFund <- var(excessFundRatesOfReturn)

r_f <- tail(riskFreeRateOfReturn, n = 1)

meanFundReturns <- meanFundExcessReturns + r_f[[1]]

write.csv(varCovForFund, "variance.csv", row.names = TRUE)

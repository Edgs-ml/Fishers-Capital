##### Load Packages #####
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(fBasics)
library(ghyp)

##### Define the tokens of the assets #####
Symbols_1 <- c("AMZN", "WM", "NVS", "SAN", "BIMBOA.MX", "C", "NVDA", "KC=F")

getSymbols(Symbols_1, from=as.Date("2017-02-05"), to=Sys.Date())

##### Combine the Adjusted Prices into a single matrix #####
Symbols_1_ad <- NULL
for(i in Symbols_1){
  assign(paste0("Ad_",i), Ad(get(i)))
  Symbols_1_ad <- c(Symbols_1_ad, paste0("Ad_",i))
}

Portfolio_1 <- merge.xts(get(Symbols_1_ad[1]),
                       get(Symbols_1_ad[2]),
                       join = "inner")
for(i in 3:length(Symbols_1_ad)){
  Portfolio_1 <- merge.xts(Portfolio_1,
                         get(Symbols_1_ad[i]),
                         join = "inner")
}
colnames(Portfolio_1) <- Symbols_1

##### Compute the returns of the portfolio #####
Returns_1 <- Return.calculate(Portfolio_1)[-1,]


##### Create the Portfolio Object #####
Specs_Port1 <- portfolio.spec(Symbols_1)

##### Add Constraints #####
Specs_Port1 <- add.constraint(Specs_Port1,type="full_investment")
Specs_Port1 <- add.constraint(Specs_Port1,type="long_only")

##### Add Objective #####
Specs_Port1 <- add.objective(Specs_Port1,type="risk",name="StdDev")
Specs_Port1 <- add.objective(Specs_Port1,type='return',name='mean')
Specs_Port1

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}
covnig(Returns_1)


##### Optimization #####
Optimized_Port1 <- optimize.portfolio(Returns_1,
                                      Specs_Port1, 
                                      momentFUN = covnig,
                                      optimize_method = "random",
                                      trace = TRUE)
Optimized_Port1


chart.Weights(Optimized_Port1, plot.type = "barplot")
W1 <- extractWeights(Optimized_Port1)
W1
sum(W1)

cor.plot(Returns_1)
##### Portfolio Evaluation #####
Return_opt1 <- Return.portfolio(Returns_1, W1)

table.AnnualizedReturns(Return_opt1,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt1,
                  geometric = FALSE)

varRisk(Portfolio_1, W1)

DtaFrm_Portfolio1 <- as.data.frame(Portfolio_1)

chart.RiskReward(Optimized_Port1,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE)
combine.portfolios(Optimized_Port1)

chart.EfficientFrontierOverlay(Optimized_Port1,
                               risk.col = 'StdDev',
                               return.col = 'mean',
                               chart.assets = TRUE)

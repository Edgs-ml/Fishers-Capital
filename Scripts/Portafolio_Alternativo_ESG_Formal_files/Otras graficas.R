##### Load Packages #####
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(fBasics)
library(ghyp)

##### Define the tokens of the assets #####
Symbols_4 <- c("AMZN", "WM", "NVS", "SAN", "BIMBOA.MX", "C", "NVDA", "SI=F")

getSymbols(Symbols_4, from=as.Date("2017-02-05"), to=Sys.Date())

##### Combine the Adjusted Prices into a single matrix #####
Symbols_4_ad <- NULL
for(i in Symbols_4){
  assign(paste0("Ad_",i), Ad(get(i)))
  Symbols_4_ad <- c(Symbols_4_ad, paste0("Ad_",i))
}

Portfolio_4 <- merge.xts(get(Symbols_4_ad[1]),
                       get(Symbols_4_ad[2]),
                       join = "inner")
for(i in 3:length(Symbols_4_ad)){
  Portfolio_4 <- merge.xts(Portfolio_4,
                         get(Symbols_4_ad[i]),
                         join = "inner")
}
colnames(Portfolio_4) <- Symbols_4

##### Compute the returns of the portfolio #####
Returns_4 <- Return.calculate(Portfolio_4)[-1,]


##### Create the Portfolio Object #####
Specs_Port4 <- portfolio.spec(Symbols_4)

##### Add Constraints #####
Specs_Port4 <- add.constraint(Specs_Port4,
                              type="weight_sum", min = 0, mx = 1)
Specs_Port4 <- add.constraint(Specs_Port4,type="long_only")

##### Add Objective #####
Specs_Port4 <- add.objective(Specs_Port4,type="risk",name="StdDev")
Specs_Port4 <- add.objective(Specs_Port4,type='return',name='mean')
Specs_Port4

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}
covnig(Returns_4)


##### Optimization #####
Optimized_Port4 <- optimize.portfolio(Returns_4,
                                      Specs_Port4, 
                                      momentFUN = covnig,
                                      optimize_method = "random",
                                      trace = TRUE)
Optimized_Port4


chart.Weights(Optimized_Port3, plot.type = "barplot")
W3 <- extractWeights(Optimized_Port3)
W3
sum(W3)

cor.plot(Returns_3)
##### Portfolio Evaluation #####
Return_opt3 <- Return.portfolio(Returns_3, W3)

table.AnnualizedReturns(Return_opt3,
                        scale = 252,
                        geometric = FALSE)
Return.cumulative(Return_opt3,
                  geometric = FALSE)

plot(cumsum(Return_opt3))

varRisk(Portfolio_3, W3)

DtaFrm_Portfolio2 <- as.data.frame(Portfolio_2)

chart.RiskReward(Optimized_Port3,
                 risk.col = 'StdDev',
                 return.col = 'mean',
                 chart.assets = TRUE)
combine.portfolios(Optimized_Port2)

chart.EfficientFrontierOverlay(Optimized_Port1,
                               risk.col = 'StdDev',
                               return.col = 'mean',
                               chart.assets = TRUE)
#---------------
# Define the tokens of the FIBRAS
Symbols_2 <- c("FMTY14.MX", "FUNO11.MX", "FIBRAPL14.MX", "FIBRAMQ12.MX")

getSymbols(Symbols_1, from=as.Date("2017-02-05"), to=Sys.Date())





# Comparación
#--------------Graficas comparativas

rand <- Optimized_Port1$random_portfolios
# 1794 portafolios de diferentes pesos al asar 

stdv <- Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$StdDev

medias <- Optimized_Port1$random_portfolio_objective_results[[1]]$objective_measures$mean


#--------
mediasNORM <- NULL
standDevNORM <- NULL
for (i in 1:1682) {
  mediasNORM[i] <- Optimized_Port_Normal$random_portfolio_objective_results[[i]]$objective_measures$mean
  standDevNORM[i] <- Optimized_Port_Normal$random_portfolio_objective_results[[i]]$objective_measures$StdDev
}

mediasNIG <- NULL
standDeviNIG <- NULL
for (i in 1:1768) {
  mediasNIG[i] <- Optimized_Port_NIG$random_portfolio_objective_results[[i]]$objective_measures$mean
  standDeviNIG[i] <- Optimized_Port_NIG$random_portfolio_objective_results[[i]]$objective_measures$StdDev
}
#-------- Fronteras

fronteraNorm <- tibble(Volatility = standDevNORM, Expected_Return = mediasNORM)
fronteraNIG <- tibble(Volatility = standDeviNIG, Expected_Return = mediasNIG)

fronteraNorm %>%
  ggplot(aes(x=Volatility, y=Expected_Return))+
  geom_point(alpha=0.5, col="pink")+
  geom_point(data=tibble(Volatility = Optimized_Port_Normal$objective_measures$StdDev,
                         Expected_Return = Optimized_Port_Normal$objective_measures$mean),
             col="red", size=3)+
  geom_point(data=fronteraNIG, aes(x=Volatility, y=Expected_Return), alpha=0.2, col="blue")+
  geom_point(data=tibble(Volatility = Optimized_Port_NIG$objective_measures$StdDev, 
                         Expected_Return = Optimized_Port_NIG$objective_measures$mean), 
             alpha=1, col="darkgreen", size=3)




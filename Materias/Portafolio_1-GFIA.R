title: "Portafolio_NIG_Evidencia1"
author: "Edgar Sigfrido Soto Aparicio"
date: "3/29/2022"

library(tidyverse)
library(quantmod)
library(PortfolioAnalytics)
library(fPortfolio)
library(PerformanceAnalytics)
library(readr)
library(psych)
library(DEoptim)
library(visdat)
-------
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(fBasics)
library(ghyp)
library(knitr)


Alternatives <- c("BSMXB.MX", "VOLARA.MX", "TLEVISACPO.MX", "FEMSAUBD.MX",
                  "CEMEXCPO.MX", "LABB.MX", "AGUA.MX", "KIMBERA.MX", 
                  "NEMAKA.MX","KC=F", "SB=F", "GC=F", "SI=F", "ZW=F",
                  "FMTY14.MX", "FUNO11.MX", "FIBRAPL14.MX", "FIBRAMQ12.MX")
#no deja meter fibra Macquaire

getSymbols(Alternatives, from=as.Date("2017-02-05"), to=Sys.Date())
Santander <- Ad(BSMXB.MX)
Volaris <- Ad(VOLARA.MX)
Televisa <- Ad(TLEVISACPO.MX)
FEMSA <- Ad(FEMSAUBD.MX)
Cemex <- Ad(CEMEXCPO.MX)
Genoma <- Ad(LABB.MX)
Rotoplas <- Ad(AGUA.MX)
Kimberly <- Ad(KIMBERA.MX)
Nemaka <- Ad(NEMAKA.MX)
Coffee <- Ad(`KC=F`)
Sugar <- Ad(`SB=F`)
Gold <- Ad(`GC=F`)
Silver <- Ad(`SI=F`)
Wheat <- Ad(`ZW=F`)
FibraMTY <- Ad(FMTY14.MX)
FibraUNO <- Ad(FUNO11.MX)
FibraPrologis <- Ad(FIBRAPL14.MX)
FibraMacquaire <- Ad(FIBRAMQ12.MX)

AlternativesAd <- merge.xts(Santander, Volaris, Televisa, FEMSA, Cemex, Genoma, 
                            Rotoplas, Kimberly, Nemaka, Coffee, Sugar, Gold,
                            Silver, Wheat, FibraMTY, FibraUNO, FibraPrologis, 
                            FibraMacquaire,
                            join = "inner")
#interseccion, solo toma los días donde todos cotizan

AlternativesReturns <- Return.calculate(AlternativesAd, method = "log")[-1] #logN(x2/x1)

covnig<-function(R,portfolio){
  a<-fit.NIGmv(R,silent=TRUE)
  COV<-a@variance
  mu<-a@expected.value
  mu<-matrix(mu,ncol = 1)
  resultado<-list(mu=mu,sigma=COV)
  return(resultado)
}
covnig(AlternativesReturns)





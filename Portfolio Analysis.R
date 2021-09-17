install.packages("PerformanceAnalytics")
install.packages("PortfolioAnalytics")
install.packages("dygraphs")
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(xts)
library(PortfolioAnalytics)
library(dygraphs)


setDefaults(getSymbols.yahoo, from = as.Date('2019/01/01'), to= as.Date('2020/10/14'))
IDX = c('INFY','TCS')
length(IDX)
weight = rep.int(x = 1/length(IDX),times = length(IDX))
getSymbols(Symbols = IDX)
head(INFY)
list_index = list(INFY,TCS)
names(list_index) <- IDX
Index <- lapply(list_index, '[', i =,j = 6)
##Index_1 <- lapply(list_index, FUN ='[',i =,j = 6)
names(Index)
#do.call(cbind.data.frame, Index)
Index_1 <- do.call(what = cbind.data.frame, args = Index)
names(Index_1) <- IDX
#Index_final <- as.xts(Index_1)
head(Index_1)
##############################################################
index_retun = Return.calculate(Index_1)
head(index_retun)

dygraph(data = index_retun,main = "Stocks progress") %>% 
  dyAxis("y",label = "%") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(2, "Set2"))
  #dyOptions(colors = RColorBrewer::brewer.pal(n = 5,"Set8"))


portfolio_monthly_returns <- Return.portfolio(index_retun, weights = weight)

dygraph(portfolio_monthly_returns, main = "Portfolio Monthly Return") %>%
  dyAxis("y", label = "%")

money_growth = Return.portfolio(R = index_retun,weights = weight,wealth.index = TRUE)

dygraph(money_growth, main = "Growth of 1 rupee Invested in Portfolio") %>%
  dyAxis("y", label = "Rs")

##############################################################
portfolio_excess_returns <- Return.excess(portfolio_monthly_returns, Rf = .0003)

sharpe_ratio_manual <- round(mean(portfolio_excess_returns)/StdDev(portfolio_excess_returns), 4)
sharpe_ratio <- round(SharpeRatio(portfolio_monthly_returns, Rf = .0003), 4)

sharpe ratio
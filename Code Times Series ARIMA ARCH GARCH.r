### Importer les packages ###

packages =  c("ggplot2", "dplyr", "tidyr", "data.table", 'corrplot', 'gridExtra', 'forecast', 'tseries', 'TSA', 'tibble', 'TTR', 'xts', 'dygraphs', 'assertthat')

my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

purrr::walk(packages, my.install, character.only = TRUE, warn.conflicts = FALSE)

### Lecture des données ###

s_data <- read.csv(file ="H:/Desktop/TS_DATA.csv")

summary(s_data)

str(s_data)

### Nettoyage des données ###

s_data[is.na(s_data)] <- 0
s_data$Date <- as.Date(s_data$Date, format = "%Y-%m-%d")
summary(s_data)

str(s_data)


### Distributions univariées ###

options(repr.plot.width=12, repr.plot.height=12) 

p1 = ggplot(s_data, aes(Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p2 = ggplot(s_data, aes(High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p3 = ggplot(s_data, aes(Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

p4 = ggplot(s_data, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)


### Analyse TS ###

sample_num = 5

tmp <- filter(s_data, High > 100)
length(unique(tmp$Name))

assert_that(sample_num < length(unique(tmp$Name)))

sample_ticker <- as.character(sample(tmp$Name, sample_num))
sample_ticker <- c(sample_ticker, 'GOOGL') 
candidate_ticker <- unique(sample_ticker)
candidate_ticker <- c("IBM", "BA", "AAPL", "GS", "GOOGL")
candidate_num <- length(candidate_ticker)
stock_list <- vector(mode="list", length=candidate_num)
names(stock_list) <- candidate_ticker
i = 1
for (ticker in candidate_ticker){
  stock_list[[i]] <- filter(s_data, Name == ticker)
  # print(stock_list[[i]])
  i <- i+1
  # print(ticker)
}
str(stock_list)

### Créer et tracer des séries temporelles - High ###


xts_list <- vector(mode="list", length=candidate_num)
ts_list <- vector(mode="list", length=candidate_num)

names(xts_list) = candidate_ticker
names(ts_list) = candidate_ticker

for (ticker in candidate_ticker){
  stock = stock_list[[ticker]]
  xts = xts(stock$Close, order.by=stock$Date)
  attr(xts, 'frequency') <- length(xts)/12
  ts = as.ts(xts, start = c(2006))
  xts_list[[ticker]] <- xts
  ts_list[[ticker]] <- ts
}
xts_table= do.call(cbind, xts_list)
dygraph(xts_table, xlab = "Time", ylab = "High value", main = "Séries temporelles") %>%
  # dySeries(labels.default()) %>%
  # dyOptions(colors = c("red")) %>%
  dyRangeSelector()

### Stationnarité ###


xts = xts_list[['GOOGL']]
ts = ts_list[['GOOGL']]
adf.test(xts, alternative = "stationary", k = 0)


### Décomposition des séries temporelles ###

tscomponents_add <- decompose(ts, type = "additive")
tscomponents_mul <- decompose(ts, type = "multiplicative")

plot(tscomponents_add, col = "red")

plot(tscomponents_mul, col = "blue")

### Différencier une série temporelle ###

xtsdiff1 <- diff(xts, differences=1)
tsdiff1 <- diff(ts, differences=1)
plot.xts(xtsdiff1, col = "blue")

adf.test(tsdiff1, alternative = "stationary", k = 0)

findfrequency(xts)         

findfrequency(xtsdiff1)

### Modele ARIMA ###

### Sélection d'un modèle ARIMA candidat ###

Acf(xtsdiff1, lag.max=60)

Acf(xtsdiff1, lag.max=60, plot=FALSE)

Pacf(xtsdiff1, lag.max=60)  

Pacf(xtsdiff1, lag.max=60, plot=FALSE)

### Fitting d'un modèle ARIMA ###

tsarima240 <- auto.arima(head(xts, -240), max.p = 3, max.q = 3, max.d = 3) 
print(tsarima240)

autoplot(tsarima240)

tsarima120 <- auto.arima(head(xts, -120), max.p = 3, max.q = 3, max.d = 3) 
print(tsarima120)

autoplot(tsarima120)


tsarima60 <- auto.arima(head(xts, -60), max.p = 3, max.q = 3, max.d = 3)
print(tsarima60)

autoplot(tsarima60)

tsarima30 <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3) 
print(tsarima30)

autoplot(tsarima30)

tsarima7 <- auto.arima(head(xts, -7), max.p = 3, max.q = 3, max.d = 3)  
print(tsarima7)

autoplot(tsarima7)

### Prévision à l'aide d'un modèle ARIMA ###

tsforecasts240 <- forecast(tsarima240, h = 240) # prévoir les 240 prochaines séries temporelles
tsforecasts120 <- forecast(tsarima120, h = 120) # prévoir les 120 prochaines séries temporelles
tsforecasts60 <- forecast(tsarima60, h = 60) # prévoir les 60 prochaines séries temporelles
tsforecasts30 <- forecast(tsarima30, h = 30) # prévoir les 30 prochaines séries temporelles
tsforecasts7 <- forecast(tsarima7, h = 7) # prévoir les 7 prochaines séries temporelles

autoplot(tsforecasts240)

accuracy(tsforecasts240, head(tail(xts, 240), 240))

accuracy(tsforecasts240, head(tail(xts, 240), 120))

accuracy(tsforecasts240, head(tail(xts, 240), 60))

accuracy(tsforecasts240, head(tail(xts, 240), 30))

accuracy(tsforecasts240, head(tail(xts, 240), 7))



autoplot(tsforecasts120)

accuracy(tsforecasts120, head(tail(xts, 120), 120))

accuracy(tsforecasts120, head(tail(xts, 120), 60))

accuracy(tsforecasts120, head(tail(xts, 120), 30))

accuracy(tsforecasts120, head(tail(xts, 120), 7))


autoplot(tsforecasts60)

accuracy(tsforecasts60, head(tail(xts, 60), 60))

accuracy(tsforecasts60, head(tail(xts, 60), 30))

accuracy(tsforecasts60, head(tail(xts, 60), 7))


autoplot(tsforecasts30)

accuracy(tsforecasts30, head(tail(xts, 30), 30))

accuracy(tsforecasts30, head(tail(xts, 30), 7))


autoplot(tsforecasts7)

accuracy(tsforecasts7, head(tail(xts, 7), 7))


### Analyse des prédiction des series temporelles ###


ggplot(data.frame(residuals = tsforecasts240$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()
checkresiduals(tsforecasts240)


ggplot(data.frame(residuals = tsforecasts120$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()
checkresiduals(tsforecasts120)

ggplot(data.frame(residuals = tsforecasts60$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()
checkresiduals(tsforecasts60)

### Tester la fonction arima_modeling sur les autres entreprises ###

arima_modeling <- function(xts, ts, ticker){
  ## Stationarity
  print(ticker)
  adf.test(xts, alternative = "stationary", k = 0)
  
  ## Decomposing Time Series
  tscomponents <- decompose(ts)
  plot(tscomponents, col = "red")
  
  ## Differencing a Time Series
  xtsdiff1 <- diff(xts, differences=1)
  tsdiff1 <- diff(ts, differences=1)
  plot.xts(xtsdiff1, col = "blue")
  findfrequency(xts)          # find dominant frequency of original time series
  findfrequency(xtsdiff1)     # find dominant frequency of differenced time series
  
  ## Selecting a Candidate ARIMA Model
  print(ticker)
  print("Selecting a candidate ARIMA Model")
  Acf(xtsdiff1, lag.max=60)             # plot a correlogram
  Acf(xtsdiff1, lag.max=60, plot=FALSE) # get the autocorrelation values
  
  Pacf(xtsdiff1, lag.max=60)             # plot a partial correlogram
  Pacf(xtsdiff1, lag.max=60, plot=FALSE) # get the partial autocorrelation values
  
  ## Fitting an ARIMA Model
  tsarima <- auto.arima(head(xts, -30), max.p = 3, max.q = 3, max.d = 3)
  
  # excluding last 120 time series as test data
  print(tsarima)
  autoplot(tsarima)
  print(ticker)
  
  ## Forecasting using an ARIMA Model
  print(ticker)
  tsforecasts <- forecast(tsarima, h = 30) # forecast the next 120 time series
  acc <- accuracy(tsforecasts, head(tail(xts, 30), 7))
  print(acc)
  autoplot(tsforecasts)
  
  print(ticker)
  
  ggplot(data.frame(residuals = tsforecasts$residuals), aes(residuals)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# make a histogram
  checkresiduals(tsforecasts)
}

for (ticker in candidate_ticker){
  if (ticker != 'GOOGL'){
    arima_modeling(xts_list[[ticker]], ts_list[[ticker]], as.character(ticker))
  }
}

### ARCH et GARCH ###

install.packages("tidyverse")

# Chargement les librairies

library(tidyverse)
library(quantmod)

getSymbols(Symbols = "IBM", src = "yahoo", 
           from = as.Date("2004-01-01"), to = as.Date("2019-03-01"))


class(IBM)

head(IBM)

dim(IBM)


# paramètres graphiques, res pour résolution
options(repr.plot.res = 300, repr.plot.height = 3) 

plot(IBM[, "IBM.Open"], main = "Prix d'ouverture de l'action IBM",
     col = "lightblue")


# paramètres graphiques, res pour résolution
options(repr.plot.res = 300, repr.plot.height = 4.4)

plot.xts(IBM[,1:4],legend.loc = "left", main = "Prix de l'action IBM",
         col = rainbow(4))


# parametres graphiques, res pour résolution
options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(IBM[,2:3],legend.loc = "left", main = "Prix plus haut et plus bas",
         col = rainbow(n= 2))

plot(IBM[, "IBM.Volume"]/1000000, col = "lightblue", main = "Volume d'actions échangées en millions")


# parametres graphiques, res pour resolution
options(repr.plot.res = 300, repr.plot.height = 5) 
candleChart(IBM, type = "line", theme = "white", show.grid = T)

### modelisation ###

# Installons les packages
install.packages(c("rugarch", "rmgarch"))

library(rugarch)
library(rmgarch)

### GARCH univarie ###


# On commence par instancier l'objet ugarch sans y ajouter de parametres
univ_garch = ugarchspec() 

univ_garch


mod <- ugarchfit(spec = univ_garch, data = IBM[, "IBM.Open"])


# affichons les noms des informations contenues dans ces deux objets
print("Objets contenus dans model@fit")
names(mod@fit)
print("Objets contenus dans model@model")
names(mod@model)

# Regardons les coefficients d'estimation
mod@fit$matcoef

variance <- xts(mod@fit$var, order.by = as.Date(index(IBM)))
names(variance) <- "variance"
head(variance)

plot(variance, main = "variance conditionnelle estimée", col = "lightblue")


prev <- cbind(c(IBM[, "IBM.Open"]), mod@fit$fitted.values)
names(prev) <- c("valeur.observee", "valeur.predite")
head(prev)

plot.xts(prev, col = c("lightblue", "red"), legend.loc= "left", lwd = 1)


# parametres graphiques, res pour resolution
options(repr.plot.res = 300, repr.plot.height = 3) 
resid <- xts(mod@fit$residuals, order.by = as.Date(index(IBM)))
plot.xts(resid, main = "Résidus du modèle", col = "lightblue")
resid2 <- xts(mod@fit$residuals^2, order.by = as.Date(index(IBM)))
plot.xts(resid2, main = "Carré des résidus du modèle", col = "lightblue")


rIBM <- dailyReturn(IBM)
head(rIBM)

# parametres graphiques, res pour resolution
options(repr.plot.res = 300, repr.plot.height = 3) 
plot(rIBM, main = "Rendement journalier de l'action IBM", col = "lightblue")

return_garch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(3,3)), 
                           mean.model = list(archm = 3))
fit_return <- ugarchfit(spec = return_garch, data = rIBM)

return_var <- xts(fit_return@fit$var, order.by = as.Date(index(rIBM)))
plot(return_var, main = "Variance conditionnelle du rendement", col = "lightblue")

ugarchforecast(fitORspec = mod, n.ahead = 10)

mean(IBM[, "IBM.Open"])


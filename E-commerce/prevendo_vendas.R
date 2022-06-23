library(tidyverse)
library(readxl)
library(zoo)
library(xts)
library(forecast)

transactions_retail <- read_excel("transactions_retail.xlsx", 
                                  col_types = c("numeric", "text", "text", 
                                                "numeric", "date", "numeric", "numeric", 
                                                "text", "numeric", "text", "text"))

transactions_retail_adj <- transactions_retail %>% select(InvoiceDate, Value, Ano, `Mês`) %>%
                       mutate(Dia = "01", Data = paste(Dia,"/",`Mês`,"/",Ano)) %>%
                       mutate(Data = str_replace_all(Data," ",""))


# Transformando em uma serie temporal padronizada
time_series <- transactions_retail_adj %>% group_by(Data) %>% summarise(Value = sum(Value)) %>%
               mutate(Data = as.Date(Data, "%d / %m / %Y"))

time_series <- xts(time_series[,-1], order.by = time_series$Data)


# Realizando a previsão/forecast
autoplot(time_series)
ggAcf(time_series)

Box.test(time_series, lag = 11, fitdf = 0, type = "Lj") #dados sao ruido branco

BoxCox.lambda(time_series)
fit <- auto.arima(time_series, lambda = 0.58)
fit

fit %>% forecast(h = 3) %>% autoplot()


## Prevendo dos produtos dos 4 eixos ####
# codigos dos itens: 22423 - estrela, 79321 - cachorro, 84879 - duvidoso

codigo_pr <- "84879"

transactions_retail_pr <- transactions_retail %>% select(InvoiceDate,StockCode, Value, Ano, `Mês`) %>%
  mutate(Dia = "01", Data = paste(Dia,"/",`Mês`,"/",Ano)) %>%
  mutate(Data = str_replace_all(Data," ",""))

time_series_pr <- transactions_retail_pr %>% filter(StockCode == codigo_pr) %>% group_by(Data) %>% summarise(Value = sum(Value)) %>%
  mutate(Data = as.Date(Data, "%d / %m / %Y"))
time_series_pr <- xts(time_series_pr[,-1], order.by = time_series_pr$Data)

autoplot(time_series_pr)
ggAcf(time_series_pr)
Box.test(time_series_pr, lag = 11, fitdf = 0, type = "Lj") #dados sao ruido branco
BoxCox.lambda(time_series_pr)
fit_pr <- auto.arima(time_series_pr, lambda = -0.5)
fit_pr
fit_pr %>% forecast(h = 3) %>% autoplot()

# amostragem baixa, o mais útil é prever na média de todo o período
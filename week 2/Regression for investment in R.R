#-------------------------------
# Regression for investment in R
#-------------------------------

library(alphavantager)

# API key
av_api_key('08CB58LIH7HN6SKH')
print(av_api_key())
args(av_get)
av_get(symbol = 'MSFT', av_fun = 'TIME_SERIES_INTRADAY', interval = '15min', outputsze='compact')

library(tidyverse)
library(lubridate)

temp <- tempfile()
FF_data <- 'https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip'
download.file(FF_data, temp, quiet = TRUE)
FF_factors <- read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3_CSV"), skip = 3)
# or 

FF_factors <- read_csv('C:/Users/julia/OneDrive/Desktop/Coursera/Regression in R for investment/F-F_Research_Data_5_Factors_2x3.CSV')


head(FF_factors)

FF_factors <- FF_factors[1:690,] %>%
  rename(date = X1, Mkt_RF = 'Mkt-RF') %>%
  mutate(date = ymd(paste(substr(date, 1, 4), '-', substr(date, 5,6), '-01'))) %>%
  mutate(date = rollback(date+month(1))) %>%
  mutate_at(vars(-date), as.numeric)

start_date <- "1979-12-01"
end_date <- "2002-12-31"

FF_factors <- FF_factors %>%
  filter(date >= start_date, date <= end_date)

summary(FF_factors)

# plot the characteristics of the factors
# How much a factor affects the asset return over time
FF_factors %>%
  mutate(date = year(date)) %>%
  filter(date > 1979) %>%
  gather(key = key, value = value, -date) %>%
  group_by(date, key) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line()

# factors cummulative returns
FF_factors_Return <- FF_factors %>% filter(year(date) > 1979)
per.to.dec <- function(x){x/100}
FF_factors_CumReturn <- FF_factors_Return %>% 
  mutate_at(vars(-date), per.to.dec) %>%
  mutate(cum_Mkt_RF = cumprod(1+Mkt_RF)-1) %>%
  mutate(cum_SMB = cumprod(1+SMB)-1) %>%
  mutate(cum_HML = cumprod(1+HML)-1) %>%
  mutate(cum_RMW = cumprod(1+RMW)-1) %>%
  mutate(cum_CMA = cumprod(1+CMA)-1) 
  
FF_factors_CumReturn %>%
  select(date, cum_SMB, cum_HML) %>%
  gather(key = key, value = value, -date) %>%
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line()

# regressions
library(quantmod)

data = FF_factors

FAANG <- c('FB', 'AMZN', 'AAPL', 'NFLX', 'GOOGL')
for(i in 1:length(FAANG)) {
  getSymbols(FAANG[i], from = "1979-12-01", to= "2019-12-31", periodicity = "monthly")
  data <- as.data.frame(monthlyReturn(get(FAANG[i])[,6]) = 100)
  data$ticker <- rep(FAANG[i], dim(data)[1])
  data$date <- rownames(data)
  rownames(data) <- NULL
  
  if(i == 1){stock_returns <- data[-1,]} else {stock_returns <- rbind(stock_returns, data[-1,])}
}

#


#-------
# Week 2
#-------

library(lubridate)
library(quantmod)
library(lubridate)

macro <- c('GDPC1', 'CPIAUCSL', 'DTB3', 'DGS10', 'DAAA', 'DBAA', 'UNRATE', 'INDPRO', 'DCOILWTICO')

rm(macro_factors)
for (i in 1:length(macro)) {
  getSymbols(macro[i], src = 'FRED')
  data <- as.data.frame(get(macro[i]))
  data$date <- as.POSIXct(rownames(data))
  rownames(data) <- NULL
  colnames(data)[1] <- 'macro_value'
  
  data$quarter <- as.yearqtr(data$date)
  data$macro_ticker <- rep(macro[i], dim(data)[1])
  
  data <- data %>%
    mutate(data = ymd(date)) %>%
    group_by((quarter)) %>%
    top_n(1, date) %>%
    filter(date >= '1980-01-01', date <= '2019-12-31') # %>%
    #select(-date)
  
  if(i == 1){macro_factors <- data} else {macro_factors <- rbind(macro_factors, data)}
}

getSymbols('^GSPC', from = '1979-12-01', to = '2019-12-31', periodicity = 'monthly')
SP500_returns <- as.data.frame(quarterlyReturn(GSPC[,6]))

SP500_returns$date <- rownames(SP500_returns)
SP500_returns$ticker <- rep('GSPC', dim(SP500_returns)[1])
SP500_returns <- SP500_returns %>%
  mutate(date = ymd(date)) %>%
  mutate(quarter = as.yearqtr(date)) #%>%
  # select(-date)

data_MacroModel <- left_join(macro_factors, SP500_returns, by = 'quarter') %>%
  na.omit() %>%
  spread(key = macro_ticker, value = macro_value)

MacroModel <- lm(quarterly.returns ~ GDPC1 + CPIAUCSL +DTB3 +DGS10 + DAAA +DBAA+UNRATE+INDPRO+DCOILWTICO,
                 data = data_MacroModel, na.action = 'na.exclude') %>%
  broom::tidy()
  

# Fama-MacBeth

Nasdaq100_FF5Model <- read_csv(file = 'C:/Users/julia/OneDrive/Desktop/Coursera/Regression in R for investment/F-F_Research_Data_5_Factors_2x3.CSV')
Nasdaq100_FF5Model <- Nasdaq100_FF5Model[, colSums(is.na(Nasdaq100_FF5Model)<120)]
Nasdaq100_FF5Model <- Nasdaq100_FF5Model[-(1:240),]

Nasdaq100 <- colnames(Nasdaq100_FF5Model)[-(1:7)]

#----
# end
#----
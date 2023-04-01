#-------------------------------
# Regression for investment in R
#-------------------------------

library(alphavantager)

# API key
av_api_key('08CB58LIH7HN6SKH')
print(av_api_key())
args(av_get)
av_get(symbol = 'MSFT', av_fun = 'TIME_SERIES_INTRADAY', interval = '15min', outputsze='compact')

#----
# end
#----
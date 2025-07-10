#=========================================
#             Thodoris Bonias
#             analysis.R
#=========================================

#necessary libraries
library(tidyverse)
library(tidyquant)
library(quantmod)
library(corrplot)
library(lubridate)

rm(indices)

#vector with tickers and give name to tickers
indices <- c("^GSPC", "^DJI", "^IXIC")

#tq_get to give the values to the vector from 2010 to now
stock_data <- tq_get(indices,
                     from = "2010-01-01",
                     to = Sys.Date(),
                     get = "stock.prices")%>%
  select(symbol, date, adjusted)


#turns daily values to monthly closing values
monthly_data<-stock_data%>%
  mutate(month = floor_date(date,"month"))%>%
  group_by(symbol,month)%>%
  summarise(adjusted = last(adjusted), .groups = "drop")

#The table appears
wide_monthly <- monthly_data%>%
  pivot_wider(names_from = symbol, values_from = adjusted)

#make them appear with full name instead of ticker
wide_monthly <- wide_monthly %>%
  rename(
    `S&P500` = `^GSPC`,
    DowJones = `^DJI`,
    Nasdaq   = `^IXIC`
  )

view(wide_monthly)

#Perfomance plot the last 15 years of the 3 indices
ggplot(wide_monthly, aes(x = month)) +
  geom_line(aes(y = DowJones), color = "blue", linewidth = 1) +
  geom_line(aes(y = `S&P500`), color = "green", linewidth = 1) +
  geom_line(aes(y = Nasdaq), color = "purple", linewidth = 1) +
  labs(title = "Monthly Prices of Major U.S. Indices",
       x = "Date", y = "Adjusted Close") +
  theme_minimal()

#Log returns to compare the perfomance
returns <- wide_monthly %>%
  mutate(
    return_DowJones = log(DowJones/lag(DowJones)),
    return_SnP = log(`S&P500`/lag(`S&P500`)),
    return_Nasdaq = log(Nasdaq/lag(Nasdaq))
  )

#Plot returns
ggplot(returns,aes(x=month)) +
  geom_line(aes(y = return_DowJones), color = "blue", linewidth = 0.8) +
  geom_line(aes(y = return_SnP), color = "green", linewidth = 0.8) +
  geom_line(aes(y = return_Nasdaq), color = "purple", linewidth = 0.8) +
  labs(title = "Monthly Log Returns of Major U.S. Indices",
       x = "Date", y = "Log Return") +
  theme_minimal()




                            



library(tidyverse)
library(httr)
library(XML)

rates_response <- xmlToDataFrame("https://api.businessonline.ge/api/rates/nbg/usd/2010-01-01/2021-09-01")

setwd("D:\\Dropbox\\pollster.ge\\Geo Local 2021\\polls\\covariates")

rates <- xmlToDataFrame("2021-10-01.xml")%>%
  mutate(Date = str_replace_all(Date, "T00:00:00", ""),
         Date = as.Date(Date))

write.csv(rates, "rates.csv", row.names = F)


inflation <- readxl::read_excel("samomxmareblo-fasebis-indeqsebi-(wina-Tve=100).xlsx", sheet="montly_rate")

write.csv(inflation, "inflation", row.names=F)

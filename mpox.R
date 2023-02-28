library(shiny)
library(shinythemes)
library(plotly)
library(zoo)
library(tidyverse)
library(lubridate)

mpox <- read_csv('https://opendata.ecdc.europa.eu/monkeypox/casedistribution/csv/data.csv', show_col_types=F)

mpox <- mpox %>%
    rename(date=DateRep, country=CountryExp, cases=ConfCases) %>%
    select(date, country, cases) %>%
    mutate(country=ifelse(country=='Czechia', 'Czech Republic', country)) %>%
    arrange(country, date) %>%
    group_by(country) %>%
    mutate(cum_cases=cumsum(cases),
           smooth_cases=zoo::rollmean(cases, 7, fill='NA', align='center')) %>%
    ungroup()

mpox_all <- mpox %>%
    group_by(date) %>%
    summarise(country='All', cases=sum(cases)) %>%
    mutate(cum_cases=cumsum(cases),
           smooth_cases=zoo::rollmean(cases, 7, fill='NA', align='center')) %>%
    ungroup()

mpox <- bind_rows(mpox_all, mpox)

write_csv(mpox, 'data/mpox.csv')

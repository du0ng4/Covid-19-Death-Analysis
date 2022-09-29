# Author: Andy Duong
# Email: aduong.cs@gmail.com
# Covid-19 Death Rate Analysis

# #
# data wrangling
# #

library(tidyverse)

# cases and deaths data retrieved from the 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Center for Systems Science and Engineering
# https://github.com/CSSEGISandData/COVID-19

# total number of cases by country
cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
cases <- cases %>% pivot_longer(cols=5:ncol(cases), names_to="date", values_to="cases")

# total number of deaths by country
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths <- deaths %>% pivot_longer(cols=5:ncol(deaths), names_to="date", values_to="deaths")

# merge into single data frame
covid <- cases %>% full_join(deaths)
covid <- covid %>% group_by(`Country/Region`, date) %>% summarise('cases'=sum(cases), 'deaths'=sum(deaths))
covid <- covid %>% rename('Country'=`Country/Region`)

# hospital bed density by country
# provided by World Health Organization (WHO)
# retrieved from https://www.who.int/data/gho/data/themes/topics/indicator-groups/indicator-group-details/GHO/hospital-bed-density
beds <- read_csv("WHS6_102.csv")
beds <- beds %>% group_by(Country) %>% filter(Year==max(Year))
beds <- beds %>% rename(beds=`Hospital beds (per 10 000 population)`)
beds <- beds %>% mutate(Country=replace(Country, Country=="Iran (Islamic Republic of)", "Iran"))
beds <- beds %>% mutate(Country = replace(Country, Country=="Republic of Korea", "South Korea"))
beds <- beds %>% mutate(Country = replace(Country, Country=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
covid <- covid %>% mutate(Country=replace(Country, Country=="Korea, South", "South Korea"))
covid <- covid %>% mutate(Country = replace(Country, Country=="US", "United States of America"))
covid <- covid %>% inner_join(beds)

# demographics by country
demographics <- read_csv("demographics.csv")
demographics$`Series Code` <- as.character(demographics$`Series Code`)
demographics$`Series Code` <- gsub("^.{0,3}", "", demographics$`Series Code`)
demographics <- subset(demographics, select=-c(`Country Code`, `Series Name`))
demographics <- demographics %>% pivot_wider(names_from='Series Code', values_from='YR2015')
demographics <- na.omit(demographics)
demographics <- demographics %>% rename("Country"="Country Name")
demographics <- demographics %>% rename("life.expectancy"="DYN.LE00.IN")
demographics$POP.80UP <- demographics$POP.80UP.FE + demographics$POP.80UP.MA
demographics$POP.65UP <- demographics$POP.65UP.FE.IN + demographics$POP.65UP.MA.IN
demographics$POP.1564 <- demographics$POP.1564.MA.IN + demographics$POP.1564.FE.IN
demographics$POP.0014 <- demographics$POP.0014.MA.IN + demographics$POP.0014.FE.IN
demographics$mortality <- demographics$DYN.AMRT.FE + demographics$DYN.AMRT.MA
demographics <- subset(demographics, select=-c(3, 5:16))
demographics <- demographics %>% mutate(Country=replace(Country,Country=="Iran, Islamic Rep.", "Iran"))
demographics <- demographics %>% mutate(Country=replace(Country,Country=="Korea, Rep.", "South Korea"))
demographics <- demographics %>% mutate(Country=replace(Country,Country=="United States", "United States of America"))

# merge to single data frame
covid <- covid %>% inner_join(demographics)


# #
# Linear Modeling
# #

# relationship between deaths and total cases
# Multiple R-squared: 0.808
m <- lm(formula=deaths~cases, data=covid)
summary(m)

# relationship between deaths and total cases + life expectancy
# Multiple R-Squared: 0.808
m <- lm(formula=deaths~cases+life.expectancy, data=covid)
summary(m)

# relationship between deaths and total cases + existing mortality
# Multiple R-Squared: 0.808
m <- lm(formula=deaths~cases+mortality, data=covid)
summary(m)

# relationship between deaths and total cases + bed density
# Multiple R-squared:  0.8115
m <- lm(formula=deaths~cases+beds, data=covid)
summary(m)

# relationship between deaths and total cases + population above 65
# Multiple R-squared:  0.809
m <- lm(formula=deaths~cases+POP.65UP, data=covid)
summary(m)

# Normalized population
# Multiple R-squared:  0.8085
covid$PROP.65UP <- covid$POP.65UP/covid$POP.TOTL
m <- lm(formula=deaths~cases+PROP.65UP, data=covid)
summary(m)

# relationship between deaths and total cases + population ages above 65 + bed density
# Multiple R-squared:  0.8131
m <- lm(formula=deaths~cases+POP.65UP+beds, data=covid)
summary(m)

# relationship between deaths and total cases + proportion ages above 65 + bed density
# Multiple R-squared:  0.812
m <- lm(formula=deaths~cases+PROP.65UP+beds, data=covid)
summary(m)

# relationship between deaths and total cases + population ages 15 to 64
# Multiple R-squared:  0.8093
m <- lm(formula=deaths~cases+POP.1564, data=covid)
summary(m)

# normalized population
# Multiple R-squared:  0.8084
covid$PROP.1564 <- covid$POP.1564/covid$POP.TOTL
m <- lm(formula=deaths~cases+PROP.1564, data=covid)
summary(m)

# relationship between deaths and total cases + population ages 15 to 64 + bed density
# Multiple R-squared:  0.8127
m <- lm(formula=deaths~cases+POP.1564+beds, data=covid)
summary(m)

# relationship between deaths and total cases + proportion ages 15 to 64 + bed density
# Multiple R-squared:  0.8139
m <- lm(formula=deaths~cases+PROP.1564+beds, data=covid)
summary(m)
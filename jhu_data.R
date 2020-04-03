# COVID-19 stats tracker, data cleaning script
# Mikaela Springsteen, contactmspringsteen@gmail.com

# COVID-19 data from Johns Hopkins University:
# https://github.com/CSSEGISandData/COVID-19

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load data
total <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
worldstats <- read.csv("Documents/GitHub/covid19-countries/worldstats.csv")

# 0 observations to NA
covid_cases <- list(total, recovered, deaths)
covid_cases <- lapply(covid_cases, function(df) {
  df[ , c(5:ncol(df))][df[ , c(5:ncol(df))] == 0] <- NA
  df
})

# reshape
covid_cases[[1]] <- gather(covid_cases[[1]], Date, Total, -c(1:4))
covid_cases[[2]] <- gather(covid_cases[[2]], Date, Recovered, -c(1:4))
covid_cases[[3]] <- gather(covid_cases[[3]], Date, Deaths, -c(1:4))

# rename variables
covid_cases <- lapply(covid_cases, function(df) {
  names(df)[1] <- "Province"
  names(df)[2] <- "Country"
  df
})

# format variables
covid_cases <- lapply(covid_cases, function(df) {
  df$Country[df$Country == "Bahamas, The"] <- "The Bahamas"
  df$Country[df$Country == "Bahamas"] <- "The Bahamas"
  df$Country[df$Country == "Burma"] <- "Myanmar"
  df$Country[df$Country == "Cabo Verde"] <- "Cape Verde"
  df$Country[df$Country == "Congo (Brazzaville)"] <- "Republic of the Congo"
  df$Country[df$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
  df$Country[df$Country == "East Timor"] <- "Timor-Leste"
  df$Country[df$Country == "Gambia, The"] <- "The Gambia"
  df$Country[df$Country == "Gambia"] <- "The Gambia"
  df$Country[df$Country == "Korea, South"] <- "South Korea"
  df$Country[df$Country == "Taiwan*"] <- "Taiwan"
  df$Country[df$Country == "Saint Lucia"] <- "St. Lucia"
  df$Country[df$Country == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
  df$Country[df$Country == "US"] <- "United States"
  df$Country[df$Country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
  df$Province[df$Province == ""] <- NA
  df$Country[df$Province == "Macau"] <- "Macau"
  df$Province[df$Province == "Macau"] <- NA
  df$Country[df$Province == "Hong Kong"] <- "Hong Kong"
  df$Province[df$Province == "Hong Kong"] <- NA
  df$Country[df$Country == "West Bank and Gaza"] <- "Palestine"
  df$Date <- as.Date(df$Date, "%m/%d/%y")
  df
})

# summarize by country
covid_cases[[1]] <- aggregate(data = covid_cases[[1]], Total ~ Country + Date, sum, drop = FALSE)
covid_cases[[2]] <- aggregate(data = covid_cases[[2]], Recovered ~ Country + Date, sum, drop = FALSE)
covid_cases[[3]] <- aggregate(data = covid_cases[[3]], Deaths ~ Country + Date, sum, drop = FALSE)

# keep complete cases
covid_cases <- lapply(covid_cases, function(df) {
  df <- df[complete.cases(df), ]
  df
})

# merge covid_cases
covid_cases <- covid_cases %>% reduce(left_join, by = c("Country", "Date"))

# add DayCount variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(DayCount = row_number())

# add Day variable (day 1 = the first day a country has at least 100 cases)
Day_dat <- covid_cases %>% group_by(Country) %>% filter(Total >= 100) %>% mutate(Day = row_number())
covid_cases <- merge(covid_cases, Day_dat, all = TRUE)

# add NewCases variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(NewCases = Total - lag(Total, default = first(Total)))

# add NewDeaths variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(NewDeaths = Deaths - lag(Deaths, default = first(Deaths)))

# reorder variables
covid_cases <- covid_cases[ , c(1, 2, 6, 7, 3, 8, 4, 5, 9)]

# filter countries
worldstats <- filter(worldstats, Country %in% levels(as.factor(covid_cases$Country)))

# merge covid_cases and worldstats
covid_cases <- merge(covid_cases, worldstats, by = c("Country"), all = TRUE)

# add Totalper100_000 variable
covid_cases$Totalper100_000 <- (covid_cases$Total/covid_cases$Population)*100000

# add TotalRate variable
covid_cases$TotalRate <- covid_cases$Total/covid_cases$Population

# add RecoveredRate variable
covid_cases$RecoveredRate <- covid_cases$Recovered/covid_cases$Total

# add DeathRate variable
covid_cases$DeathRate <- covid_cases$Deaths/covid_cases$Total

# add Death_per100thou variable
covid_cases$Deaths_per100thou <- (covid_cases$Deaths/covid_cases$Population)*100000

# add Population_mil variable
covid_cases$Population_mil <- (covid_cases$Population)/1000000

# remove cruise ships
covid_cases <- filter(covid_cases, Country != "Diamond Princess")
covid_cases <- filter(covid_cases, Country != "MS Zaandam")

# restructuring for app
covid_cases <- covid_cases %>% drop_na(Day)
covid_cases <- select(covid_cases, Country, Day, Totalper100_000, DeathRate, Population_mil, Over65_perc, Slums_perc, GDP_pcap_ppp, Salaried_perc, Poverty_perc, StatsCapacity, SciArticles, LifeExp, HospBed_per10thou, MD_per10thou, HygBasic_natperc)

# write csv
write.csv(covid_cases, "Documents/GitHub/covid19-countries/covid_cases.csv", row.names = FALSE)
rm(list=ls())

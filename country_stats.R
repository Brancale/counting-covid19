# COVID-19 stats tracker, country stats compilation
# Mikaela Springsteen, contactmspringsteen@gmail.com

# World Bank data:
# https://data.worldbank.org/

# World Health Organization data (accessed 20 March 2020):
# Physicians per 10,000 pop:
## https://apps.who.int/gho/data/node.main.HWFGRP_0020?lang=en
# Hospital beds per 10,000 pop:
## https://apps.who.int/gho/data/view.main.HS07v

# WHO/UNICEF Water Supply, Sanitation and Hygiene data (accessed 20 March 2020):
# https://washdata.org/data/downloads#WLD

# UN Millennium Development Goals data (accessed 21 March 2020):
# http://mdgs.un.org/unsd/mdg/Data.aspx

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")

# load data
pop <-  WDI(country = "all", indicator = c("Population" = "SP.POP.TOTL"), start = 2010, end = 2019)
perc65 <-  WDI(country = "all", indicator = c("Over65_perc" = "SP.POP.65UP.TO.ZS"), start = 2010, end = 2019)
perc80f <-  WDI(country = "all", indicator = c("Over80F_percf" = "SP.POP.80UP.FE.5Y"), start = 2010, end = 2019)
perc80m <-  WDI(country = "all", indicator = c("Over80M_percm" = "SP.POP.80UP.MA.5Y"), start = 2010, end = 2019)
gdp <- WDI(country = "all", indicator = c("GDP_pcap_ppp" = "NY.GDP.PCAP.PP.CD"), start = 2010, end = 2019)
imports <- WDI(country = "all", indicator = c("Imports_percgdp" = "NE.IMP.GNFS.ZS"), start = 2014, end = 2019)
parttime <- WDI(country = "all", indicator = c("PartTime_perc" = "SL.TLF.PART.ZS"), start = 2014, end = 2019)
salaried <- WDI(country = "all", indicator = c("Salaried_perc" = "SL.EMP.WORK.ZS"), start = 2014, end = 2019)
stats <- WDI(country = "all", indicator = c("StatsCapacity" = "IQ.SCI.OVRL"), start = 2019, end = 2019)
articles <-  WDI(country = "all", indicator = c("SciArticles" = "IP.JRN.ARTC.SC"), start = 2018, end = 2019)
health <- WDI(country = "all", indicator = c("HealthSpend_pcap_ppp" = "SH.XPD.CHEX.PP.CD"), start = 2016, end = 2019)
lifeexp <-  WDI(country = "all", indicator = c("LifeExp" = "SP.DYN.LE00.IN"), start = 2010, end = 2019)
hospitalbeds <- read.csv("data/WHOhospitalbeds.csv", stringsAsFactors = FALSE)
physicians <- read.csv("data/WHOphysicians.csv", stringsAsFactors = FALSE)
hygienehome <- read.csv("data/WHOUNICEFwashhome.csv", na.strings = "-", stringsAsFactors = FALSE)
slums <- read.csv("data/UNMDGslums.csv", stringsAsFactors = FALSE)
poverty <- read.csv("data/UNMDGpoverty.csv", stringsAsFactors = FALSE)

# filter data to complete cases
worldbank <- list(pop, perc65, perc80f, perc80m, lifeexp, gdp, imports, parttime, salaried, stats, articles, health)
worldbank <- lapply(worldbank, function(x) {
  x <- x %>% dplyr::filter(complete.cases(.));
  x
})
slums <- gather(slums, Year, Slums_perc, -Country)
slums$Year <- gsub("X","" , slums$Year)
slums <- slums %>% dplyr::filter(complete.cases(.))
poverty <- gather(poverty, Year, Poverty_perc, -Country)
poverty$Year <- gsub("X","" , poverty$Year)
poverty <- poverty %>% dplyr::filter(complete.cases(.))

# filter to most recent obs for each country
worldbank <- lapply(worldbank, function(x) {
  x <- x %>% group_by(country) %>% slice(which.max(year));
  x
})
hospitalbeds <- hospitalbeds %>% group_by(Country) %>% slice(which.max(Year))
physicians <- physicians %>% group_by(Country) %>% slice(which.max(Year))
hygienehome <- hygienehome %>% group_by(Country) %>% slice(which.max(Year))
slums <- slums %>% group_by(Country) %>% slice(which.max(Year))
poverty <- poverty %>% group_by(Country) %>% slice(which.max(Year))

# select variables
worldbank <- lapply(worldbank, function(x) {
  x <- select(x, -iso2c, -year);
  x
})
hospitalbeds <- select(hospitalbeds, -Year)
physicians <- select(physicians, Country, Medical.doctors..per.10.000.population.)
hygienehome <- select(hygienehome, -Year, -Sl)
slums <- select(slums, -Year)
poverty <- select(poverty, -Year)

# merge each of world bank data, WHO data, MDG data
worldbank <- worldbank %>% reduce(left_join, by = "country")
who <- Reduce(function(x, y) merge(x, y, by = c("Country"), all = TRUE), list(hospitalbeds, physicians, hygienehome))
mdg <- merge(slums, poverty, by = "Country", all = TRUE)

# rename variables
names(worldbank)[1] <- c("Country")
names(who)[2] <- c("HospBed_per10thou")
names(who)[3] <- c("MD_per10thou")

# validate country names
#setdiff(levels(as.factor(covid_cases$Country)), levels(as.factor(worldbank$Country)))

# format world bank country names
worldbank$Country[worldbank$Country == "Bahamas, The"] <- "The Bahamas"
worldbank$Country[worldbank$Country == "Brunei Darussalam"] <- "Brunei"
worldbank$Country[worldbank$Country == "Cabo Verde"] <- "Cape Verde"
worldbank$Country[worldbank$Country == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
worldbank$Country[worldbank$Country == "Congo, Rep."] <- "Republic of the Congo"
worldbank$Country[worldbank$Country == "Czech Republic"] <- "Czechia"
worldbank$Country[worldbank$Country == "Egypt, Arab Rep."] <- "Egypt"
worldbank$Country[worldbank$Country == "Gambia, The"] <- "The Gambia"
worldbank$Country[worldbank$Country == "Hong Kong SAR, China"] <- "Hong Kong"
worldbank$Country[worldbank$Country == "Iran, Islamic Rep."] <- "Iran"
worldbank$Country[worldbank$Country == "Korea, Dem. People’s Rep."] <- "North Korea"
worldbank$Country[worldbank$Country == "Korea, Rep."] <- "South Korea"
worldbank$Country[worldbank$Country == "Kyrgyz Republic"] <- "Kyrgyzstan"
worldbank$Country[worldbank$Country == "Lao PDR"] <- "Laos"
worldbank$Country[worldbank$Country == "Macao SAR, China"] <- "Macau"
worldbank$Country[worldbank$Country == "Macedonia, FYR"] <- "North Macedonia"
worldbank$Country[worldbank$Country == "Russian Federation"] <- "Russia"
worldbank$Country[worldbank$Country == "Slovak Republic"] <- "Slovakia"
worldbank$Country[worldbank$Country == "Syrian Arab Republic"] <- "Syria"
worldbank$Country[worldbank$Country == "Venezuela, RB"] <- "Venezuela"

# validate country names
#setdiff(levels(as.factor(covid_cases$Country)), levels(as.factor(who$Country)))

# format who country names
who$Country[who$Country == "Bahamas"] <- "The Bahamas"
who$Country[who$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
who$Country[who$Country == "Brunei Darussalam"] <- "Brunei"
who$Country[who$Country == "Cabo Verde"] <- "Cape Verde"
who$Country[who$Country == "Congo"] <- "Democratic Republic of the Congo"
who$Country[who$Country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
who$Country[who$Country == "Democratic People's Republic of Korea"] <- "North Korea"
who$Country[who$Country == "Gambia"] <- "The Gambia"
who$Country[who$Country == "Iran (Islamic Republic of)"] <- "Iran"
who$Country[who$Country == "Lao People's Democratic Republic"] <- "Laos"
who$Country[who$Country == "Republic of Korea"] <- "South Korea"
who$Country[who$Country == "Republic of Moldova"] <- "Moldova"
who$Country[who$Country == "Republic of North Macedonia"] <- "North Macedonia"
who$Country[who$Country == "Russian Federation"] <- "Russia"
who$Country[who$Country == "Saint Lucia"] <- "St. Lucia"
who$Country[who$Country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
who$Country[who$Country == "Syrian Arab Republic"] <- "Syria"
who$Country[who$Country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
who$Country[who$Country == "United Republic of Tanzania"] <- "Tanzania"
who$Country[who$Country == "United States of America"] <- "United States"
who$Country[who$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
who$Country[who$Country == "Viet Nam"] <- "Vietnam"

# validate country names
#setdiff(levels(as.factor(covid_cases$Country)), levels(as.factor(mdg$Country)))

# format mdg country names
mdg$Country[mdg$Country == "Cabo Verde"] <- "Cape Verde"
mdg$Country[mdg$Country == "Congo"] <- "Democratic Republic of the Congo"
mdg$Country[mdg$Country == "Czech Republic"] <- "Czechia"
mdg$Country[mdg$Country == "Gambia"] <- "The Gambia"
mdg$Country[mdg$Country == "Lao People's Democratic Republic"] <- "Laos"
mdg$Country[mdg$Country == "Republic of Moldova"] <- "Moldova"
mdg$Country[mdg$Country == "Russian Federation"] <- "Russia"
mdg$Country[mdg$Country == "Saint Lucia"] <- "St. Lucia"
mdg$Country[mdg$Country == "State of Palestine"] <- "Palestine"
mdg$Country[mdg$Country == "Syrian Arab Republic"] <- "Syria"
mdg$Country[mdg$Country == "Swaziland"] <- "Eswatini"
mdg$Country[mdg$Country == "United Republic of Tanzania"] <- "Tanzania"
mdg$Country[mdg$Country == "The former Yugoslav Republic of Macedonia"] <- "North Macedonia"
mdg$Country[mdg$Country == "Viet Nam"] <- "Vietnam"

# merge world bank, WHO, and mdg data
worldstats <- Reduce(function(x, y) merge(x, y, by = c("Country"), all = TRUE), list(worldbank, who, mdg))

# clean up hygiene character + number combinations
worldstats[ , 16:24][worldstats[ , 16:24] == "<1"] <- "0"
worldstats[ , 16:24][worldstats[ , 16:24] == ">99"] <- "100"

# reorder variables
worldstats <- worldstats[ , c(1, 2, 3, 4, 5, 25, 7, 8, 9, 10, 26, 11, 12, 13, 6, 14, 15, 16:24)]

# write csv
write.csv(worldstats, "data/worldstats.csv", row.names = FALSE)
rm(list=ls())

# COVID-19 stats tracker, data cleaning script
# Mikaela Springsteen, contactmspringsteen@gmail.com

# COVID-19 data from Johns Hopkins University:
# https://github.com/CSSEGISandData/COVID-19

# testing data from Our World in Data:
# https://github.com/owid/covid-19-data/tree/master/public/data

# packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")

# load data
total <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
testing <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv"))
coronanet <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv"))
worldstats <- read.csv("data/worldstats.csv")

# select, rename coronanet variables
coronanet <- select(coronanet, -date_announced, -date_end, -type_text, -source_file_Type, -target_direction, -travel_mechanism, -compliance, -enforcer, -ISO_A3, -ISO_A2, -link_type, -target_country, -target_geog_level, -target_region, -target_province, -target_city, -target_other, -target_who_what, -index_high_est, -index_med_est, -index_low_est, -index_country_rank, -domestic_policy, -type_num_vaccines)
coronanet$type[coronanet$type == "Restriction of Non-Essential Businesses"] <- "Restrict businesses"
coronanet$type[coronanet$type == "Restriction of Non-Essential Government Services"] <- "Restrict gov. services"
coronanet$type[coronanet$type == "Closure of Schools"] <- "School closure"
coronanet$type[coronanet$type == "External Border Restrictions"] <- "Restrict external borders"
coronanet$type[coronanet$type == "Internal Border Restrictions"] <- "Restrict internal borders"
coronanet$type[coronanet$type == "Restrictions of Mass Gatherings"] <- "Restrict mass gatherings"
coronanet$type[coronanet$type == "Social Distancing"] <- "Social distancing"
coronanet$type[coronanet$type == "Quarantine/Lockdown"] <- "Quarantine"
coronanet$type[coronanet$type == "Health Monitoring"] <- "Health monitoring"
coronanet$type[coronanet$type == "Health Resources"] <- "Health resources"
coronanet$type[coronanet$type == "Health Testing"] <- "Testing"
coronanet$type[coronanet$type == "New Task Force or Bureau"] <- "New task force"
coronanet$type[coronanet$type == "Public Awareness Campaigns"] <- "Public awareness"
coronanet$type[coronanet$type == "Other Policy Not Listed Above"] <- "Other"
coronanet$type[coronanet$type == "Anti-Disinformation Measures"] <- "Anti-disinformation"
coronanet$type[coronanet$type == "Declaration of Emergency"] <- "Declaration of emergency"
coronanet$type[coronanet$type == "New Task Force, Bureau or Administrative Configuration"] <- "New task force"
coronanet$type[coronanet$type == "Public Awareness Measures"] <- "Public awareness"
coronanet$type[coronanet$type == "Closure and Regulation of Schools"] <- "Close/regulate schools"
coronanet$type[coronanet$type == "Restriction and Regulation of Businesses"] <- "Restrict/regulate business"
coronanet$type[coronanet$type == "Restriction and Regulation of Government Services"] <- "Restrict/regulate gov. services"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Doctors"] <- " (doctors)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Government Quarantine (i.e. quarantine at a government hotel or facility)"] <- " (by gov.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hand Sanitizer"] <- " (hand sanitizer)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Certificates"] <- " (health certificates)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Research Facilities"] <- " (research facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Screenings (e.g. temperature checks)"] <- " (screenings)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Volunteers"] <- " (health volunteers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Higher education (i.e. degree granting institutions)"] <- " (higher ed.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hospitals"] <- " (hospitals)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Masks"] <- " (masks)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Non-Essential Commercial Businesses"] <- " (commercial businessess)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Nurses"] <- " (nurses)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other"] <- " (other)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Health Infrastructure"] <- " (other infrastructure)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Health Materials"] <- " (other materials)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Heath Staff"] <- " (other staff)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Personal Grooming (e.g. hair salons)"] <- " (personal grooming)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Preschool or childcare facilities (generally for children ages 5 and below)"] <- " (preschool/childcare)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Primary Schools (generally for children ages 10 and below)"] <- " (primary)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Public Testing Facilities (e.g. drive-in testing for COVID-19)"] <- " (testing facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Quarantine only applies to people of certain ages. Please note the age restrictions in the text box."] <- " (certain ages only)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Quarantine outside the home or government facility (i.e. quarantine in a hotel)"] <- " (outside home or gov.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Restaurants/Bars"] <- " (restaurants/bars)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Retail Businesses"] <- " (retail)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Secondary Schools (generally for children ages 10 to 18)"] <- " (secondary ed.)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Self-Quarantine (i.e. quarantine at home)"] <- " (self-quarantine)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Shopping Centers"] <- " (shopping centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Temporary Medical Centers"] <- " (temp. medical centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Temporary Quarantine Centers"] <- " (temp. quarantine centers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Test Kits"] <- " (test kits)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Travel History Form (e.g. documents where traveler has recently been)"] <- " (travel history form)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Infrastructure"] <- " (unspecified infrastructure)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Materials"] <- " (unspecified materials)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Health Staff"] <- " (unspecified staff)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Ventilators"] <- " (ventilators)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "All non-essential government services restricted"] <- " (all non-essential gov. services restricted)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "All or unspecified non-essential businesses"] <- " (all or unspecified non-essential business)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "All public spaces / everywhere"] <- " (all public spaces)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Attendance at religious services prohibited (e.g. mosque/church closings)"] <- " (attendance at religious services prohibited)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Cancellation of an annually recurring event"] <- " (cancellation of annual event)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Disseminating information related to COVID-19 to the public that is reliable and factually accurate"] <- " (disseminating reliable and accurate info)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Health Insurance"] <- " (health insurance)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hygiene measures for commercial areas"] <- " (hygiene measures for commercial areas)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hygiene measures for public areas"] <- " (hygiene measures for public areas)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Hygiene measures for public transport"] <- " (hygiene measures for public transport)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Inside public or commercial building (e.g. supermarkets)"] <- " (in public/commercial buildings)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "New Task Force or Bureau (i.e. establishment of a temporary body)"] <- " (new task force)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Administrative Configurations"] <- " (other admin configurations)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Areas Hygiene Measures Applied"] <- " (hygiene measures for other areas)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Businesses"] <- " (other business)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other External Border Restriction"] <- " (other external border restriction)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Mask Wearing Policy"] <- " (other mask policy)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Quarantine"] <- " (other quarantine)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Personal Grooming Businesses (e.g. hair salons)"] <- " (personal grooming business)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Personal Protective Equipment"] <- " (personal protective equipment)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Visa restrictions (e.g. suspend issuance of visa)"] <- " (visa restrictions)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Vaccines"] <- " (vaccines)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Wearing masks"] <- " (wearing masks)"

coronanet$type_sub_cat[coronanet$type_sub_cat == "All essential government services regulated"] <- " (all essential gov. services regulated)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "All non-essential government services regulated"] <- " (all non-essential gov. services regulated)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "All or unspecified essential businesses"] <- " (all/unspecified essential business)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Beaches"] <- " (beaches)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Both Disseminating and Gathering information related to COVID-19"] <- " (disseminating & gathering Covid-19 info)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Burial procedures"] <- " (burial procedures)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Campsites"] <- " (campsites)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Cancellation of a recreational or commercial event"] <- " (recreational/commercial event cancelled)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Construction"] <- " (construction)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Existing government entity given new powers"] <- " (existing gov. entity given new powers)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Gathering information related to COVID-19 from the public"] <- " (gathering Covid-19 info from the public)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Information service activities"] <- " (info service activities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Issuing of permits/certificates and/or processing of government documents"] <- " (issuing permits/processing gov. documents)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Lockdown applies to all people"] <- " (lockdown applies to everyone)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Medicine/Drugs"] <- " (medicine/drugs)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Mining and quarrying"] <- " (mining/quarrying)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Essential Businesses"] <- " (other essential businesses)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other Non-Essential Businesses"] <- " (other non-essential businesses)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other public facilities"] <- " (other public facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Other public outdoor spaces"] <- " (other public outdoor spaces)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Parks"] <- " (parks)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Pharmacies"] <- " (pharmacies)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Postponement of a recreational or commercial event"] <- " (recreational/commercial event postponed)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Postponement of an annually recurring event"] <- " (annually recurring event postponed)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Prison population reduced (e.g. early release of prisoners)"] <- " (prison population reduced)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Public courts"] <- " (public courts)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Public libraries"] <- " (public libraries)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Public museums/galleries"] <- " (public museums/galleries)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Publishing activities"] <- " (publishing activities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Regulated government working hours (e.g. work from home policies for government workers)"] <- " (gov. working hours regulated)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Regulations on government meetings (including e.g. suspension of parliament)"] <- " (regulations on gov. meetings)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Supermarkets/grocery stores"] <- " (supermarkets/grocery stores)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Telecommunications"] <- " (telecommunications)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Tourist Sites"] <- " (tourist sites)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified Mask Wearing Policy"] <- " (unspecified mask policy)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified outdoor spaces"] <- " (unspecified outdoor spaces)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Unspecified public facilities"] <- " (unspecified public facilities)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Visa extensions (e.g. visa validity extended)"] <- " (visa extensions)"
coronanet$type_sub_cat[coronanet$type_sub_cat == "Warehousing and support activities for transportation"] <- " (warehouse/support for transport)"
coronanet$type_sub_cat[is.na(coronanet$type_sub_cat)] <- ""
coronanet$Measure <- paste0(coronanet$type, coronanet$type_sub_cat)
coronanet <- select(coronanet, date_start, country, init_country_level, link, Measure)
coronanet <- filter(coronanet, init_country_level == "National" | init_country_level == "No, it is at the national level")
coronanet <- select(coronanet, -init_country_level)
coronanet <- unique(coronanet[ , 1:4])
names(coronanet) <- c("Date", "Country", "Link", "Measure")
coronanet <- select(coronanet, -Link)
coronanet <- unique(coronanet[ , 1:3])
coronanet <- coronanet %>%
  group_by(Country, Date) %>%
  mutate(Measure = paste0(Measure, collapse = " <br> "))
coronanet <- unique(coronanet[ , 1:3])

# select, rename testing variables
testing <- select(testing, Entity, Date, "Cumulative total")
names(testing) <- c("Description", "Date", "Tests")
#levels(as.factor(testing$Description))
#View(filter(testing, Description == "India - people tested"))
testing <- filter(testing, Description != "United States - inconsistent units (COVID Tracking Project)")
testing <- filter(testing, Description != "Japan - people tested")
testing <- filter(testing, Description != "Italy - people tested")
testing <- filter(testing, Description != "United Kingdom - tests performed")
testing <- filter(testing, Description != "India - people tested")

# split testing$Entity into Country and Units
newtesting <- colsplit(testing$Description, " - ", names = c("Country", "Units"))
testing <- cbind(testing, newtesting)

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
  df$Country[df$Country == "Czech Republic"] <- "Czechia"
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
testing$Date <- as.Date(testing$Date, "%Y-%m-%d")
testing$Country[testing$Country == "Czech Republic"] <- "Czechia"
# setdiff(levels(as.factor(covid_cases[[1]]$Country)), levels(as.factor(coronanet$Country)))
coronanet$Country[coronanet$Country == "Ivory Coast"] <- "Cote d'Ivoire"
coronanet$Country[coronanet$Country == "Czech Republic"] <- "Czechia"
coronanet$Country[coronanet$Country == "Swaziland"] <- "Eswatini"
coronanet$Country[coronanet$Country == "Macedonia"] <- "North Macedonia"
coronanet$Country[coronanet$Country == "Republic of Congo"] <- "Republic of the Congo"
coronanet$Country[coronanet$Country == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
coronanet$Country[coronanet$Country == "Saint Lucia"] <- "St. Lucia"
coronanet$Country[coronanet$Country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
coronanet$Country[coronanet$Country == "United Republic of Tanzania"] <- "Tanzania"
coronanet$Country[coronanet$Country == "Gambia"] <- "The Gambia"
coronanet$Country[coronanet$Country == "East Timor"] <- "Timor-Leste"
coronanet$Country[coronanet$Country == "United States of America"] <- "United States"

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

# merge covid_cases and testing on Country and Date
covid_cases <- merge(covid_cases, testing, by = c("Country", "Date"), all = TRUE)

# merge covid_cases and coronanet on Country and Date
covid_cases <- merge(covid_cases, coronanet, by = c("Country", "Date"), all = TRUE)

# add DayCount variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(DayCount = row_number())

# add Day variable (day 1 = the first day a country has at least 100 cases)
Day_dat <- covid_cases %>% group_by(Country) %>% filter(Total >= 100) %>% mutate(Day = row_number())
covid_cases <- merge(covid_cases, Day_dat, all = TRUE)

# add NewCases variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(NewCases = Total - lag(Total, default = first(Total)))

# add NewDeaths variable
covid_cases <- covid_cases %>% group_by(Country) %>% mutate(NewDeaths = Deaths - lag(Deaths, default = first(Deaths)))

# filter countries
worldstats <- filter(worldstats, Country %in% levels(as.factor(covid_cases$Country)))

# merge covid_cases and worldstats
covid_cases <- merge(covid_cases, worldstats, by = c("Country"), all = TRUE)

# transform NewCases variable to per 100,000 people scale
covid_cases$NewCases <- (covid_cases$NewCases/covid_cases$Population)*100000

# add Totalper100_000 variable
covid_cases$Totalper100_000 <- (covid_cases$Total/covid_cases$Population)*100000

# add TotalRate variable
covid_cases$TotalRate <- covid_cases$Total/covid_cases$Population

# add RecoveredRate variable
covid_cases$RecoveredRate <- covid_cases$Recovered/covid_cases$Total

# add DeathRate variable
covid_cases$DeathRate <- covid_cases$Deaths/covid_cases$Total

# add Deathsper100_000 variable
covid_cases$Deathsper100_000 <- (covid_cases$Deaths/covid_cases$Population)*100000

# add Testsper100_000 variable
covid_cases$Testsper100_000 <- (covid_cases$Tests/covid_cases$Population)*100000

# add Population_mil variable
covid_cases$Population_mil <- (covid_cases$Population)/1000000

# remove cruise ships
covid_cases <- filter(covid_cases, Country != "Diamond Princess")
covid_cases <- filter(covid_cases, Country != "MS Zaandam")

# limit to obs where Day exists
covid_cases <- covid_cases %>% drop_na(Day)

# top 10
countries <- select(covid_cases, Country, Date, Totalper100_000, Total)
countries <- countries %>% group_by(Country) %>% slice(which.max(Date))
countries[with(countries, order(-Total)), ]

# middle 10
nlevels(as.factor(countries$Country))
middlecountries <- countries[with(countries, order(-Total)), ]
middlecountries[62:71,]

# bottom 10
countries[with(countries, order(Total)), ]

# restructuring for app
covid_cases <- select(covid_cases, Country, Day, Date, Testsper100_000, NewCases, Units, Measure, Totalper100_000, DeathRate, Population_mil, Over65_perc, Slums_perc, GDP_pcap_ppp, Salaried_perc, Poverty_perc, StatsCapacity, SciArticles, LifeExp, HospBed_per10thou, MD_per10thou, HygBasic_natperc, Population)

# write csv
write.csv(covid_cases, "covid_cases.csv", row.names = FALSE)
rm(list=ls())

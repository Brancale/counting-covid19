# COVID-19 stats tracker
# 28 March 2020
# Mikaela Springsteen, contactmspringsteen@gmail.com

# includes code adapted from
# https://github.com/ceefluz/radar

# packages

if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(withr)) install.packages("withr", repos = "http://cran.us.r-project.org")
if(!require(rintrojs)) install.packages("rintrojs", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(htmlwidgets)) install.packages("htmlwidgets", repos = "http://cran.us.r-project.org")

# update data to be used
#source("jhu_data.R")

# import data
covid_cases <- read.csv("covid_cases.csv")
covid_cases$Cases <- covid_cases$Totalper100_000
covid_cases$Tests <- covid_cases$Testsper100_000

# Shiny ui
ui <- dashboardPage(
  # header
  dashboardHeader(title = "Counting Covid-19 International", titleWidth = 300,

                  tags$li(a(tags$i("*compare infection rates for other regions here*"), href = "https://mikaelaspringsteen.github.io/countingcovid19/"), class = "dropdown"),
                  dropdownMenu(type = "notifications",
                    icon = icon("question"),
                    badgeStatus = NULL,
                    headerText = tags$i("Questions? Suggestions? Want to request", tags$br(),
                                        "a stat be added to the app? Get in touch at", tags$br(),
                                        "contactmspringsteen@gmail.com")),
                  tags$li(a("ABOUT THIS APP", href = "https://github.com/mikaelaspringsteen/counting-covid19/blob/master/README.md"), class = "dropdown")),

  # sidebar
  dashboardSidebar(
    useShinyjs(),
    introjsUI(),
    width = 300,
    tags$br(),
    h5("Select a single filter or combine", align = "center"),
    h5("several to visualize their impact on" , align = "center"),
    h5("tracking the spread of the virus.", align = "center"),
    tags$hr(),
    introBox(data.step = 3, data.intro = "Click here to update graphs with your selections or to reset any highlights set from the controls to the right of the graph.",
    fluidRow(
      column(1, offset = 3,
      actionButton("updategraph", tags$b("Update graph"))
      )
    )
    ),
    introBox(data.step = 2, data.intro = "Selecting variables here will highlight any countries on the graph which match those characteristics.",
    sidebarMenu(
      introBox(data.step = 1, data.intro = "Select countries here. You may also isolate the course of the virus in specific countries by double clicking on the country name to the right of the graph.",
      uiOutput("countries")
      ),
      menuItem("Population statistics", tabName = "populationstatistics",
               checkboxInput(
                 inputId = "popcheck",
                 label = "Population (in millions)",
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "popinput",
                 label = NULL,
                 min = 0,
                 max = ceiling(max(covid_cases$Population_mil, na.rm = TRUE)),
                 value = c(0, ceiling(max(covid_cases$Population_mil, na.rm = TRUE))),
                 step = 25
                 ),
               checkboxInput(
                 inputId = "agecheck",
                 label = "% of population aged 65+",
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "ageinput",
                 label = NULL,
                 min = floor(min(covid_cases$Over65_perc, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Over65_perc, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Over65_perc, na.rm = TRUE)), ceiling(max(covid_cases$Over65_perc, na.rm = TRUE))),
                 step = 5,
                 post = "%"
               ),
               checkboxInput(
                 inputId = "slumscheck",
                 label = "% of urban population living in slums",
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "slumsinput",
                 label = NULL,
                 min = floor(min(covid_cases$Slums_perc, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Slums_perc, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Slums_perc, na.rm = TRUE)), ceiling(max(covid_cases$Slums_perc, na.rm = TRUE))),
                 step = 5,
                 post = "%"
               )
      ),
      menuItem("Economic statistics", tabName = "economicstatistics",
               checkboxInput(
                 inputId = "gdpcheck",
                 label = "GDP (at purchasing power parity per capita, latest pre-pandemic numbers)",
                 value = FALSE
                 ),
               sliderInput(
                 inputId = "gdpinput",
                 label = NULL,
                 min = floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)), ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE))),
                 step = 100
               ),
               checkboxInput(
                 inputId = "salariedcheck",
                 label = "% of workers in salaried occupations",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "salariedinput",
                 label = NULL,
                 min = floor(min(covid_cases$Salaried_perc, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Salaried_perc, na.rm = TRUE)), ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE))),
                 step = 5,
                 post = "%"
               ),
               checkboxInput(
                 inputId = "povertycheck",
                 label = "% of population below national poverty line",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "povertyinput",
                 label = NULL,
                 min = floor(min(covid_cases$Poverty_perc, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$Poverty_perc, na.rm = TRUE)), ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE))),
                 step = 5,
                 post = "%"
               )
      ),
      menuItem("Health statistics", tabName = "healthstatistics",
               checkboxInput(
                 inputId = "lifeexpcheck",
                 label = "Life expectancy (in years)",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "lifeexpinput",
                 label = NULL,
                 min = floor(min(covid_cases$LifeExp, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$LifeExp, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$LifeExp, na.rm = TRUE)), ceiling(max(covid_cases$LifeExp, na.rm = TRUE))),
                 step = 5
               ),
               checkboxInput(
                 inputId = "hospcheck",
                 label = "Hospital beds (per 10,000 people)",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "hospinput",
                 label = NULL,
                 min = floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE))),
                 step = 5
               ),
               checkboxInput(
                 inputId = "mdcheck",
                 label = "Medical doctors (per 10,000 people)",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "mdinput",
                 label = NULL,
                 min = floor(min(covid_cases$MD_per10thou, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$MD_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE))),
                 step = 5
               ),
               checkboxInput(
                 inputId = "hygienecheck",
                 label = "% of population with access to basic hygiene facilities",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "hygieneinput",
                 label = NULL,
                 min = floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)), ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE))),
                 step = 5
               )
      ),
      menuItem("Scientific statistics", tabName = "scientificstatistics",
               checkboxInput(
                 inputId = "statscapacitycheck",
                 label = "Statistical capacity score",
                 value = FALSE
               ),
               sliderInput(
                 inputId = "statscapacityinput",
                 label = NULL,
                 min = floor(min(covid_cases$StatsCapacity, na.rm = TRUE)),
                 max = ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE)),
                 value = c(floor(min(covid_cases$StatsCapacity, na.rm = TRUE)), ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE))),
                 step = 5
               )
      )
    )
    )
  ),
  # body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    introBox(data.step = 4, data.intro = "Switch between tabs to see different Covid-19 metrics. A description of the graph is located below each panel.",
    tabsetPanel(
      tabPanel("Tests",
               introBox(data.step = 5, data.intro = "Each graph is interactive. Hover over points/lines for more information, or find more settings (including a home button to reset axes) at the top right of each graph.",
               fluidRow(column(12, uiOutput("tests_graph")))
               ),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all countries. The blue represents the mean for the highlighted countries. The light band represents standard error.", tags$br(), "Low testing and high case rates (toward the top left of the graph) may indicate the presence of a large number of undetected cases. High testing and low case rates (toward the bottom right of the graph) may indicate the virus is being successfully contained or that a country may not be finding all positive cases.")))
      ),
      tabPanel("Cases",
               fluidRow(column(12, uiOutput("cases_graph"))),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all countries. The blue represents the mean for the highlighted countries. The light bands represent standard error.", tags$br(),"Cases have been scaled to represent the number of confirmed cases for every 100,000 people in each country, to simplify comparison betweeen countries.", tags$br(), "If a country is not testing many people, this number is probably lower than that country's actual infection rate as mild cases go undetected.")))
      ),
      tabPanel("Deaths",
               fluidRow(column(12, uiOutput("case_fatality_graph"))),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               fluidRow(column(12, helpText("The grey dotted line represents the conditional mean for all countries. The blue represents the mean for the highlighted countries. The light bands represent standard error.", tags$br(), "Also known as the 'case fatality rate' (or CFR), this number is calculated by dividing the number of detected cases by the number of reported deaths.", tags$br(), "If a country is not testing many people the CFR may be artifically high as mild cases go undetected. Inaccurately recording Covid-19 deaths may result in an artificially low CFR.")))
      )
    )
    )
  )
)

# Shiny server
server <- function(input, output, session) {
  # intro message
  observeEvent("", {
    showModal(modalDialog(
      easyClose = TRUE,
      title = tags$b("Counting Covid-19: International"),
      tags$b(tags$i("Please note: this app is based on a large dataset, and the graphs may take some time to load.")),
      tags$br(),
      tags$hr(),
      tags$b("What we know about the infection or death rate of Covid-19 depends on one thing:"),
      tags$br(),
      tags$b("how good are countries at counting the people who have Covid-19?"),
      tags$br(),
      tags$br(),
      "The number of people tested and confirmed to have the virus (usually described as 'cases' of the virus), is lower than the total number of people who have the virus (called 'infections') because not everyone who has the virus will be tested. Some people, especially those with mild symptoms or those unable to access the healthcare system, will not be tested. This difference between the number of 'cases' and the number of 'infections' can vary from country to country, and will impact that country's apparent number of cases and their apparent mortality rate. This is why there is such a range of rates across the globe.", "For more, see ", tags$a(href = "https://www.npr.org/sections/goatsandsoda/2020/03/27/821958435/why-death-rates-from-coronavirus-can-be-deceiving", "Why 'Death Rates' From Coronavirus Can Be Deceiving"), "or ", tags$a(href = "https://www.bbc.com/future/article/20200401-coronavirus-why-death-and-mortality-rates-differ", "Coronavirus: Why death and mortality rates differ"), ".",
      tags$br(),
      tags$br(),
      "Testing more people will result in better, more accurate data about Covid-19's infection and mortality rate, but what is it that makes certain countries better at testing than others? Is it money? Something about the population? Their system of healthcare?",
      tags$br(),
      tags$br(),
      tags$b("Exploring what characteristics are associated with increased testing, lower case rates, or lower case fatality rates might help explain what makes some countries better at counting cases of Covid-19 than others."),
      tags$br(),
      tags$hr(),
      tags$i("For information about combatting the spread of the virus, or about symptoms and treatment, there are a number of excellent resources run by infectious disease experts and medical professionals, including the ", tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "WHO"), "and ", tags$a(href = "https://www.cdc.gov/coronavirus/2019-nCoV/index.html", "CDC"), "for public health information, the ", tags$a(href = "https://www.nih.gov/health-information/coronavirus", "NIH"), "and ", tags$a(href = "https://www.gisaid.org/", "GISAID"), "for research information, and ", tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "JHU"), "for data."),
      tags$br(),
      tags$br(),
      footer = tagList(
        actionButton(inputId = "intro", label = tags$b("See how it works")))
    ))
  })
  # start intro tour
  observeEvent(input$intro,{
    removeModal()
  })
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Let's go!",
                                               "showStepNumbers" = "false"))
  )
  # general settings
  options(list(scipen = 99))
  # country selection
  output$countries <- renderUI({
    countrieslist <- unique(as.character(covid_cases$Country))
    pickerInput(
      inputId = "countriesinput", label = h5("Select countries to include in plot"),
      choices = countrieslist,
      selected = c("United States", "Spain", "Italy", "France", "Germany", "United Kingdom", "Turkey", "Iran", "China", "Russia", "Hungary", "Croatia", "Uzbekistan", "Oman", "Iceland", "Iraq", "Estonia", "Armenia", "Azerbaijan", "New Zealand", "Trinidand and Tobago", "Ethiopia", "Liberia", "Cambodia", "Madagascar", "Maldives", "Brunei", "Myanmar", "Gabon", "Sudan"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  # create minimal dataset
  min_covid_case <- reactive({
    select(covid_cases, Country, Date, Day, Tests, Cases, DeathRate, Measure) %>%
      filter(Country %in% input$countriesinput)
  })
  # enable inputs if variable is checked
  observeEvent(input$popcheck, {
    if (input$popcheck == FALSE) {
      disable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = 0,
        max = ceiling(max(covid_cases$Population_mil, na.rm = TRUE)),
        value = c(0, ceiling(max(covid_cases$Population_mil, na.rm = TRUE))),
        step = 25
      )
    } else {
      enable("popinput")
      updateSliderInput(
        session,
        inputId = "popinput",
        label = NULL,
        min = 0,
        max = ceiling(max(covid_cases$Population_mil, na.rm = TRUE)),
        value = c(0, ceiling(max(covid_cases$Population_mil, na.rm = TRUE))),
        step = 25
      )
    }
  })
  observeEvent(input$agecheck, {
    if (input$agecheck == FALSE) {
      disable("ageinput")
      updateSliderInput(
        session,
        inputId = "ageinput",
        label = NULL,
        min = floor(min(covid_cases$Over65_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Over65_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Over65_perc, na.rm = TRUE)), ceiling(max(covid_cases$Over65_perc, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("ageinput")
      updateSliderInput(
        session,
        inputId = "ageinput",
        label = NULL,
        min = floor(min(covid_cases$Over65_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Over65_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Over65_perc, na.rm = TRUE)), ceiling(max(covid_cases$Over65_perc, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$slumscheck, {
    if (input$slumscheck == FALSE) {
      disable("slumsinput")
      updateSliderInput(
        session,
        inputId = "slumsinput",
        label = NULL,
        min = floor(min(covid_cases$Slums_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Slums_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Slums_perc, na.rm = TRUE)), ceiling(max(covid_cases$Slums_perc, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("slumsinput")
      updateSliderInput(
        session,
        inputId = "slumsinput",
        label = NULL,
        min = floor(min(covid_cases$Slums_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Slums_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Slums_perc, na.rm = TRUE)), ceiling(max(covid_cases$Slums_perc, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$gdpcheck, {
    if (input$gdpcheck == FALSE) {
      disable("gdpinput")
      updateSliderInput(
        session,
        inputId = "gdpinput",
        label = NULL,
        min = floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
        max = ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
        value = c(floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)), ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE))),
        step = 100
      )
    } else {
      enable("gdpinput")
      updateSliderInput(
        session,
        inputId = "gdpinput",
        label = NULL,
        min = floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
        max = ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE)),
        value = c(floor(min(covid_cases$GDP_pcap_ppp, na.rm = TRUE)), ceiling(max(covid_cases$GDP_pcap_ppp, na.rm = TRUE))),
        step = 100
      )
    }
  })
  observeEvent(input$salariedcheck, {
    if (input$salariedcheck == FALSE) {
      disable("salariedinput")
      updateSliderInput(
        session,
        inputId = "salariedinput",
        label = NULL,
        min = floor(min(covid_cases$Salaried_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Salaried_perc, na.rm = TRUE)), ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("salariedinput")
      updateSliderInput(
        session,
        inputId = "salariedinput",
        label = NULL,
        min = floor(min(covid_cases$Salaried_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Salaried_perc, na.rm = TRUE)), ceiling(max(covid_cases$Salaried_perc, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$povertycheck, {
    if (input$povertycheck == FALSE) {
      disable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty_perc, na.rm = TRUE)), ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("povertyinput")
      updateSliderInput(
        session,
        inputId = "povertyinput",
        label = NULL,
        min = floor(min(covid_cases$Poverty_perc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$Poverty_perc, na.rm = TRUE)), ceiling(max(covid_cases$Poverty_perc, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$lifeexpcheck, {
    if (input$lifeexpcheck == FALSE) {
      disable("lifeexpinput")
      updateSliderInput(
        session,
        inputId = "lifeexpinput",
        label = NULL,
        min = floor(min(covid_cases$LifeExp, na.rm = TRUE)),
        max = ceiling(max(covid_cases$LifeExp, na.rm = TRUE)),
        value = c(floor(min(covid_cases$LifeExp, na.rm = TRUE)), ceiling(max(covid_cases$LifeExp, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("lifeexpinput")
      updateSliderInput(
        session,
        inputId = "lifeexpinput",
        label = NULL,
        min = floor(min(covid_cases$LifeExp, na.rm = TRUE)),
        max = ceiling(max(covid_cases$LifeExp, na.rm = TRUE)),
        value = c(floor(min(covid_cases$LifeExp, na.rm = TRUE)), ceiling(max(covid_cases$LifeExp, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$hospcheck, {
    if (input$hospcheck == FALSE) {
      disable("hospinput")
      updateSliderInput(
        session,
        inputId = "hospinput",
        label = NULL,
        min = floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("hospinput")
      updateSliderInput(
        session,
        inputId = "hospinput",
        label = NULL,
        min = floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HospBed_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$HospBed_per10thou, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$mdcheck, {
    if (input$mdcheck == FALSE) {
      disable("mdinput")
      updateSliderInput(
        session,
        inputId = "mdinput",
        label = NULL,
        min = floor(min(covid_cases$MD_per10thou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$MD_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("mdinput")
      updateSliderInput(
        session,
        inputId = "mdinput",
        label = NULL,
        min = floor(min(covid_cases$MD_per10thou, na.rm = TRUE)),
        max = ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE)),
        value = c(floor(min(covid_cases$MD_per10thou, na.rm = TRUE)), ceiling(max(covid_cases$MD_per10thou, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$hygienecheck, {
    if (input$hygienecheck == FALSE) {
      disable("hygieneinput")
      updateSliderInput(
        session,
        inputId = "hygieneinput",
        label = NULL,
        min = floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)), ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("hygieneinput")
      updateSliderInput(
        session,
        inputId = "hygieneinput",
        label = NULL,
        min = floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)),
        max = ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE)),
        value = c(floor(min(covid_cases$HygBasic_natperc, na.rm = TRUE)), ceiling(max(covid_cases$HygBasic_natperc, na.rm = TRUE))),
        step = 5
      )
    }
  })
  observeEvent(input$statscapacitycheck, {
    if (input$statscapacitycheck == FALSE) {
      disable("statscapacityinput")
      updateSliderInput(
        session,
        inputId = "statscapacityinput",
        label = NULL,
        min = floor(min(covid_cases$StatsCapacity, na.rm = TRUE)),
        max = ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE)),
        value = c(floor(min(covid_cases$StatsCapacity, na.rm = TRUE)), ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE))),
        step = 5
      )
    } else {
      enable("statscapacityinput")
      updateSliderInput(
        session,
        inputId = "statscapacityinput",
        label = NULL,
        min = floor(min(covid_cases$StatsCapacity, na.rm = TRUE)),
        max = ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE)),
        value = c(floor(min(covid_cases$StatsCapacity, na.rm = TRUE)), ceiling(max(covid_cases$StatsCapacity, na.rm = TRUE))),
        step = 5
      )
    }
  })
  # create selected dataset
  selected_covid_case <- reactive({
    popfilter <- quote(between(Population_mil, as.numeric(input$popinput[1]), as.numeric(input$popinput[2])))
    agefilter <- quote(between(Over65_perc, as.numeric(input$ageinput[1]), as.numeric(input$ageinput[2])))
    slumsfilter <- quote(between(Slums_perc, as.numeric(input$slumsinput[1]), as.numeric(input$slumsinput[2])))
    gdpfilter <- quote(between(GDP_pcap_ppp, as.numeric(input$gdpinput[1]), as.numeric(input$gdpinput[2])))
    salariedfilter <- quote(between(Salaried_perc, as.numeric(input$salariedinput[1]), as.numeric(input$salariedinput[2])))
    povertyfilter <- quote(between(Poverty_perc, as.numeric(input$povertyinput[1]), as.numeric(input$povertyinput[2])))
    lifeexpfilter <- quote(between(LifeExp, as.numeric(input$lifeexpinput[1]), as.numeric(input$lifeexpinput[2])))
    hospfilter <- quote(between(HospBed_per10thou, as.numeric(input$hospinput[1]), as.numeric(input$hospinput[2])))
    mdfilter <- quote(between(MD_per10thou, as.numeric(input$mdinput[1]), as.numeric(input$mdinput[2])))
    hygienefilter <- quote(between(HygBasic_natperc, as.numeric(input$hygieneinput[1]), as.numeric(input$hygieneinput[2])))
    statscapacityfilter <- quote(between(StatsCapacity, as.numeric(input$statscapacityinput[1]), as.numeric(input$statscapacityinput[2])))
    covid_cases %>%
      select(
        Country, Date, Day, Tests, Cases, DeathRate, Population_mil, Measure,
        if (input$popcheck == FALSE) {"Country"} else {"Population_mil"},
        if (input$agecheck == FALSE) {"Country"} else {"Over65_perc"},
        if (input$slumscheck == FALSE) {"Country"} else {"Slums_perc"},
        if (input$gdpcheck == FALSE) {"Country"} else {"GDP_pcap_ppp"},
        if (input$salariedcheck == FALSE) {"Country"} else {"Salaried_perc"},
        if (input$povertycheck == FALSE) {"Country"} else {"Poverty_perc"},
        if (input$lifeexpcheck == FALSE) {"Country"} else {"LifeExp"},
        if (input$hospcheck == FALSE) {"Country"} else {"HospBed_per10thou"},
        if (input$mdcheck == FALSE) {"Country"} else {"MD_per10thou"},
        if (input$hygienecheck == FALSE) {"Country"} else {"HygBasic_natperc"},
        if (input$statscapacitycheck == FALSE) {"Country"} else {"StatsCapacity"}
      ) %>%
    filter(
      Country %in% input$countriesinput,
      if (input$popcheck == FALSE) {!is.na(Country)} else {!!popfilter},
      if (input$agecheck == FALSE) {!is.na(Country)} else {!!agefilter},
      if (input$slumscheck == FALSE) {!is.na(Country)} else {!!slumsfilter},
      if (input$gdpcheck == FALSE) {!is.na(Country)} else {!!gdpfilter},
      if (input$salariedcheck == FALSE) {!is.na(Country)} else {!!salariedfilter},
      if (input$povertycheck == FALSE) {!is.na(Country)} else {!!povertyfilter},
      if (input$lifeexpcheck == FALSE) {!is.na(Country)} else {!!lifeexpfilter},
      if (input$hospcheck == FALSE) {!is.na(Country)} else {!!hospfilter},
      if (input$mdcheck == FALSE) {!is.na(Country)} else {!!mdfilter},
      if (input$hygienecheck == FALSE) {!is.na(Country)} else {!!hygienefilter},
      if (input$statscapacitycheck == FALSE) {!is.na(Country)} else {!!statscapacityfilter}
    )
  })
  # tests graph
  tests_plot <- reactive({
    validate(
      need(input$countriesinput != "", "Please select at least 1 country from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), Country) != ""), "There are no countries matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional countries from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), Tests) != ""), "That country has no testing data available."))
    plot <-
      with_options(list(digits = 1),
      ggplotly(
        ggplot(selected_covid_case()) +
          geom_line(data = min_covid_case(),
                    aes(x = Tests, y = Cases, group = Country,
                        text = paste(Country, "<br>Day: ", Day, "<br>Tests: ", round(Tests, digits = 1), "<br>Cases: ", round(Cases, digits = 1))), color = "#bdc3c7", alpha = .5, show.legend = FALSE) +
          geom_line(aes(x = Tests, y = Cases, color = Country, group = Country,
                        text = paste(Country, "<br>Day: ", Day, "<br>Tests: ", round(Tests, digits = 1), "<br>Cases: ", round(Cases, digits = 1))), show.legend = FALSE) +
          geom_smooth(aes(x = Tests, y = Cases), data = min_covid_case(),
                      method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
          geom_ribbon(aes(x = Tests, y = Cases), data = min_covid_case(),
                      stat = "smooth", method = "loess", alpha = .15) +
          geom_smooth(aes(x = Tests, y = Cases),
                      method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
          geom_ribbon(aes(x = Tests, y = Cases),
                      stat = "smooth", method = "loess", alpha = .15) +
          scale_x_log10(expand = c(0, 0)) +
          scale_y_log10(expand = c(0, 0)) +
          labs(
            title = "Covid-19 testing by confirmed infections",
            x = "Tests performed per 100,000 people", y = "Confirmed Covid-19 cases per 100,000 people") +
          theme(text = element_text(family = "Georgia"),
                panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
                plot.title = element_text(face = "italic"),
                plot.subtitle = element_text(face = "italic"),
                axis.title = element_text(face = "italic"),
                plot.caption = element_text(face = "italic"),
                panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks = element_blank(),
                axis.text.x = NULL,
                axis.line.x = element_line(colour = "#908f85"),
                plot.margin = unit(c(2, 1, 2, 1), "lines")),
        height = 600,
        tooltip = "text"
      )
      )
  })
  output$tests_plot <- renderPlotly({
    input$updategraph
    isolate({
      tests_plot() %>% add_trace(data = subset(selected_covid_case(), !is.na(Measure)), y = ~log10(Cases), x = ~log10(Tests),
                                 text = ~paste(Country, "<br>Day: ", Day, "<br>Tests: ", round(Tests, digits = 1), "<br>Cases: ", round(Cases, digits = 1), "<br>Policies enacted today: ", "<br>", Measure),
                                 color = I("#575D61"), mode = "markers", alpha = .7, marker = list(size = 7), showlegend = FALSE,
                                 hovertemplate = "%{text}<extra></extra>")
    })
  })
  output$tests_graph <- renderUI({
    withSpinner(
      plotlyOutput("tests_plot"),
      type = 1,
      color = "#3c8dbc"
    )
  })
  # cases graph
  cases_plot <- reactive({
    validate(
      need(input$countriesinput != "", "Please select at least 1 country from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), Country) != ""), "There are no countries matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional countries from the dropdown to the left."))
    plot <-
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(),
                aes(x = Day, y = Cases, group = Country,
                    text = paste(Country, "<br>Day: ", Day, "<br>Cases: ", round(Cases, digits = 1))),
                color = "#bdc3c7", alpha = .5, show.legend = FALSE) +
      geom_line(aes(x = Day, y = Cases, color = Country, group = Country,
                    text = paste(Country, "<br>Day: ", Day, "<br>Cases: ", round(Cases, digits = 1))),
                show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = Cases), data = min_covid_case(),
        method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases), data = min_covid_case(),
        stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = Cases),
        method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = Cases),
        stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = "Confirmed Covid-19 cases",
        x = "Days from 100th in-country case", y = "Detected cases per 100,000 people") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_log10(expand = c(0, 0)) +
      guides(shape = FALSE) +
      theme(text = element_text(family = "Georgia"),
        panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
        plot.title = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic"),
        axis.title = element_text(face = "italic"),
        plot.caption = element_text(face = "italic"),
        panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = NULL,
        axis.line.x = element_line(colour = "#908f85"),
        plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600,
      tooltip = "text"
      )
      )
  })
  output$cases_plot <- renderPlotly({
    input$updategraph
    isolate({
      cases_plot() %>% add_trace(data = subset(selected_covid_case(), !is.na(Measure)), y = ~log10(Cases), x = ~Day,
                                 text = ~paste(Country, "<br>Day: ", Day, "<br>Cases: ", round(Cases, digits = 1), "<br>Policies enacted today: ", "<br>", Measure),
                                 color = I("#575D61"), mode = "markers", alpha = .7, marker = list(size = 7), showlegend = FALSE,
                                 hovertemplate = "%{text}<extra></extra>")
    })
  })
  output$cases_graph <- renderUI({
      withSpinner(
        plotlyOutput("cases_plot"),
        type = 1,
        color = "#3c8dbc"
      )
  })
  # cfr graph
  case_fatality_plot <- reactive({
    validate(
      need(input$countriesinput != "", "Please select at least 1 country from the dropdown to the left."))
    validate(
      need(try(select(selected_covid_case(), Country) != ""), "There are no countries matching the selected criteria.\nPlease select fewer variables, adjust the range of those already selected, or add additional countries from the dropdown to the left."))
    plot <-
      with_options(list(digits = 1),
      ggplotly(
      ggplot(selected_covid_case()) +
      geom_line(data = min_covid_case(), aes(x = Day, y = DeathRate, group = Country,
                                             text = paste(Country, "<br>Day: ", Day, "<br>Death Rate: ", paste(round(100*DeathRate, 2), "%", sep = ""))),
                color = "#bdc3c7", show.legend = FALSE) +
      geom_line(aes(x = Day, y = DeathRate, color = Country, group = Country,
                    text = paste(Country, "<br>Day: ", Day, "<br>Death Rate: ", paste(round(100*DeathRate, 2), "%", sep = ""))), show.legend = FALSE) +
      geom_smooth(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    method = "loess", se = FALSE, color = "#bdc3c7", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate), data = min_covid_case(),
                    stat = "smooth", method = "loess", alpha = .15) +
      geom_smooth(aes(x = Day, y = DeathRate),
                    method = "loess", se = FALSE, color = "#3c8dbc", size = .5, alpha = .6, linetype = "dotted") +
      geom_ribbon(aes(x = Day, y = DeathRate),
                    stat = "smooth", method = "loess", alpha = .15) +
      labs(
        title = list(text = paste0("Reported Covid-19 death rate", "<br>", "<sup>",
                                   "","<sup>")),
        x = "Days from 100th in-country case", y = "Percent of detected cases resulting in a death") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), breaks = c(.001, .01, .05, .1, .15), labels = scales::percent) +
      theme(text = element_text(family = "Georgia"),
            panel.background = element_rect(fill = "#f7f5f0", colour = "#f7f5f0"),
            plot.title = element_text(face = "italic"),
            plot.subtitle = element_text(face = "italic"),
            axis.title = element_text(face = "italic"),
            plot.caption = element_text(face = "italic"),
            panel.grid.major = element_line(colour = "#D5D3CC", size = rel(.5)),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = NULL,
            axis.line.x = element_line(colour = "#908f85"),
            plot.margin = unit(c(2, 1, 2, 1), "lines")),
      height = 600,
      tooltip = "text"
      )
      )
  })
  output$case_fatality_plot <- renderPlotly({
    input$updategraph
    isolate({
      case_fatality_plot() %>% add_trace(data = subset(selected_covid_case(), !is.na(Measure)), y = ~DeathRate, x = ~Day,
                                         text = ~paste(Country, "<br>Day: ", Day, "<br>Death Rate: ", paste(round(100*DeathRate, 2), "%", sep = ""),  "<br>Policies enacted today: ", "<br>", Measure),
                                         color = I("#575D61"), mode = "markers", alpha = .7, marker = list(size = 7), showlegend = FALSE,
                                         hovertemplate = "%{text}<extra></extra>")
    })
  })
  output$case_fatality_graph <- renderUI({
    withSpinner(
      plotlyOutput("case_fatality_plot"),
      type = 1,
      color = "#3c8dbc"
    )
  })

}

# Shiny app
shinyApp(ui, server)

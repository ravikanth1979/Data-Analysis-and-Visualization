

library(shiny)
library(rstudioapi)
library(readr)
library(grid)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(plotly)
library(caret)
library(e1071)
library(gridExtra)
library(shinyjs)
library(shinythemes)

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
source("get_required_dataset.R")
require(gdata)
dataset <- read_csv("..\\API_21_DS2_en_excel_v2.csv", skip=3)
regions_list <- read_csv("regions.txt")
lpi_dataset = get_lpi_dataset(dataset)
tourism_dataset = get_tourism_dataset(dataset)
trade_dataset = get_trade_dataset(dataset)

shinyUI(dashboardPage(
  dashboardHeader(title = "The World Bank Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Logistics Performance Index", tabName = "logistics_performance_index", icon = icon("dashboard")),
      menuItem("International Tourism", tabName = "international_tourism", icon = icon("dashboard")),
      menuItem("Trade(Exports & Imports)", tabName = "exports_imports", icon = icon("dashboard")),
      sliderInput('plotHeight', 'Height of plot (in pixels)', 
                  min = 100, max = 650, value = 300)
      
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tabItems(
      # First tab content
      tabItem(tabName = "logistics_performance_index",
              
              tabsetPanel(type = "tabs",
                          tabPanel("Line Charts", 
                                   fluidPage(
                                     fluidRow(column(6, checkboxGroupInput("LineIndicator", "",
                                                                               c("Overall LPI score" = "LP.LPI.OVRL.XQ",
                                                                                 "Customs score" = "LP.LPI.CUST.XQ",
                                                                                 "Infrastructure Score" = "LP.LPI.INFR.XQ",
                                                                                 "International shipments score" = "LP.LPI.ITRN.XQ",
                                                                                 "Logistics quality and competence score" = "LP.LPI.LOGS.XQ",
                                                                                 "Tracking and tracing score" = "LP.LPI.TRAC.XQ",
                                                                                 "Timeliness Score" = "LP.LPI.TIME.XQ"))
                                     
                                     ),
                                     column(6, selectizeInput("LineCountry",
                                                                  "Countries",
                                                                  choices = sort(unique(lpi_dataset$Country_Name)),
                                                                  multiple = TRUE
                                                   ))
                                     ),
                                     fluidRow(plotlyOutput("plotLine")))),
                          tabPanel("Bar Charts", fluidPage(
                            fluidRow(column(6, checkboxGroupInput("BarIndicator", "",
                                                                      c("Overall LPI score" = "LP.LPI.OVRL.XQ",
                                                                        "Customs score" = "LP.LPI.CUST.XQ",
                                                                        "Infrastructure Score" = "LP.LPI.INFR.XQ",
                                                                        "International shipments score" = "LP.LPI.ITRN.XQ",
                                                                        "Logistics quality and competence score" = "LP.LPI.LOGS.XQ",
                                                                        "Tracking and tracing score" = "LP.LPI.TRAC.XQ",
                                                                        "Timeliness Score" = "LP.LPI.TIME.XQ"))
                            
                            ),
                            column(6, selectizeInput("BarCountry",
                                                         "Countries",
                                                         choices = sort(unique(lpi_dataset$Country_Name)),
                                                         multiple = TRUE
                                          )) , radioButtons("BarYear", "Year",
                                                             c("2007" = "2007",
                                                               "2010" = "2010",
                                                               "2012" = "2012",
                                                               "2014" = "2014")
                                                             ,selected = NULL, inline = TRUE,
                                                             width = NULL)),
                            fluidRow(plotlyOutput("plotBar")))),
                          tabPanel(title="Nonlinear Regression - Net Trade vs All LPI Indicators", value="trade_lpi_regression",
                                   fluidRow(column(12,radioButtons("trade_lpi_regression_year", "Year",
                                                                                  c("2007" = "2007",
                                                                                    "2010" = "2010",
                                                                                    "2012" = "2012",
                                                                                    "2014" = "2014")
                                                                                  ,selected = NULL, inline = TRUE,
                                                                                  width = NULL))),
                                   fluidRow(column(12, radioButtons("trade_lpi_regression_indicator", "",
                                                                             c("Overall LPI score vs. Net Trade" = "LP.LPI.OVRL.XQ",
                                                                               "Customs score vs. Net Trade" = "LP.LPI.CUST.XQ",
                                                                               "Infrastructure Score vs. Net Trade" = "LP.LPI.INFR.XQ",
                                                                               "International shipments score vs. Net Trade" = "LP.LPI.ITRN.XQ",
                                                                               "Logistics quality and competence score vs. Net Trade" = "LP.LPI.LOGS.XQ",
                                                                               "Tracking and tracing score vs. Net Trade" = "LP.LPI.TRAC.XQ",
                                                                               "Timeliness Score vs. Net Trade" = "LP.LPI.TIME.XQ")
                                                                                ,selected = NULL, inline = TRUE,
                                                                               width = NULL)
                                   )
                                   ),
                                   fluidRow(plotlyOutput("nonlinearregression"))
                                   ),

                          tabPanel(title="Linear Regression Analysis with LPI Factors", value="lpi_to_lpi_factors",
                                   fluidRow(column(6,radioButtons("RegressionYear", "Year",
                                                                                        c("2007" = "2007",
                                                                                          "2010" = "2010",
                                                                                          "2012" = "2012",
                                                                                          "2014" = "2014")
                                                                                        ,selected = NULL, inline = TRUE,
                                                                                        width = NULL))),
                                   fluidRow(column(12,radioButtons("RegressionIndicator", "Indicator",
                                                c("Customs vs Overall" = "LP.LPI.CUST.XQ",
                                                  "Infrastructure vs Overall" = "LP.LPI.INFR.XQ",
                                                  "International Shipement vs Overall" = "LP.LPI.ITRN.XQ",
                                                  "Logistics quality and competence vs Overall" = "LP.LPI.LOGS.XQ",
                                                  "Tracking and tracing vs Overall" = "LP.LPI.TRAC.XQ",
                                                  "Timelines vs Overall" = "LP.LPI.TIME.XQ"),
                                                selected = NULL, inline = TRUE,
                                                width = NULL))),
                                   fluidRow(column(7,plotlyOutput("regression")),
                                            column(5,verbatimTextOutput('regression_summary',placeholder = TRUE)))
                                   ),
                          tabPanel(title="Linear Regression Analysis between top countries vs bottom countries", value="lpi_to_top_bottom_factors",
                                   fluidRow(column(12,radioButtons("RegressionTopBottomIndicator", "Indicator",
                                                                   c("Customs vs Overall" = "LP.LPI.CUST.XQ",
                                                                     "Infrastructure vs Overall" = "LP.LPI.INFR.XQ",
                                                                     "International Shipement vs Overall" = "LP.LPI.ITRN.XQ",
                                                                     "Logistics quality and competence vs Overall" = "LP.LPI.LOGS.XQ",
                                                                     "Tracking and tracing vs Overall" = "LP.LPI.TRAC.XQ",
                                                                     "Timelines vs Overall" = "LP.LPI.TIME.XQ"),
                                                                   selected = NULL, inline = TRUE,
                                                                   width = NULL))),
                                   fluidRow(column(6,selectInput(inputId = "toplpicountries", label = "Top LPI Countries", choices = NULL)),
                                            column(6,selectInput(inputId = "bottomlpicountries", label = "Bottom LPI Countries", choices = NULL))),

                                   fluidRow(column(6,plotlyOutput("topregression")),
                                            column(6,plotlyOutput("bottomregression"))),
                                   fluidRow(column(6,verbatimTextOutput('regression_top_summary',placeholder = TRUE)),
                                            column(6,verbatimTextOutput('regression_bottom_summary',placeholder = TRUE))) 
                          ),

                          
                          
                          tabPanel(title="Global LPI Rankings", value="global_lpi_rankings",
                                   radioButtons("LPI_Raning_Year", "Year",
                                                               c("2007" = "2007",
                                                                 "2010" = "2010",
                                                                 "2012" = "2012",
                                                                 "2014" = "2014")
                                                               ,selected = NULL, inline = TRUE,
                                                                  width = NULL),
                                    fluidRow(DT::dataTableOutput("lpi_rankings")))
              
      )),
    tabItem(tabName = "international_tourism",
              tabsetPanel(type = "tabs", 
                          tabPanel(title="Line Charts", value="tourism_line_charts",
                                   fluidPage(
                                     fluidRow(column(4, radioButtons("line_relative_line", "",
                                                                     c("Line Chart" = "line_chart",
                                                                       "Relative Line Chart" = "relative_line_chart"),
                                                                     selected = NULL, inline = TRUE,
                                                                     width = NULL),
                                                     radioButtons("tourism_indicator_group",
                                                                  h5(""),
                                                                  c("Number of Arrival/Deprartures", 
                                                                    "Expenditure/Receipts" ,
                                                                    "Expenditures (% of total imports)/Receipts (% of total exports)"),
                                                                  inline = T
                                                     )),
                                              column(4,  hidden( checkboxGroupInput("Tourism_Line_arr_depart", "",
                                                                            c("International tourism, number of arrivals" = "ST.INT.ARVL",
                                                                              "International tourism, number of departures" = "ST.INT.DPRT"))),
                                                         hidden( checkboxGroupInput("Tourism_Line_recipts_expenditure", "",
                                                                                c("International tourism, receipts (current US$)" = "ST.INT.RCPT.CD",
                                                                                  "International tourism, expenditures (current US$)" = "ST.INT.XPND.CD",
                                                                                  "International tourism, receipts for passenger transport items (current US$)" = "ST.INT.TRNR.CD",
                                                                                  "International tourism, expenditures for passenger transport items (current US$)" = "ST.INT.TRNX.CD"))),
                                                         hidden( checkboxGroupInput("Tourism_Line_100_receipts_expenditure", "",
                                                                                c("International tourism, receipts (% of total exports)" = "ST.INT.RCPT.XP.ZS",
                                                                                  "International tourism, expenditures (% of total imports)" = "ST.INT.XPND.MP.ZS")))),
                                              column(4, selectizeInput("tourism_line_countries",
                                                                           "Country List",
                                                                           choices = sort(unique(tourism_dataset$Country_Name)),
                                                                           multiple = TRUE))),
                                          
                                     fluidRow(plotlyOutput("plotLineTourism"))
                                   )
                                   ),
                          tabPanel(title="Bar Charts", value="tourism_bar_charts",
                                        
                                  fluidRow(column(3, radioButtons("tourism_bar_indicator_group",
                                                                  h5(""),
                                                                  c("Number of Arrival/Deprartures", 
                                                                    "Expenditure/Receipts" ,
                                                                    "Expenditures (% of total imports)/Receipts (% of total exports)"),
                                                                  inline = T
                                  )),
                                  column(2,  hidden( checkboxGroupInput("Tourism_Bar_arr_depart", "",
                                                                        c("International tourism, number of arrivals" = "ST.INT.ARVL",
                                                                          "International tourism, number of departures" = "ST.INT.DPRT"))),
                                         hidden( checkboxGroupInput("Tourism_Bar_recipts_expenditure", "",
                                                                    c("International tourism, receipts (current US$)" = "ST.INT.RCPT.CD",
                                                                      "International tourism, expenditures (current US$)" = "ST.INT.XPND.CD",
                                                                      "International tourism, receipts for passenger transport items (current US$)" = "ST.INT.TRNR.CD",
                                                                      "International tourism, expenditures for passenger transport items (current US$)" = "ST.INT.TRNX.CD"))),
                                         hidden( checkboxGroupInput("Tourism_Bar_100_receipts_expenditure", "",
                                                                    c("International tourism, receipts (% of total exports)" = "ST.INT.RCPT.XP.ZS",
                                                                      "International tourism, expenditures (% of total imports)" = "ST.INT.XPND.MP.ZS")))),
                                  column(4, selectizeInput("tourism_bar_countries",
                                                               "Countries",
                                                               choices = sort(unique(tourism_dataset$Country_Name)),
                                                               multiple = TRUE)),
                                  column(2, sliderInput('bar_year', 'Year', 
                                                        min = 1995, max = 2014, value = 2009))),
                                  
                                  fluidRow(plotlyOutput("plotBarTourism"))
                                  )


      )),
    tabItem(tabName = "exports_imports",
            tabsetPanel(type = "tabs",
                        tabPanel("Line Charts", 
                        fluidPage(fluidRow(column(4, radioButtons("trade_line_indicator_group",
                                                 h5(""),
                                                 c("Exports & Imports", 
                                                   "% of GDP" ,
                                                   "Anual % Growth"),
                                                 inline = T)),
                                           column(4, hidden( checkboxGroupInput("export_imports_us_dollar", "",
                                                                                 c("Exports of goods and services (current US$)" = "NE.EXP.GNFS.CD",
                                                                                   "Exports of goods and services (constant 2010 US$)" = "NE.EXP.GNFS.KD",
                                                                                   "Imports of goods and services (current US$)" = "NE.IMP.GNFS.CD",
                                                                                   "Imports of goods and services (constant 2010 US$)" = "NE.IMP.GNFS.KD",
                                                                                   "External balance on goods and services (current US$)" = "NE.RSB.GNFS.CD"))),
                                                  hidden( checkboxGroupInput("exports_imports_gdp_100", "",
                                                                             c("Exports of goods and services (% of GDP)" = "NE.EXP.GNFS.ZS",
                                                                               "Imports of goods and services (% of GDP)" = "NE.IMP.GNFS.ZS",
                                                                               "External balance on goods and services (% of GDP)" = "NE.RSB.GNFS.ZS",
                                                                               "Trade (% of GDP)" = "NE.TRD.GNFS.ZS"))),
                                                  hidden( checkboxGroupInput("anual_growth_100", "",
                                                                             c("Exports of goods and services (annual % growth)" = "NE.EXP.GNFS.KD.ZG",
                                                                               "Imports of goods and services (annual % growth)" = "NE.IMP.GNFS.KD.ZG")))),
                                                 column(4, selectizeInput("trade_line_countries",
                                                                              "Countries",
                                                                              choices = sort(unique(trade_dataset$Country_Name)),
                                                                              multiple = TRUE))
                                 ),
                                 fluidRow(plotlyOutput("plotLineTrade")))),
                        tabPanel("Bar Chart", 
                                 fluidPage(fluidRow(column(3, radioButtons("trade_bar_indicator_group",
                                                                           h5(""),
                                                                           c("Exports & Imports", 
                                                                             "% of GDP" ,
                                                                             "Anual % Growth"),
                                                                           inline = T)),
                                                    column(3, hidden( checkboxGroupInput("export_imports_bar_us_dollar", "",
                                                                                         c("Exports of goods and services (current US$)" = "NE.EXP.GNFS.CD",
                                                                                           "Exports of goods and services (constant 2010 US$)" = "NE.EXP.GNFS.KD",
                                                                                           "Imports of goods and services (current US$)" = "NE.IMP.GNFS.CD",
                                                                                           "Imports of goods and services (constant 2010 US$)" = "NE.IMP.GNFS.KD",
                                                                                           "External balance on goods and services (current US$)" = "NE.RSB.GNFS.CD"))),
                                                           hidden( checkboxGroupInput("exports_imports_bar_gdp_100", "",
                                                                                      c("Exports of goods and services (% of GDP)" = "NE.EXP.GNFS.ZS",
                                                                                        "Imports of goods and services (% of GDP)" = "NE.IMP.GNFS.ZS",
                                                                                        "External balance on goods and services (% of GDP)" = "NE.RSB.GNFS.ZS",
                                                                                        "Trade (% of GDP)" = "NE.TRD.GNFS.ZS"))),
                                                           hidden( checkboxGroupInput("anual_growth_bar_100", "",
                                                                                      c("Exports of goods and services (annual % growth)" = "NE.EXP.GNFS.KD.ZG",
                                                                                        "Imports of goods and services (annual % growth)" = "NE.IMP.GNFS.KD.ZG")))),
                                                    column(3, selectizeInput("trade_bar_countries",
                                                                             "Countries",
                                                                             choices = sort(unique(trade_dataset$Country_Name)),
                                                                             multiple = TRUE)),
                                                    column(2, sliderInput('bar_year_trade', 'Year', 
                                                                          min = 1995, max = 2014, value = 2009))
                                                    
                                 ),
                                 fluidRow(plotlyOutput("plotBarTrade")))),
                        tabPanel("Global Rankings", 
                                 fluidPage(fluidRow(column(4,selectInput("trade_year", label = h3("Select Year"), 
                                                         choices = sort(unique(trade_dataset$Year)), selected = 1)), 
                                                    column(4, radioButtons("trade_export_import",
                                                                           h5(" "),
                                                                           c("Imports"="NE.IMP.GNFS.CD", 
                                                                             "Exports"="NE.EXP.GNFS.CD"),
                                                                           inline = T)     
                                                         )),
                                           fluidRow(DT::dataTableOutput("exportimportrankings"))))

                
    )
)))))

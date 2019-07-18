#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

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

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
source("get_required_dataset.R")
source("get_relative_dataset.R")
require(gdata)
dataset <- read_csv("API_21_DS2_en_excel_v2.csv", skip=3)
lpi_dataset = get_lpi_dataset(dataset)
regions_list <- read_csv("regions.txt")
regions_list <- as.character(unlist(regions_list))
tourism_dataset = get_tourism_dataset(dataset)
tourism_relative_dataset = read_csv("relative_indicator_value.csv")
trade_dataset = get_trade_dataset(dataset)
net_trade_dataset = get_net_trade_dataset(dataset)
#net_trade_dataset$Indicator_Score = scale(net_trade_dataset$Indicator_Score)

shinyServer(function(input, output, session) {

  options(scipen=999)
  
  lpi_indicators_mapping<-list("LP.LPI.OVRL.XQ" = "Overall LPI score",
                           "LP.LPI.CUST.XQ" = "Customs score",
                           "LP.LPI.INFR.XQ" = "Infrastructure Score",
                           "LP.LPI.ITRN.XQ" = "International shipments score",
                           "LP.LPI.LOGS.XQ" = "Logistics quality and competence score",
                           "LP.LPI.TRAC.XQ" = "Tracking and tracing score",
                           "LP.LPI.TIME.XQ" = "Timeliness score")
  
  
  observe({
    
    
    
    if(input$tourism_indicator_group == "Number of Arrival/Deprartures") {
       show("Tourism_Line_arr_depart")
      updateCheckboxGroupInput(session, "Tourism_Line_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
       hide("Tourism_Line_recipts_expenditure")
       hide("Tourism_Line_100_receipts_expenditure")
    }
    if(input$tourism_indicator_group == "Expenditure/Receipts") {
      show("Tourism_Line_recipts_expenditure")
      updateCheckboxGroupInput(session, "Tourism_Line_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      hide("Tourism_Line_arr_depart")
      hide("Tourism_Line_100_receipts_expenditure")
    }
    if(input$tourism_indicator_group == "Expenditures (% of total imports)/Receipts (% of total exports)") {
      show("Tourism_Line_100_receipts_expenditure")
      updateCheckboxGroupInput(session, "Tourism_Line_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Line_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      hide("Tourism_Line_recipts_expenditure")
      hide("Tourism_Line_arr_depart")
    }

    if(input$tourism_bar_indicator_group == "Number of Arrival/Deprartures") {
      show("Tourism_Bar_arr_depart")
      updateCheckboxGroupInput(session, "Tourism_Bar_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      hide("Tourism_Bar_recipts_expenditure")
      hide("Tourism_Bar_100_receipts_expenditure")
    }
    if(input$tourism_bar_indicator_group == "Expenditure/Receipts") {
      show("Tourism_Bar_recipts_expenditure")
      updateCheckboxGroupInput(session, "Tourism_Bar_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      hide("Tourism_Bar_arr_depart")
      hide("Tourism_Bar_100_receipts_expenditure")
    }
    if(input$tourism_bar_indicator_group == "Expenditures (% of total imports)/Receipts (% of total exports)") {
      show("Tourism_Bar_100_receipts_expenditure")
      updateCheckboxGroupInput(session, "Tourism_Bar_arr_depart", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_recipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      updateCheckboxGroupInput(session, "Tourism_Bar_100_receipts_expenditure", label = NULL, choices = NULL, 
                               selected = c(""))
      hide("Tourism_Bar_recipts_expenditure")
      hide("Tourism_Bar_arr_depart")
    }


    if(input$trade_line_indicator_group == "Exports & Imports") {
      show("export_imports_us_dollar")
      updateCheckboxGroupInput(session, "export_imports_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("exports_imports_gdp_100")
      hide("anual_growth_100")
    }
    if(input$trade_line_indicator_group == "% of GDP") {
      show("exports_imports_gdp_100")
      updateCheckboxGroupInput(session, "export_imports_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("export_imports_us_dollar")
      hide("anual_growth_100")
    }
    if(input$trade_line_indicator_group == "Anual % Growth") {
      show("anual_growth_100")
      updateCheckboxGroupInput(session, "export_imports_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("export_imports_us_dollar")
      hide("exports_imports_gdp_100")
    }
    
    
    
    if(input$trade_bar_indicator_group == "Exports & Imports") {
      show("export_imports_bar_us_dollar")
      updateCheckboxGroupInput(session, "export_imports_bar_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_bar_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_bar_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("exports_imports_bar_gdp_100")
      hide("anual_growth_bar_100")
    }
    if(input$trade_bar_indicator_group == "% of GDP") {
      show("exports_imports_bar_gdp_100")
      updateCheckboxGroupInput(session, "export_imports_bar_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_bar_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_bar_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("export_imports_bar_us_dollar")
      hide("anual_growth_bar_100")
    }
    if(input$trade_bar_indicator_group == "Anual % Growth") {
      show("anual_growth_bar_100")
      updateCheckboxGroupInput(session, "export_imports_bar_us_dollar", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "exports_imports_bar_gdp_100", label = NULL, choices = NULL,
                               selected = c(""))
      updateCheckboxGroupInput(session, "anual_growth_bar_100", label = NULL, choices = NULL,
                               selected = c(""))
      hide("export_imports_bar_us_dollar")
      hide("exports_imports_bar_gdp_100")
    }
    
    
  })
  
  top_lpi_countries_list <- reactive({
    temp_data_set = lpi_dataset[(lpi_dataset$Indicator_Code == "LP.LPI.OVRL.XQ"),]
    temp_data_set = temp_data_set[order(temp_data_set$Indicator_Score, decreasing = TRUE),]  
  })
  
  bottom_lpi_countries_list <- reactive({
    temp_data_set = lpi_dataset[(lpi_dataset$Indicator_Code == "LP.LPI.OVRL.XQ"),]
    temp_data_set = temp_data_set[order(temp_data_set$Indicator_Score, decreasing = FALSE),]
  })
  
  top_lpi_countries_data <- reactive({
    lpi_dataset[(lpi_dataset$Country_Name %in% {input$toplpicountries}),]
  })
  
  bottom_lpi_countries_data <- reactive({
    lpi_dataset[(lpi_dataset$Country_Name %in% {input$bottomlpicountries}),]
  })
  
  
  observeEvent(top_lpi_countries_list(), {
    updateSelectInput(session = session, inputId = "toplpicountries", choices = head(unique(top_lpi_countries_list()$Country_Name),10))
  })
  
  observeEvent(bottom_lpi_countries_list(), {
    updateSelectInput(session = session, inputId = "bottomlpicountries", choices = head(unique(bottom_lpi_countries_list()$Country_Name),10))
  })
  
  # inputdata <- reactive({filedata$data[1:{input$num},]})
  lpi_line_inputdata <- reactive({ 
    lpi_dataset[lpi_dataset$Country_Name %in% {input$LineCountry} & lpi_dataset$Indicator_Code %in% {input$LineIndicator},]})
  
  tourism_line_inputdata<- reactive({
    if(input$line_relative_line == "line_chart") {
    tourism_line_dataset = tourism_dataset[tourism_dataset$Country_Name %in% {input$tourism_line_countries}
                    & ((tourism_dataset$Indicator_Code %in% {input$Tourism_Line_arr_depart}) |
                         (tourism_dataset$Indicator_Code %in% {input$Tourism_Line_recipts_expenditure}) |
                         (tourism_dataset$Indicator_Code %in% {input$Tourism_Line_100_receipts_expenditure}))
                    & input$line_relative_line == "line_chart",]
  }
  if(input$line_relative_line == "relative_line_chart") {

    tourism_line_dataset = tourism_relative_dataset[tourism_relative_dataset$Country_Name %in% {input$tourism_line_countries}
                    & ((tourism_relative_dataset$Indicator_Code %in% {input$Tourism_Line_arr_depart}) |
                         (tourism_relative_dataset$Indicator_Code %in% {input$Tourism_Line_recipts_expenditure}) |
                         (tourism_relative_dataset$Indicator_Code %in% {input$Tourism_Line_100_receipts_expenditure}))
                    & input$line_relative_line == "relative_line_chart",]
    tourism_line_dataset$Indicator_Score = tourism_line_dataset$relative_indicator_value
    print(tourism_line_dataset$Indicator_Score)
  }
 
    tourism_line_dataset
  })
  
  tourism_bar_inputdata<- reactive({
    tourism_dataset[tourism_dataset$Country_Name %in% {input$tourism_bar_countries}
                    & tourism_dataset$Year %in% {input$bar_year}
                    & ((tourism_dataset$Indicator_Code %in% {input$Tourism_Bar_arr_depart}) |
                         (tourism_dataset$Indicator_Code %in% {input$Tourism_Bar_recipts_expenditure}) |
                         (tourism_dataset$Indicator_Code %in% {input$Tourism_Bar_100_receipts_expenditure})),]
  })
  
  trade_line_inputdata<- reactive({
    trade_dataset[trade_dataset$Country_Name %in% {input$trade_line_countries}
                    & ((trade_dataset$Indicator_Code %in% {input$export_imports_us_dollar}) |
                         (trade_dataset$Indicator_Code %in% {input$exports_imports_gdp_100}) |
                         (trade_dataset$Indicator_Code %in% {input$anual_growth_100})),]
  })
  
  trade_bar_inputdata<- reactive({
    trade_dataset[trade_dataset$Country_Name %in% {input$trade_bar_countries}
                  & trade_dataset$Year %in% {input$bar_year_trade}
                  & ((trade_dataset$Indicator_Code %in% {input$export_imports_bar_us_dollar}) |
                       (trade_dataset$Indicator_Code %in% {input$exports_imports_bar_gdp_100}) |
                       (trade_dataset$Indicator_Code %in% {input$anual_growth_bar_100})),]
  })

  lpi_bar_inputdata <- reactive({ 
    lpi_dataset[lpi_dataset$Country_Name %in% {input$BarCountry} & 
                lpi_dataset$Indicator_Code %in% {input$BarIndicator} &
                lpi_dataset$Year %in% {input$BarYear},]})
  


  lpi_regression_inputdata <- reactive({
    lpi_dataset[lpi_dataset$Year %in% {input$RegressionYear} &
                                (lpi_dataset$Indicator_Code == {input$RegressionIndicator} | lpi_dataset$Indicator_Code == "LP.LPI.OVRL.XQ"),]

  })

  
  lpi_trade_regression_inputdata <- reactive({
    print("Inside lpi_trade_regression_inputdata....")
    lpi_trade_dataset <- lpi_dataset[lpi_dataset$Year %in% {input$trade_lpi_regression_year} &
                                     lpi_dataset$Indicator_Code %in% {input$trade_lpi_regression_indicator},]
    lpi_trade_dataset$trade_score <- NA
    print(lpi_trade_dataset)
    for(i in 1:nrow(lpi_trade_dataset)){
      
      print(lpi_trade_dataset$Year[i])
      print(lpi_trade_dataset$Country_Name[i])
      net_exp_temp_dataset <- trade_dataset[trade_dataset$Year == lpi_trade_dataset$Year[i] &
                          trade_dataset$Country_Name == lpi_trade_dataset$Country_Name[i] &  
                          trade_dataset$Indicator_Code == "NE.EXP.GNFS.CD",]
      net_imp_temp_dataset <- trade_dataset[trade_dataset$Year == lpi_trade_dataset$Year[i] &
                          trade_dataset$Country_Name == lpi_trade_dataset$Country_Name[i] &  
                          trade_dataset$Indicator_Code == "NE.IMP.GNFS.CD",]
      if(nrow(net_exp_temp_dataset)>0)
         lpi_trade_dataset$trade_score[i] = net_exp_temp_dataset$Indicator_Score + net_imp_temp_dataset$Indicator_Score
    }
    print(lpi_trade_dataset)
    
    row_nums = list()
    count = 1
    for(row in 1:nrow(lpi_trade_dataset)){
      if(sum(is.na(lpi_trade_dataset[row,]))>0){
        row_nums[count] = row
        count = count + 1
      }
    }
    lpi_trade_dataset = lpi_trade_dataset[-unlist(row_nums),]
    
    lpi_trade_dataset
  })

  lpi_ranking_inputdata <- reactive({
    temp_data_set = lpi_dataset[(lpi_dataset$Year %in% {input$LPI_Raning_Year}) &
                                  (lpi_dataset$Indicator_Code == "LP.LPI.OVRL.XQ"),]
    temp_data_set = temp_data_set[order(temp_data_set$Indicator_Score, decreasing = TRUE),]
    temp_data_set <- tibble::rowid_to_column(temp_data_set, "ID")
    temp_data_set = temp_data_set[,c(2,1,7,6)]
    
    names(temp_data_set) = c("Country","LPI Rank", "Overall LPI Score","Year" )
    
    temp_data_set
  })

  export_import_ranking_inputdata <- reactive({
    print({input$trade_export_import})
    # `%ni%` <- purrr::negate(`%in%`)
    temp_data_set = trade_dataset[(trade_dataset$Year %in% {input$trade_year}) &
                                  (trade_dataset$Indicator_Code == {input$trade_export_import}) &
                                  !(trade_dataset$Country_Name %in% regions_list),]
    temp_data_set = temp_data_set[order(temp_data_set$Indicator_Score, decreasing = TRUE),]
    temp_data_set <- tibble::rowid_to_column(temp_data_set, "ID")
    temp_data_set = temp_data_set[,c(2,1,7,6)]

    names(temp_data_set) = c("Country","Rank", "Score","Year" )

    temp_data_set
  })

  
  output$plotLine = renderPlotly({
    
    p <- ggplot(lpi_line_inputdata())+
      geom_line(aes(x = as.numeric(lpi_line_inputdata()$Year), y = lpi_line_inputdata()$Indicator_Score, colour = paste(Country_Name, "/", lpi_indicators_mapping[lpi_line_inputdata()$Indicator_Code]))) +
      geom_point(aes(x = as.numeric(lpi_line_inputdata()$Year), y = lpi_line_inputdata()$Indicator_Score))+
      labs (x = "Years", y = "Indicator Value", title = "Logistics Performance Index(1=low to 5=high)", colour = "")+
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )
    
    ggplotly(p, height = input$plotHeight)  %>%
      layout(autosize=TRUE)
  })
  
  output$plotLineTourism = renderPlotly({
    tourism_line_inputdata <- ggplot(tourism_line_inputdata())+
      geom_line(aes(x = as.numeric(tourism_line_inputdata()$Year), y = tourism_line_inputdata()$Indicator_Score, colour = paste(Country_Name, "/", tourism_line_inputdata()$Indicator_Name))) +
      geom_point(aes(x = as.numeric(tourism_line_inputdata()$Year), y = tourism_line_inputdata()$Indicator_Score))+
      labs (x = "Years", y = "Indicator Value", title = "International Tourism", colour = "Country/Indicator Name")+
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11)) 
      ggplotly(tourism_line_inputdata, height = input$plotHeight)  %>%
      layout(autosize=TRUE)
  })
  
  
  output$plotLineTrade = renderPlotly({
    trade_line_plot <- ggplot(trade_line_inputdata())+
      geom_line(aes(x = as.numeric(trade_line_inputdata()$Year), y = trade_line_inputdata()$Indicator_Score, colour = paste(Country_Name, "/", trade_line_inputdata()$Indicator_Name))) +
      geom_point(aes(x = as.numeric(trade_line_inputdata()$Year), y = trade_line_inputdata()$Indicator_Score))+
      labs (x = "Years", y = "Indicator Value", title = "International Trade(Exports/Imports)", colour = "Country/Indicator Name")+
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11))
    ggplotly(trade_line_plot, height = input$plotHeight)  %>%
      layout(autosize=TRUE)
  })


  output$plotBarTourism = renderPlotly({
    print(tourism_bar_inputdata())
    
    tourism_data <- tourism_bar_inputdata()
    
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.ARVL"] <- "Number of arrivals"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.DPRT"] <- "Number of departures"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.RCPT.CD"] <- "Receipts (current US$)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.RCPT.XP.ZS"] <- "Receipts (% of total exports)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.TRNR.CD"] <- "Receipts for passenger transport items (current US$)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.TRNX.CD"] <- "Expenditures for passenger transport items (current US$)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.TVLR.CD"] <- "Receipts for travel items (current US$)Tracking and tracing"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.TVLX.CD"] <- "Expenditures for travel items (current US$)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.XPND.CD"] <- "Expenditures (current US$)"
    tourism_data$Indicator_Code[tourism_data$Indicator_Code== "ST.INT.XPND.MP.ZS"] <- "Expenditures (% of total imports)"
    
    p <- ggplot(tourism_bar_inputdata())+
      geom_bar(aes(x = tourism_data$Indicator_Code, y = tourism_data$Indicator_Score, fill = Country_Name), width=0.5, stat = "identity", position=position_dodge()) +
      coord_flip() +
      labs (x = "Indicators", y = "Indicator Value", title = "International tourism", fill="") +
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )
    
    
    ggplotly(p, height = input$plotHeight)  %>%
      layout(autosize=TRUE)

  })
  
  output$plotBarTrade = renderPlotly({
    print(trade_bar_inputdata())
    
    trade_data <- trade_bar_inputdata()

    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.EXP.GNFS.CD"] <- "Exports of goods and services (current US$)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.EXP.GNFS.KD"] <- "Exports of goods and services (constant 2010 US$)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.EXP.GNFS.KD.ZG"] <- "Exports of goods and services (annual % growth)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.EXP.GNFS.ZS"] <- "Exports of goods and services (% of GDP)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.IMP.GNFS.CD"] <- "Imports of goods and services (current US$)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.IMP.GNFS.KD"] <- "Imports of goods and services (constant 2010 US$)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.IMP.GNFS.KD.ZG"] <- "Imports of goods and services (annual % growth)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.IMP.GNFS.ZS"] <- "Imports of goods and services (% of GDP)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.RSB.GNFS.CD"] <- "External balance on goods and services (current US$)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.RSB.GNFS.ZS"] <- "External balance on goods and services (% of GDP)"
    trade_data$Indicator_Code[trade_data$Indicator_Code== "NE.TRD.GNFS.ZS"] <- "Trade (% of GDP)"

    p <- ggplot(trade_data)+
      geom_bar(aes(x = trade_data$Indicator_Code, y = trade_data$Indicator_Score, fill = Country_Name), width=0.5, stat = "identity", position=position_dodge()) +
      coord_flip() +
      labs (x = "Indicators", y = "Indicator Value", title = "International tourism", fill="") +
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )
    
    
    ggplotly(p, height = input$plotHeight)  %>%
      layout(autosize=TRUE)
    
  })
  

  
  output$plotBar = renderPlotly({
    
    lpi_bar_data = lpi_bar_inputdata()
    
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.OVRL.XQ"] <- "Overall LPI"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.CUST.XQ"] <- "Customs"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.INFR.XQ"] <- "Infrastructure"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.ITRN.XQ"] <- "International shipments"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.LOGS.XQ"] <- "Logistics quality and competence"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.TRAC.XQ"] <- "Tracking and tracing"
    lpi_bar_data$Indicator_Code[lpi_bar_data$Indicator_Code== "LP.LPI.TIME.XQ"] <- "Timeliness"

    p <- ggplot(lpi_bar_data)+
      geom_bar(aes(x = lpi_bar_data$Indicator_Code, y = lpi_bar_data$Indicator_Score, fill = Country_Name), width=0.5, stat = "identity", position=position_dodge()) +
      labs (x = "Indicators", y = "Indicator Value", title = "Logistics Performance Index(1=low to 5=high)", fill="") +
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )
 
    ggplotly(p, height = input$plotHeight)  %>%
      layout(autosize=TRUE)
  })
  
  output$nonlinearregression <- renderPlotly({
    
    lpi_trade_dataset <- lpi_trade_regression_inputdata()
    print(lpi_trade_dataset)
    lpi_new_trade_dataset = lpi_trade_dataset[order(lpi_trade_dataset$Indicator_Score, decreasing = TRUE),]
    lpi_new_trade_dataset$color = NA
    i = 1
    for(i in 1:nrow(lpi_new_trade_dataset)){
      if(i<=37) lpi_new_trade_dataset$color[i] = "red"
      else lpi_new_trade_dataset$color[i] = "blue"
    }
    print(lpi_new_trade_dataset$color)
    x_label <- lpi_indicators_mapping[{input$trade_lpi_regression_indicator}]
    
    overall_trade_score <-  ggplot(data = lpi_new_trade_dataset, aes(x = Indicator_Score, y = trade_score))+ 
                      labs (x = as.character(x_label) , y = "Net Trade" , title = paste("Regression ",x_label," vs Total Trade"))+
                      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )+
                      geom_point(colour=lpi_new_trade_dataset$color) + 
                      geom_smooth(method = 'nls', formula = y ~ (a * ((x) ** b)), 
                                  start = c(a = 0.001, b = 0.001), se = FALSE) +
                      ggtitle(paste("Nonlinear Regression - ",x_label,"vs. Net Trade"))
    
      ggplotly(overall_trade_score, height = input$plotHeight) %>%
      layout(autosize=TRUE)

    })
  
  output$nonlinearcolorregression <- renderPlotly({
    
    lpi_trade_dataset <- lpi_trade_regression_inputdata()
    print(lpi_trade_dataset)
    
    customs_score <-  ggplot(data = lpi_trade_dataset, aes(x = Indicator_Score, y = trade_score,  color= "deepblue")) + 
      labs (x = "Overall LPI Score" , y = "Total Trade" , title = "Regression Overall LPI Score vs Total Trade", color=" ")+
      theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )+
      geom_point()+ 
      geom_smooth(method = 'nls', formula = y ~ exp(a * x + b), 
                  start = c(a = 0.001, b = 3), se = FALSE) +
      ggtitle("Nonlinear Regression - Net Trade vs. Overall LPI")
    
    ggplotly(customs_score, height = input$plotHeight) %>%
      layout(autosize=TRUE)
    
  })
  
  output$regression <- renderPlotly({

    if(length(lpi_regression_inputdata()[,1]) > 0){

      temp_dataset = list()
      `%not_in%` <- purrr::negate(`%in%`)
      temp_dataset$otherindicator = lpi_regression_inputdata()$Indicator_Score[lpi_regression_inputdata()$Indicator_Code=="LP.LPI.OVRL.XQ"]

      countries_list = lpi_regression_inputdata()$Country_Name[lpi_regression_inputdata()$Indicator_Code=="LP.LPI.OVRL.XQ"]
      print(lpi_regression_inputdata()$Country_Name[lpi_regression_inputdata()$Country_Name %not_in% countries_list])
      print(countries_list)
      temp_dataset$mainindicator = lpi_regression_inputdata()$Indicator_Score[(lpi_regression_inputdata()$Indicator_Code=={input$RegressionIndicator}) 
                                                                        & (lpi_regression_inputdata()$Country_Name %in% countries_list)] 
       
      print(length(temp_dataset$otherindicator))
      print(length(temp_dataset$mainindicator))
      
      x_label <- lpi_indicators_mapping[{input$RegressionIndicator}]
      
      Overall.LPI.Score = temp_dataset$mainindicator
      
      regressor = lm(formula = Overall.LPI.Score ~ temp_dataset$otherindicator, data = as.data.frame(temp_dataset))
      print(summary(regressor))
      
      customs_score <- ggplot(as.data.frame(temp_dataset),aes(x=temp_dataset$otherindicator, y=Overall.LPI.Score)) +
        labs (x = as.character(x_label) , y = "Overall Score", title = paste("Regression (", x_label," vs Overall score)"))+
        theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )+
        geom_point(colour="deepskyblue4") +
        geom_smooth(method="lm", fullrange = TRUE, formula = y~x)
        ggtitle("Regression Plot")      
        ggplotly(customs_score, height = input$plotHeight) %>%
        layout(autosize=TRUE)
    }})
  
  output$topregression <- renderPlotly({
    
    if(length(top_lpi_countries_data()[,1]) > 0){
      
      temp_dataset = list()
      temp_dataset$otherindicator = top_lpi_countries_data()$Indicator_Score[top_lpi_countries_data()$Indicator_Code=="LP.LPI.OVRL.XQ"]

      temp_dataset$mainindicator = top_lpi_countries_data()$Indicator_Score[(top_lpi_countries_data()$Indicator_Code=={input$RegressionTopBottomIndicator})] 
      
      print(length(temp_dataset$otherindicator))
      print(length(temp_dataset$mainindicator))
      
      x_label <- lpi_indicators_mapping[{input$RegressionIndicator}]
      
      Overall.LPI.Score = temp_dataset$mainindicator
      
      regressor = lm(formula = Overall.LPI.Score ~ temp_dataset$otherindicator, data = as.data.frame(temp_dataset))
      print(summary(regressor))
      
      customs_score <- ggplot(as.data.frame(temp_dataset),aes(x=temp_dataset$otherindicator, y=Overall.LPI.Score)) +
        labs (x = as.character(x_label) , y = "Overall Score", title = paste("Regression (", x_label," vs Overall score)"))+
        theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )+
        geom_point(colour="deepskyblue4") +
        geom_smooth(method="lm", fullrange = TRUE, formula = y~x)
      ggtitle("Regression Plot")      
      ggplotly(customs_score, height = input$plotHeight) %>%
        layout(autosize=TRUE)
    }})
  
  
  output$bottomregression <- renderPlotly({
    
    if(length(bottom_lpi_countries_data()[,1]) > 0){
      
      temp_dataset = list()

      temp_dataset$otherindicator = bottom_lpi_countries_data()$Indicator_Score[bottom_lpi_countries_data()$Indicator_Code=="LP.LPI.OVRL.XQ"]

      temp_dataset$mainindicator = bottom_lpi_countries_data()$Indicator_Score[(bottom_lpi_countries_data()$Indicator_Code=={input$RegressionTopBottomIndicator})] 
      
      print(length(temp_dataset$otherindicator))
      print(length(temp_dataset$mainindicator))
      
      x_label <- lpi_indicators_mapping[{input$RegressionIndicator}]
      
      Overall.LPI.Score = temp_dataset$mainindicator
      
      regressor = lm(formula = Overall.LPI.Score ~ temp_dataset$otherindicator, data = as.data.frame(temp_dataset))
      print(summary(regressor))
      
      customs_score <- ggplot(as.data.frame(temp_dataset),aes(x=temp_dataset$otherindicator, y=Overall.LPI.Score)) +
        labs (x = as.character(x_label) , y = "Overall Score", title = paste("Regression (", x_label," vs Overall score)"))+
        theme(axis.text = element_text(face = "bold.italic", color = "black", size = 10), title = element_text(face = "bold.italic", color = "black", size = 11) )+
        geom_point(colour="deepskyblue4") +
        geom_smooth(method="lm", fullrange = TRUE, formula = y~x)
      ggtitle("Regression Plot")      
      ggplotly(customs_score, height = input$plotHeight) %>%
        layout(autosize=TRUE)
    }})

  output$regression_summary <- renderPrint({
    
    if(length(lpi_regression_inputdata()[,1]) > 0){
      
      temp_dataset = list()
      temp_dataset$otherindicator = lpi_regression_inputdata()$Indicator_Score[lpi_regression_inputdata()$Indicator_Code=="LP.LPI.OVRL.XQ"]
      
      countries_list = lpi_regression_inputdata()$Country_Name[lpi_regression_inputdata()$Indicator_Code=="LP.LPI.OVRL.XQ"]

      temp_dataset$mainindicator = lpi_regression_inputdata()$Indicator_Score[(lpi_regression_inputdata()$Indicator_Code=={input$RegressionIndicator}) 
                                                                              & (lpi_regression_inputdata()$Country_Name %in% countries_list)] 

      x_label <- lpi_indicators_mapping[{input$RegressionIndicator}]
      summary(lm(formula = mainindicator ~ otherindicator, data = temp_dataset)) 

    }})
  
  output$regression_top_summary <- renderPrint({
    if(length(top_lpi_countries_data()[,1]) > 0){
      temp_dataset = list()
      temp_dataset$otherindicator = top_lpi_countries_data()$Indicator_Score[top_lpi_countries_data()$Indicator_Code=="LP.LPI.OVRL.XQ"]
      temp_dataset$mainindicator = top_lpi_countries_data()$Indicator_Score[(top_lpi_countries_data()$Indicator_Code=={input$RegressionTopBottomIndicator})] 
      summary(lm(formula = mainindicator ~ otherindicator, data = temp_dataset)) 
    }})
  
  output$regression_bottom_summary <- renderPrint({
    if(length(bottom_lpi_countries_data()[,1]) > 0){
      temp_dataset = list()
      temp_dataset$otherindicator = bottom_lpi_countries_data()$Indicator_Score[bottom_lpi_countries_data()$Indicator_Code=="LP.LPI.OVRL.XQ"]
      temp_dataset$mainindicator = bottom_lpi_countries_data()$Indicator_Score[(bottom_lpi_countries_data()$Indicator_Code=={input$RegressionTopBottomIndicator})] 
      summary(lm(formula = mainindicator ~ otherindicator, data = temp_dataset)) 
      }})

  output$nonlinear_regression_summary <- renderPrint({
    
      lpi_trade_dataset <- lpi_trade_regression_inputdata()

      nls_regression <- nls(data = lpi_trade_dataset, formula = lpi_trade_dataset$trade_score ~ exp(a * lpi_trade_dataset$Indicator_Score + b), 
                            start = c(a = 0.001, b = 1), model = TRUE)
      
      summary(nls_regression) 
      
    })

  output$lpi_rankings <- DT::renderDataTable({
   DT::datatable(lpi_ranking_inputdata(),  rownames = FALSE)
  })
  
  output$exportimportrankings <- DT::renderDataTable({
    DT::datatable(export_import_ranking_inputdata(),  rownames = FALSE)
  })

})

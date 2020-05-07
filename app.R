library(fullPage)
library(shiny)
library(tidyverse)
library(highcharter)
library(viridis)
library(shinyWidgets)

#===== DATASETS =====
load("gridwatch.RData")

download_btns <- c('downloadPNG', 'downloadCSV') 

#==== UI =====

ui <- pagePiling(
  tags$head(
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css")
  ),
  
  center = TRUE,
  sections.color = c(
    "#4B97D2", "#deebf7", "#deebf7",
    "#deebf7","#deebf7","#deebf7",
    "#deebf7","#deebf7","#deebf7"),
  
  menu = c(
    "Intro" = "intro",
    "Sources of Energy" = "p2",
    "Rise of Renewables" = "p3",
    "Fuels & Emissions" = "p4",
    "Energy use 2019" = "p5",
    "Renewables in 2019" = "p6",
    "Exceptional Days" = "p7",
    "Clocks Change" = "p8",
    "Lockdown" = "p9",
    "About" = "p10"
  ),

  pageSectionImage(
    menu = "intro",
    img = "wind_turbines.jpg",
    tags$a(tags$img(src = "strapline.png", width = "250px"),href = "https://www.cultureofinsight.com/", target = "_blank", style = "color:white"),
    tags$br(),tags$br(),tags$br(),
    h1("UK Energy Consumption", style = "color: black; font-size: 66px;"),
    tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
    tags$br(),tags$br(),tags$br(),tags$br(),
    h4("With the news that in April 2020 the UK broke its record for the number of days without coal power contributing to national energy use,", 
       tags$a(href = "https://www.cultureofinsight.com/","Culture of Insight", target = "_blank", style = "color:white"), "decided to look into the data.",
       style = "color: white;"),
    h4("Combining the excellent data from Gridwatch with DEFRA and Met Office data, we were able to uncover some interesting insights into the UK's use of Energy, and the path towards sustainability.",
       style = "color: white;")
  ),

  pageSection(
    menu = "p2",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
          h1("The end of Coal", style = "text-align:left;"),
          p("Data for these analyses come largely from the excellent", 
            tags$a(href = "https://www.gridwatch.templar.co.uk/","Gridwatch", target = "_blank"),
            "which has collated data on UK energy sources and consumption on a quarter-hourly basis since 2011",
            style = "text-align:left;"),
          p("Coal has been rapidly replaced over the last five years 
            as the primary source of UK energy.", style = "text-align:left;"),
          p("In 2019 it accounted for only 2% of UK output, behind Gas at 39%, and Nuclear and Wind power 
            both on 19%. The Government plans to phase out Coal power 
            entirely by 2025", style = "text-align:left;"),
          p("You can interact with the chart: hover over for tooltips and export data with the menu top right.",
          style = "text-align:left; color:#636363")),
        column(7,highchartOutput("heatmap")),
        column(1)
        )
      )
    ),
  
  pageSection(
    menu = "p3",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("The rise of Renewables", style = "text-align:left;"),
               p("Renewable sources (Wind, Biomass, Solar and Hydro) 
                 made up 25% of all UK power output in 2019.",
                 style = "text-align:left;"),
               p("Meanwhile, the UK imported 8% of its total energy from
                 overseas via interconnectors with Belgium, France, Holland
                 and Ireland which can send energy in either direction.",
                 style = "text-align:left;"),
               p("You can interact with the chart: hide or show data series by
                 clicking on the legend, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(7,highchartOutput("renewables")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p4",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("Burnt Fuels & Emissions", style = "text-align:left;"),
               p("Government statistics on", tags$a(href = "https://www.gov.uk/government/statistics/final-uk-greenhouse-gas-emissions-national-statistics-1990-to-2018","Greenhouse Gas emissions", target = "_blank"),
                 "report that CO2 output from energy production halved between 2012 and 2018.",
                 style = "text-align:left;"),
               p("This appears to have been helped by cuts in coal energy production,
                 even if these have been largely replaced by gas, and recently burning biomass",
                 style = "text-align:left;"),
               p("You can interact with the chart: hide or show data series by
                 clicking on the legend, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(7,highchartOutput("clean")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p5",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("Energy Usage in 2019", style = "text-align:left;"),
               p("Data for 2019 show the relationship between the seasons and energy
                 use, with a fall of around 1/3 between the cold, dark winter months
                 and the summer.",
                 style = "text-align:left;"),
               p("Weekends also stand out in the data, with energy usage 
                 around 20% lower than weekdays at the same time of year.", 
                 style = "text-align:left;"),
               p("You can interact with the chart: hover over data for tooltips, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(7,highchartOutput("days2019")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p6",
    fluidPage(
      fluidRow(
        column(1),
        column(3, h1("Seasonality of Renewable Sources", style = "text-align:left;"),
               p("Renewables like solar and wind power depend entirely on
                 Mother Nature's generosity, and energy is expensive to store,
                 so it's lucky that we usually get more sun in summer and more wind in winter.",
                 style = "text-align:left;"),
               p("You can see the relationship between the weather and
                 power from solar and wind in the chart opposite.",
                 style = "text-align:left;"),
               p("The height of the
                 bars indicate the energy contributed from either solar or wind power, and the
                 colour of the bars show average temperature or wind speed for the same day.",
                 style = "text-align:left;"),
               p("This analysis combines data from", 
                 tags$a(href = "https://www.gridwatch.templar.co.uk/","Gridwatch", target = "_blank"),
                 "with daily 2019 wind data from",
                 tags$a(href = "http://nw3weather.co.uk/","NW3 Weather", target = "_blank"),
                 "and temperature from",
                 tags$a(href = "https://www.metoffice.gov.uk/hadobs/hadcet/data/download.html","the Met Office", target = "_blank"),
                 style = "text-align:left;"),
               p("You can interact with the chart: hover over data for tooltips, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(2, prettyRadioButtons("source",label = "Choose Energy Source",
                                    choices = c("Solar" = "solar",
                                                "Wind" = "wind"),
                                    status = "success",
                                    icon = icon("check"), 
                                    bigger = TRUE,
                                    inline = T)),
        column(5, highchartOutput("sunburst")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p7",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("High & Low Days", style = "text-align:left;"),
               p("As we've seen, weekends see less energy demand than weekdays. A look
                 at the pattern of energy demand over these types of days also illustrates
                 how demand rises less quickly in the mornings at weekends.",
                 style = "text-align:left;"),
               p("Energy demand on exceptional days, like Christmas or sporting events,
                   also differs from the average.",
                 style = "text-align:left;"),
               p("Thursday 31st Jan, with the highest daily demand, was the coldest day in 2019 with an average -1.8C,
                   while Friday 23rd August with the lowest demand was the third hottest at 22.5C.",
                 style = "text-align:left;"),
               p("Xmas day shows a lunchtime peak from all those ovens, 
                 and the Rugby World Cup Final shows a sharp rise for the GMT morning kick-off.",
                 style = "text-align:left;"),
               p("Select a day to compare to the average from the menu.",
                 style = "text-align:left;"),
               p("You can interact with the chart: hide or show data series by
                 clicking on the legend, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")
          ),
        column(7,prettyRadioButtons("topdays",label = "Compare these days to the average Weekday and Weekend",
                                   choices = c("Christmas Day" = "Xmas Day",
                                               "Highest Demand" = "Thurs 31 Jan 2019",
                                               "Lowest Demand" = "Fri 23 Aug 2019",
                                               "New Year's Eve",
                                               "New Year's Day",
                                               "Rugby World Cup Final"),
                                   icon = icon("check"), 
                                   bigger = TRUE,
                                   status = "info",
                                   inline = T),
               highchartOutput("days")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p8",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("When the Clocks Change", style = "text-align:left;"),
               p("A closer look at the weeks immediately before and after the clocks
                 go back, which happened on 27th October 2019, shows the immediate
                 effect on energy consumption.",
                 style = "text-align:left;"),
               p("A one-hour offset can be observed in the times of day when demand rises
                 and falls, as data are recorded at consistent GMT times.",
                 style = "text-align:left;"),
               p("There is also a notable difference in energy use outside the working day,
                 with both the early morning and evening showing higher demand in the week 
                 after the change.",
                 style = "text-align:left;"),
               p("You can interact with the chart: hide or show data series by
                 clicking on the legend, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(7,highchartOutput("clockchange")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p9",
    fluidPage(
      fluidRow(
        column(1),
        column(3,
               h1("2020 Lockdown", style = "text-align:left;"),
               p("Since the Lockdown started on 23rd March, UK electricity demand has seen a fall.",
                 style = "text-align:left;"),
               p("Just comparing the average daily patterns for the week of 16-22 March before the Lockdown,
                 with the week of 23-29  March afterwards shows the effect clearly.",
                 style = "text-align:left;"),
               p("You can interact with the chart: hide or show data series by
                 clicking on the legend, and export data with the menu top right.",
                 style = "text-align:left; color:#636363")),
        column(7,highchartOutput("lockdown")),
        column(1)
      )
    )
  ),
  
  pageSection(
    menu = "p10",
    pageContainer(
      h1("About this tool"),
      p("This app was built by", tags$a(href = "https://www.cultureofinsight.com/","Culture of Insight", target = "_blank"), 
        "using",tags$a(href = "https://shiny.rstudio.com/","R Shiny", target = "_blank"),
        "and the", tags$a(href = "https://github.com/RinteRface/fullPage","fullPage API", target = "_blank"), "from John Coene"),
      p("Data for these analyses come largely from the excellent", 
        tags$a(href = "https://www.gridwatch.templar.co.uk/","Gridwatch", target = "_blank"),
        "which has collated data on UK energy sources and consumption on a quarter-hourly basis since 2011"),
      p("We have also drawn daily 2019 wind data from",
        tags$a(href = "http://nw3weather.co.uk/","NW3 Weather", target = "_blank"),
        ", temperature data from",
        tags$a(href = "https://www.metoffice.gov.uk/hadobs/hadcet/data/download.html","the Met Office", target = "_blank"),
        ", and carbon emissions data from",
        tags$a(href = "https://www.gov.uk/government/statistics/final-uk-greenhouse-gas-emissions-national-statistics-1990-to-2018","the ONS", target = "_blank")),
      p("Please contact", tags$a(href = "mailto:james@cultureofinsight.com","James Smythe"),"with any questions")
    )
  )
  
  )

#===== SERVER =====

server <- function(input, output){
  
  output$clean <- renderHighchart({
    
    hchart(dirty, "column", hcaes(x = year, y = value, group = type), 
           stacking = T, backgroundColor = "#deebf7") %>% 
      hc_xAxis(title = list(text = "Year"), tickInterval = 1) %>% 
      hc_legend(layout= 'horizontal', backgroundColor= 'rgb(0,0,0,0)',
                align= 'left', verticalAlign= 'top',
                floating= TRUE, x= 90, y= 45, reversed = TRUE)%>% 
      hc_title(text = "Energy Production & Emissions") %>% 
      hc_yAxis_multiples(
        list(lineWidth = 0, title = list(text = "UK Electricity Demand / Supply GW")),
        list(opposite = TRUE, min = 0,
             title = list(text = "C02 Emissions from Energy Production (Tonnes M)"))
      ) %>% 
      hc_add_series(data = dirty2, type = "line", hcaes(x = year, y = demand), name = "energy demand",
                    marker = list(enabled = F)) %>%
      hc_add_series(data = dirty3, type = "line", hcaes(x = year, y = emissions), name = "C02 emissions", 
                    marker = list(enabled = F), yAxis = 1) %>%
      # hc_add_theme(hc_theme_smpl()) %>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                                                sourceWidth = 1000, sourceHeight = 600,
                                                csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                                                tableCaption = "UK Electricity Demand / Supply GW",
                                                filename = paste0("CultureofInsight_", Sys.Date()),
                                                chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                                                    title = list(style = list(fontFamily = "Roboto Condensed")),
                                                                    subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                                                    plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                                                )
      )
    
  })
  
  output$heatmap <- renderHighchart({
    
    flow <- flowchart %>% 
      mutate(source = factor(source, levels = flow_order)) %>% 
      arrange(source, mnth) %>% 
      rename(Month = mnth, `Energy Source` = source)
    
    req(flow)
    
    hchart(flow, "heatmap", hcaes(x = `Month`, y = `Energy Source`, value = value),
           backgroundColor = "#deebf7", plotBackgroundColor = "#deebf7") %>% 
      hc_colorAxis(stops = color_stops(25, rev(magma(25)))) %>%
      hc_tooltip(formatter = fntltp) %>%
      hc_title(text = "UK power sources") %>%
      hc_legend(layout = "vertical", verticalAlign = "top",
                align = "right", valueDecimals = 0,
                y= 45) %>%
      # hc_add_theme(hc_theme_smpl()) %>% 
      hc_yAxis(reversed = T) %>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "UK Power Sources",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
  })
  
  output$renewables <- renderHighchart({
    
    hchart(flowchart2, "column", hcaes(x = mnth, y = value, group = source),
           stacking = T, pointPadding = 0, groupPadding = 0) %>% #
      hc_yAxis(labels = list(
        formatter = JS("function(){ return Math.abs(this.value) / 1000000 + 'M'; }")
      ), title = list(text = "GW Contribution")) %>%
      hc_xAxis(title = list(text = "Month/Year")) %>%
      hc_title(text = "Growth of Renewable Energy") %>%
      hc_legend(layout= 'horizontal', backgroundColor= 'rgb(0,0,0,0)',
                align= 'left', verticalAlign= 'top',
                floating= TRUE, x= 90, y= 45) %>%
      hc_colors(c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d","#666666")) %>%
      # hc_add_theme(hc_theme_smpl())%>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "Growth of Renewable Energy",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
  output$sunburst <- renderHighchart({
    
    source <- rlang::sym(input$source)
    
    if(input$source == "solar"){
      d <- clean %>% 
        rename(high = solar,
               color = Meantemp)

      label <- "Temperature"
    } else {
      d <- clean %>% 
        rename(high = wind,
               color = AveSpeed)
      label <- "Wind Speed"
    }
    
    x <- c(input$source)
    y <- sprintf("{point.%s}", "high")
    tltip <- tooltip_table(x, y)
    
    hchart(d, type = "columnrange",
           hcaes(x = date, low = 0, high = high, color = color)) %>%
      hc_chart(polar = TRUE) %>%
      hc_legend(enabled = F) %>%
      hc_xAxis(
        title = list(text = ""), gridLineWidth = 0.5,
        labels = list(format = "{value: %b}")) %>%
      hc_tooltip(useHTML = TRUE, pointFormat = tltip,
                 headerFormat = as.character(tags$small("{point.x:%d %B, %Y}"))) %>%
      hc_yAxis(title = list(text = paste("GW from", input$source)),
               showFirstLabel = F) %>%
      # hc_add_theme(hc_theme_smpl()) %>% 
      hc_size(height = 700)%>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "Renewable Sources by Season",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
  output$days2019 <- renderHighchart({
    
    hchart(dayrange, "column", hcaes(x = date, y = demand, color = demand)) %>% 
      hc_xAxis(title = list(text = "Date"), tickInterval = 1) %>%
      hc_title(text = "Energy Demand by Day, 2019") %>%
      hc_yAxis(title = list(text = "UK Electricity Demand GW")) %>%
      # hc_add_theme(hc_theme_smpl())%>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "Energy Demand by Day, 2019",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
  output$days <- renderHighchart({
    
    d <- daytypes %>% 
      filter(weekpart %in% c("Weekdays", "Weekends")) %>% 
      mutate(demand = round(demand))
    
    d2 <- daytypes %>% 
      filter(weekpart == input$topdays) %>% 
      mutate(demand = round(demand))
    
    hchart(d, "line", hcaes(x = hour, y = demand, group = weekpart)) %>%
      hc_xAxis(title = list(text = "Hour of Day"), tickInterval = 1) %>%
      hc_yAxis(title = list(text = "UK Electricity Demand GW")) %>%
      hc_legend(layout= 'vertical', backgroundColor= 'rgb(0,0,0,0)',
                align= 'left', verticalAlign= 'top',
                floating= TRUE, x= 90, y= 45, reversed = TRUE)%>%
      hc_title(text = paste("2019 Weekday & Weekend Demand vs", input$topdays)) %>%
      # hc_add_theme(hc_theme_smpl())%>% 
      hc_colors(c("#636363", "#636363")) %>% 
      hc_plotOptions(line = list(marker = list(enabled = F))) %>% 
      hc_add_series(data = d2, type = "line", hcaes(x = hour, y = demand), 
                    name = input$topdays, color = "#e34a33", marker = list(enabled = F)) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "UK Electricity Demand",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
  output$clockchange <- renderHighchart({
    
    hchart(clock_change, "line", hcaes(x = hour, y = demand, group = week))%>%
      hc_xAxis(title = list(text = "Hour of Day - GMT"), tickInterval = 1) %>%
      hc_yAxis(title = list(text = "UK Electricity Demand GW")) %>%
      hc_legend(layout= 'vertical', backgroundColor= 'rgb(0,0,0,0)',
                align= 'left', verticalAlign= 'top',
                floating= TRUE, x= 90, y= 45) %>%
      hc_title(text = "When The Clocks Went Back")%>%
      hc_plotOptions(line = list(marker = list(enabled = F))) %>%
      hc_colors(c("#3182bd", "#e34a33")) %>% 
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "UK Electricity Demand",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
  output$lockdown <- renderHighchart({
    
    ldo <- c("Week Pre Lockdown", "Lockdown Week 1")
    
    ld <- lockdown %>% 
      mutate(week = factor(week, levels = ldo), 
             demand = round(demand)) %>% 
      arrange(week, hour)
    
    hchart(ld, "areaspline", hcaes(x = hour, y = demand, group = week)) %>% 
        hc_xAxis(title = list(text = "Hour of Day"), tickInterval = 1) %>%
        hc_yAxis(title = list(text = "UK Electricity Demand GW")) %>%
        hc_legend(layout= 'vertical', backgroundColor= 'rgb(0,0,0,0)',
                  align= 'left', verticalAlign= 'top',
                  floating= TRUE, x= 90, y= 45, reversed = TRUE)%>%
        hc_title(text = "2020 Lockdown") %>%
      hc_plotOptions(areaspline = list(marker = list(enabled = F))) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = download_btns, text = "Download")),
                   sourceWidth = 1000, sourceHeight = 600,
                   csv = list(dateFormat = "%d/%m/%y"), fallbackToExportServer = FALSE,
                   tableCaption = "UK Electricity Demand",
                   filename = paste0("CultureofInsight_", Sys.Date()),
                   chartOptions = list(chart = list(style = list(fontFamily = "Roboto")),
                                       title = list(style = list(fontFamily = "Roboto Condensed")),
                                       subtitle = list(text = "", style = list(fontFamily = "Roboto")),
                                       plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}")))
                   )
      )
    
  })
  
}

shinyApp(ui, server)




library(shiny)
library(shinydashboard)
#library(leaflet)
#library(rgdal)
#library(raster)
library(dplyr)
library(RColorBrewer)
#library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)
library("tidyverse")
library(ggfittext)
library(highcharter)
library(shinyWidgets)
library(treemapify)
library(ggplot2)
library(tibble)
library(shinyWidgets)
library(scales)
library(DBI)
library(RMySQL)
library(purrr)
#wd
#setwd("C:/Users/John/Desktop/jumia stuff/Phones_Dashboard")
#read data
jumia=read.csv("Jumia Phones Cleanest Data v3.csv",
               stringsAsFactors = T)
jumia[jumia == ""] <- NA

jumia=jumia%>%
    filter(!is.na(ram_space2),
           !is.na(rom_space))
#jumia_rom=jumia%>%
#    filter(!is.na(rom_space))

#options
PARS <- list(
    debug = FALSE,
    classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
    sparkline_color = "#333333",
    font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

options(
    highcharter.google_fonts = FALSE,
    highcharter.debug = PARS$debug,
    # shiny.launch.browser = PARS$debug,
    highcharter.theme = 
        hc_theme_smpl(
            title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
            subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
            chart = list(
                backgroundColor = "transparent",
                style = list(fontFamily = PARS$font, fontSize = "1.0em")
            ),
            plotOptions = list(
                series = list(
                    dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
                    animation = list(duration = 3000)
                )
            ),
            legend = list(
                itemStyle =  list(
                    fontWeight = "normal"
                )
            )
        )
)

dropdownButtonp <- purrr::partial(
    dropdownButton,
    status = "customstatus",
    size = "sm",
    right = TRUE,
    status = "info",
    width = "400px",
    inline = TRUE,
)

#theme for highchart
hc_theme_sparkline_vb <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            showInLegend=FALSE,
            style = list(overflow = "visible")
        ),
        xAxis = list(
            visible = T,
            title=list(
                text=NULL
            ),
            showInLegend=FALSE,
            endOnTick = FALSE, 
            startOnTick = FALSE,
            legend=list(
                enabled=FALSE
            )
        ),
        yAxis = list(
            visible = F,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = FALSE,
            shadow = FALSE,
            borderColor = "transparent",
            borderWidth = 0,
            backgroundColor = "transparent",
            style = list(textOutline = "5px white")
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 2,
                shadow = FALSE,
                fillOpacity = 0.0,
                color = "#FFFFFFBF",
                showInLegend=FALSE,
                fillColor = list(
                    linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                    stops = list(
                        list(0.00, "#FFFFFF00"),
                        list(0.50, "#FFFFFF7F"),
                        list(1.00, "#FFFFFFFF")
                    )
                ),
                dataLabels=list(
                    align='left',
                    enabled=T,
                    format='Ksh {y}',
                    pointFormat='Ksh {point.y:,.0f}',
                    lang=list(
                        thousandsSep=','
                    )   
                    #rotation=270,
                    #x=2,
                    #y=-1
                )
            )
        ),
        credits = list(
            enabled = FALSE,
            text = ""
        )
    )
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}

#theme for highchart version 2
hc_theme_sparkline_vb2 <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            style = list(overflow = "visible")
        ),
        xAxis = list(
            visible = T, 
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        yAxis = list(
            visible = F,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = FALSE,
            shadow = FALSE,
            borderColor = "transparent",
            borderWidth = 0,
            backgroundColor = "transparent",
            style = list(textOutline = "5px white")
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 2,
                shadow = FALSE,
                fillOpacity = 0.0,
                color = "#FFFFFFBF",
                showInLegend=FALSE,
                fillColor = list(
                    linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
                    stops = list(
                        list(0.00, "#FFFFFF00"),
                        list(0.50, "#FFFFFF7F"),
                        list(1.00, "#FFFFFFFF")
                    )
                ),
                dataLabels=list(
                    align='left',
                    enabled=T,
                    format='Ksh {y}',
                    pointFormat='Ksh {point.y:,.0f}'
                    #rotation=270,
                    #x=2,
                    #y=300
                )
            )
        ),
        credits = list(
            enabled = FALSE,
            text = ""
        )
    )
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}


valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "150px",minititle = NULL) {
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon)) 
        shinydashboard:::tagAssert(icon, type = "i")
    
    boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
            class = "inner",
            if(!is.null(minititle)) tags$small(minititle),
            h3(value),
            # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
            tags$span(hc_size(spark, height = height_spark)),
            if (!is.null(subtitle)) p(subtitle)
        ),
        if (!is.null(icon)) div(class = "icon-large", icon)
    )
    
    if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
    
    div(class = if (!is.null(width)) 
        paste0("col-sm-", width), boxContent)
}

hc_theme_sparkline2 <- function(...) {
    
    theme <- list(
        chart = list(
            backgroundColor = NULL,
            margins = c(0, 0, 0, 0),
            spacingTop = 0,
            spacingRight = 0,
            spacingBottom = 0,
            spacingLeft = 0,
            plotBorderWidth = 0,
            borderWidth = 0,
            style = list(
                overflow = "visible"
            ),
            skipClone = TRUE
        ),
        xAxis = list(
            visible = FALSE, 
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        yAxis = list(
            visible = FALSE,
            endOnTick = FALSE, 
            startOnTick = FALSE
        ),
        tooltip = list(
            outside = TRUE,
            headerFormat = "",
            pointFormat = "{point.x}: <b>{point.y}</b>"
        ),
        plotOptions = list(
            series = list(
                marker = list(enabled = FALSE),
                lineWidth = 1,
                shadow = FALSE,
                fillOpacity = 0.25
            )
        )
    )
    
    theme <- structure(theme, class = "hc_theme")
    
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
            theme,
            hc_theme(...)
        )
    }
    
    theme
}

source("R-scripts/99-shiny-helpers.R")
#user interface
ui <- dashboardPage(
    skin="yellow",
    dashboardHeader(
        # Set height of dashboardHeader

        title = "JUMIA PHONES DASHBOARD", titleWidth = 500,
        #subtitle = "GOVERNMENT OF KENYA", subtitleWidth = 500,
        tags$li(a(#href = 'https://www.jumia.co.ke/smartphones/',
                  tags$img(src = 'JMIA_BIG.png',
                      title = "Jumia", height = "30px",
                      ),
                  style = "padding-top:10px; padding-bottom:10px;"),
                onclick = "window.open('https://www.jumia.co.ke/smartphones/')",
                class = "dropdown"),
     
        tags$li(actionLink("LinkedIn", 
                           label = "", 
                           icon = icon("twitter"),
                           onclick = "window.open('https://twitter.com/dankilemi')"),
                class = "dropdown"),
        tags$li(actionLink("Facebook", 
                           label = "", 
                           icon = icon("linkedin"),
                           onclick = "window.open('https://www.linkedin.com/in/kilemi-dan-a7b259201/')"),
                class = "dropdown")
        
        
    ), #disable = TRUE
    dashboardSidebar(
        width  = 230,
        #disable = TRUE
            selectInput(
                    inputId = "brands",
                    label = "BRAND",
                    choices = c(
                        "ALL"="ALL",
                        "APPLE"="apple",
                        "XIAOMI"="xiaomi",
                        "OPPO"="oppo",
                        "ITEL"="itel",
                        "REALME"="realme",
                        "HUAWEI"="huawei",
                        "TECNO"="tecno",
                        "SAMSUNG"="samsung",
                        "VIVO"="vivo",
                        "INFINIX"="infinix"
                    ),
                    selected = "ALL",
                    width = "90%"
                    
                ),

                selectInput(inputId = "ram",
                             label = "RAM",
                             choices = "",
                             selected = "ALL",
                            width = "90%"
                              
            ),
        selectInput(inputId = "rom",
                    label = "STORAGE",
                    choices = "",
                    selected = "ALL",
                    width = "90%"
                    
        )


            
        
    ),

    dashboardBody(  tags$head( 
        tags$style(HTML(".main-sidebar { font-size: 22px; }")) #change the font size to 20
    ),
                         fluidRow(
                             #class = "top-buffer",
                             column(
                                 # offset = 2,
                                 width = 12,
                                 #class = PARS$classcol,
                                 #plotlyOutput("top2"),
                                 tags$head(tags$style(HTML(".small-box {height: 240px}"))),
                                 valueBoxOutput("hc5",4),
                                 valueBoxOutput("hc6",4),
                                 box(width = 4,
                                     title = "Most Available Brands", status = "warning", solidHeader = TRUE,
                                     collapsible = TRUE,background = "olive",
                                     plotOutput("hc1",height = 200)
                                 )
                                 
                                 #imageOutput("img1", 3)
                             )
                        
                             
                         ),
                         fluidRow(
                             
                             
                             box(width = 4,
                                 title = "Average Price by Phone Brand", status = "success", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 highchartOutput("hc2",height = 250)
                             ),
                             
                             box(width=4,
                                 title = "Average Phone Price by RAM", status = "success", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 highchartOutput("hc3",height = 250)
                             ),
                             box(width = 4,
                                 title = "Average Phone Price by Storage", status = "success", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 highchartOutput("hc4",height = 250)
                             )
                    
                             
                         )
                         
                )


)




#server
server <- function(input, output,session) {
    
    
    observeEvent(
        input$brands,
        if (!"ALL" %in% input$brands){
            
            updateSelectInput(session,"ram","RAM",
                              choices = jumia$ram_space2[jumia$brand==input$brands])
        }
        
        else if ("ALL" %in% input$brands){
            updateSelectInput(session,"ram","RAM",
                              choices = c("ALL","12GB","8GB","6GB","4GB","3GB","2GB","1GB"))
        }
        else {
            updateSelectInput(session,"ram","RAM",
                              choices = "ALL") 
        }
    )
    
    observeEvent(
        input$ram,
        if(!"ALL" %in% input$brands){
            updateSelectInput(session,"rom","STORAGE",
                              choices = jumia$rom_space[jumia$ram_space2==input$ram & jumia$brand==input$brands]) 
        }
        
        else if("ALL" %in% input$brands){
            updateSelectInput(session,"rom","STORAGE",
                              choices = c("ALL","16GB","32GB","64GB","128GB","256GB"))
        }else if (!"ALL" %in% input$ram & "ALL" %in% input$brands){
            updateSelectInput(session,"rom","STORAGE",
                              choices = jumia$rom_space[jumia$ram_space2==input$ram & jumia$brand==input$brands])
        }
        else {
            updateSelectInput(session,"rom","STORAGE",
                              choices = "ALL")
        }
        
    )
    
    
    output$brand<-renderValueBox({
        valueBox(
            format(sum(jumia$Price),big.mark=",",scientific=FALSE), "Price", icon = icon("info"),
            color = "purple"
            #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, 
            #orange, fuchsia, purple, maroon, black.
        )
    })
            output$hc1<-renderPlot ({
                
                if("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom ){
                    jumia %>%
                        #filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                }
                
                else if ("ALL" %in% input$brands & "ALL" %in% input$rom){
                    jumia %>%
                        filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                }
                
                else if ("ALL" %in% input$ram & "ALL" %in% input$rom){
                    jumia %>%
                        filter(brand==input$brands)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }
                else if ("ALL" %in% input$brands & "ALL" %in% input$ram){
                    jumia %>%
                        filter(rom_space==input$rom)%>%
                        #filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }
                else if ("ALL" %in% input$brands){
                    jumia %>%
                        filter(rom_space==input$rom,
                               ram_space2 == input$ram)%>%
                        #filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }
                
                else if ("ALL" %in% input$ram){
                    jumia %>%
                        filter(rom_space==input$rom,
                               brand == input$brands)%>%
                        #filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }
    
                else if ("ALL" %in% input$rom){
                    jumia %>%
                        filter(brand==input$brands,
                               ram_space2 == input$ram)%>%
                        #filter(ram_space2==input$ram)%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }

                else {
                    
                        jumia %>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        count(brand)%>%
                        top_n(10)%>%
                        ggplot(aes(area = n, fill = brand,
                                   label = paste(toupper(brand), n, sep = "\n"))) +
                        geom_treemap() +
                        geom_treemap_text(colour = "white",
                                          place = "centre",
                                          size = 15) +
                        theme(legend.position = "none")
                    
                }



            })
            
            output$hc2<-renderHighchart ({
                
                if ("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom){
                    avg_price_brand=jumia%>%
                        #filter(rom_space==input$rom)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$brands & "ALL" %in% input$ram){
                    avg_price_brand=jumia%>%
                        filter(rom_space==input$rom)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$brands & "ALL" %in% input$rom){
                    avg_price_brand=jumia%>%
                        filter(ram_space2==input$ram)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$rom & "ALL" %in% input$ram){
                    avg_price_brand=jumia%>%
                        filter(brand==input$brands)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$brands){
                    avg_price_brand=jumia %>%
                        filter(ram_space2==input$ram,
                               rom_space==input$rom)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$ram){
                    avg_price_brand=jumia %>%
                        filter(brand==input$brands,
                               rom_space==input$rom)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    
                    hc 
                    
                }
                
                else if ("ALL" %in% input$rom){
                    avg_price_brand=jumia %>%
                        filter(brand==input$brands,
                               ram_space2==input$ram)%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
                    hc <- avg_price_brand %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_brand$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    
                    hc 
                    
                }
                
     
                else {
                    filtered <-
                        jumia %>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        group_by(brand)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    
                    filtered$avg_price<-round(filtered$avg_price,0)
                    hc <- filtered %>% 
                        hchart(
                            'bar', hcaes(x = brand, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(filtered$brand))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                
            })
            
            output$hc3<-renderHighchart ({
                
                if("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom){
                    avg_price_ram=jumia%>%
                        filter(!is.na(ram_space2))%>%
                        #filter(rom_space==input$rom)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$ram){
                    avg_price_ram=jumia%>%
                        filter(!is.na(ram_space2))%>%
                        filter(!is.na(rom_space))%>%
                        filter(rom_space==input$rom)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$rom){
                    avg_price_ram=jumia%>%
                        filter(!is.na(ram_space2))%>%
                        filter(ram_space2==input$ram)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if("ALL" %in% input$ram & "ALL" %in% input$rom){
                    avg_price_ram=jumia%>%
                        filter(!is.na(ram_space2))%>%
                        filter(brand==input$brands)%>%
                    group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$brands){
                    avg_price_ram=jumia %>%
                        filter(!is.na(ram_space2))%>%
                        filter(ram_space2==input$ram,
                               rom_space==input$rom)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                }
                
                else if ("ALL" %in% input$ram){
                    avg_price_ram=jumia %>%
                        filter(!is.na(ram_space2))%>%
                        filter(!is.na(rom_space))%>%
                        filter(brand==input$brands,
                               rom_space==input$rom)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                    
                }
                
                else if ("ALL" %in% input$rom){
                    avg_price_ram=jumia %>%
                        filter(!is.na(ram_space2))%>%
                        filter(brand==input$brands,
                               ram_space2==input$ram)%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_ram$avg_price=round(avg_price_ram$avg_price,0)
                    hc <- avg_price_ram %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_ram$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc 
                    
                }
                
                

                else {
                    filtered <-jumia %>%
                        filter(!is.na(ram_space2))%>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        group_by(ram_space2)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    filtered$avg_price=round(filtered$avg_price,0)
                    hc <- filtered %>% 
                        hchart(
                            'bar', hcaes(x = ram_space2, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(filtered$ram_space2))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                ###
                
            })
            
            output$hc4<-renderHighchart ({
                
                if("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$ram){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(rom_space==input$rom)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                }
                else if("ALL" %in% input$brands & "ALL" %in% input$rom){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(ram_space2==input$ram)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                }
                
                else if("ALL" %in% input$ram & "ALL" %in% input$rom){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(brand==input$brands)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                }
                
                else if ("ALL" %in% input$brands){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(ram_space2==input$ram,
                               rom_space==input$rom)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                }
                
                else if ("ALL" %in% input$ram){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(brand==input$brands,
                               rom_space==input$rom)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                
                else if ("ALL" %in% input$rom){
                    avg_price_rom=jumia%>%
                        filter(!is.na(rom_space))%>%
                        filter(brand==input$brands,
                               ram_space2==input$ram)%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    avg_price_rom$avg_price=round(avg_price_rom$avg_price,0)
                    hc <- avg_price_rom %>% 
                        hchart(
                            'bar', hcaes(x =rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(avg_price_rom$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                
                
                
                else {
                    filtered <-
                        jumia %>%
                        filter(!is.na(rom_space))%>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        group_by(rom_space)%>%
                        summarise(avg_price=mean(Price))%>%
                        arrange(desc(avg_price))
                    filtered$avg_price=round(filtered$avg_price,0)
                    hc <- filtered %>% 
                        hchart(
                            'bar', hcaes(x = rom_space, y = avg_price, color = avg_price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(filtered$rom_space))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    hc
                    
                }
                
            })
            
            output$hc5<-renderValueBox ({
                
                if("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom){
                    top_priced1=jumia %>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced1 %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "red",
                        icon =NULL, #tags$i(class="fa-sharp fa-solid fa-mobile",style="font-size: 40px"),
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                }
                else if("ALL" %in% input$brands & "ALL" %in% input$ram){
                    top_priced1=jumia %>%
                        filter(rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced1 %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "red",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                    
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$rom){
                    top_priced1=jumia %>%
                        filter(ram_space2==input$ram)%>%
                    dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced1 %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "red",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                    
                }
                
                else if("ALL" %in% input$ram & "ALL" %in% input$rom){
                    top_priced1=jumia %>%
                        filter(brand==input$brands)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced1 %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "red",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                    
                }
                
                else if ("ALL" %in% input$brands){
                    top_priced1=jumia %>%
                        filter(ram_space2==input$ram,
                               rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced1 %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "lime",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                }
                
                else if ("ALL" %in% input$ram){
                    top_priced=jumia %>%
                        filter(brand==input$brands,
                               rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "teal",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                    
                }
                
                else if ("ALL" %in% input$rom){
                    top_priced=jumia %>%
                        filter(brand==input$brands,
                               ram_space2==input$ram)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- top_priced %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- top_priced %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(top_priced$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle =NULL,
                        color = "teal",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                    
                }
                
                
                
                else {
                    filtered <-
                        jumia %>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(7)
                    
                    lbl <- filtered %>% 
                        pull(Price) %>% first() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- filtered %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(filtered$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb2())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "blue",
                        spark = hc,
                        minititle = "Top 7 Most Expensive Jumia Phones"
                    )
                }
            })
                
            output$hc6<-renderValueBox ({
                
                if("ALL" %in% input$brands & "ALL" %in% input$ram & "ALL" %in% input$rom){
                    bottom_priced1=jumia %>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced1 %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$ram){
                    bottom_priced1=jumia %>%
                        filter(rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced1 %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                
                else if("ALL" %in% input$brands & "ALL" %in% input$rom){
                    bottom_priced1=jumia %>%
                        filter(ram_space2==input$ram)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced1 %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                
                else if("ALL" %in% input$ram & "ALL" %in% input$rom){
                    bottom_priced1=jumia %>%
                        filter(brand==input$brands)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced1 %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                
                else if ("ALL" %in% input$brands){
                    bottom_priced1=jumia %>%
                        filter(ram_space2==input$ram,
                               rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced1 %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced1 %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced1$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "lime",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                }
                
                else if ("ALL" %in% input$ram){
                    bottom_priced=jumia %>%
                        filter(brand==input$brands,
                               rom_space==input$rom)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                else if ("ALL" %in% input$rom){
                    bottom_priced=jumia %>%
                        filter(brand==input$brands,
                               ram_space2==input$ram)%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- bottom_priced %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- bottom_priced %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(bottom_priced$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle = NULL,
                        color = "yellow",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                    
                }
                
                
                
                else {
                    filtered <-
                        jumia %>%
                        filter(ram_space2 == input$ram,
                               brand == input$brands,
                               rom_space==input$rom
                               
                        )%>%
                        dplyr::select(Name,Price) %>%
                        arrange(desc(Price)) %>%
                        top_n(-7)
                    
                    lbl <- filtered %>% 
                        pull(Price) %>% last() %>% comma() %>% paste0("Ksh ", .)
                    
                    hc <- filtered %>% 
                        hchart(
                            'bar', hcaes(x =Name, y = Price, color = Price)
                        )%>%
                        hc_xAxis(title=list(text=NULL),
                                 categories = as.list(toupper(filtered$Name))) %>%
                        hc_tooltip(valuePrefix = "Ksh ")%>%
                        hc_add_theme(hc_theme_sparkline_vb())
                    #hc
                    
                    valueBoxSpark(
                        value = lbl,
                        subtitle =NULL,
                        color = "red",
                        spark = hc,
                        minititle = "Bottom 7 Least Expensive Jumia Phones"
                    )
                }
                
                
            })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)

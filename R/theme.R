# Highcharter themes and the sparkline value box.
#
# Carried over unchanged from the top of the old app.R, so the dashboard keeps
# its exact look. Two things that lived alongside them are gone:
#
#   hc_theme_sparkline2()  defined, never called
#   dropdownButtonp()      defined, never called - and it passed `status` twice
#                          (once as "customstatus", once as "info"), so any
#                          call would have failed with "formal argument
#                          'status' matched by multiple actual arguments".

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

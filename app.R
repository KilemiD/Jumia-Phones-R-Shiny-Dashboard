# Jumia Phones Dashboard — prices and specs of smartphones listed on Jumia Kenya.
#
# Live at https://danielkilemi.shinyapps.io/Jumia_Phones_Dashboard/
#
# This file loads packages, sources R/, and assembles the page. The work is in
# R/ — see README.md.

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(treemapify)
library(highcharter)
library(scales)
library(htmltools)

# Sourced explicitly and in order; _disable_autoload.R stops Shiny loading R/
# a second time by itself.
source("R/theme.R")
source("R/data.R")
source("R/charts.R")
source("R/mod_explorer.R")

PARS <- list(
  debug = FALSE,
  font = paste0(
    '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, ',
    'Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", ',
    '"Segoe UI Symbol"'
  )
)

options(
  highcharter.google_fonts = FALSE,
  highcharter.debug = PARS$debug,
  highcharter.theme = hc_theme_smpl(
    title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
    subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = PARS$font, fontSize = "1.0em")
    ),
    plotOptions = list(
      series = list(
        dataLabels = list(
          color = "#222d32",
          style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)
        ),
        animation = list(duration = 3000)
      )
    ),
    legend = list(itemStyle = list(fontWeight = "normal"))
  )
)

# Read once per process rather than once per session.
phones <- load_phones()

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "JUMIA PHONES DASHBOARD",
    titleWidth = 500,
    tags$li(
      a(
        tags$img(src = "JMIA_BIG.png", title = "Jumia", height = "30px"),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      onclick = "window.open('https://www.jumia.co.ke/smartphones/')",
      class = "dropdown"
    ),
    tags$li(
      actionLink(
        "twitter", label = "", icon = icon("twitter"),
        onclick = "window.open('https://twitter.com/dankilemi')"
      ),
      class = "dropdown"
    ),
    tags$li(
      actionLink(
        "linkedin", label = "", icon = icon("linkedin"),
        onclick = "window.open('https://www.linkedin.com/in/kilemi-dan-a7b259201/')"
      ),
      class = "dropdown"
    )
  ),
  explorer_sidebar_ui("explorer", phones),
  explorer_body_ui("explorer")
)

server <- function(input, output, session) {
  explorer_server("explorer", phones)
}

shinyApp(ui = ui, server = server)

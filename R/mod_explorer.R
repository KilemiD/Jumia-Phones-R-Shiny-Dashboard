# The dashboard: three filters in the sidebar, five outputs in the body.
#
# Split into a sidebar UI and a body UI because dashboardPage() wants them in
# different arguments, but both belong to the same module and share one
# namespace.

explorer_sidebar_ui <- function(id, phones) {
  ns <- shiny::NS(id)

  shinydashboard::dashboardSidebar(
    width = 230,
    shiny::selectInput(
      inputId = ns("brand"),
      label = "BRAND",
      choices = brand_choices(phones),
      selected = ALL,
      width = "90%"
    ),
    shiny::selectInput(
      inputId = ns("ram"),
      label = "RAM",
      choices = filter_choices(phones$ram_space2),
      selected = ALL,
      width = "90%"
    ),
    shiny::selectInput(
      inputId = ns("rom"),
      label = "STORAGE",
      choices = filter_choices(phones$rom_space),
      selected = ALL,
      width = "90%"
    )
  )
}

explorer_body_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".main-sidebar { font-size: 22px; }")),
      shiny::tags$style(shiny::HTML(".small-box {height: 240px}"))
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shinydashboard::valueBoxOutput(ns("most_expensive"), width = 4),
        shinydashboard::valueBoxOutput(ns("least_expensive"), width = 4),
        shinydashboard::box(
          width = 4,
          title = "Most Available Brands", status = "warning",
          solidHeader = TRUE, collapsible = TRUE, background = "olive",
          shiny::plotOutput(ns("brand_mix"), height = 200)
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Average Price by Phone Brand", status = "success",
        solidHeader = TRUE, collapsible = TRUE,
        highcharter::highchartOutput(ns("price_by_brand"), height = 250)
      ),
      shinydashboard::box(
        width = 4, title = "Average Phone Price by RAM", status = "success",
        solidHeader = TRUE, collapsible = TRUE,
        highcharter::highchartOutput(ns("price_by_ram"), height = 250)
      ),
      shinydashboard::box(
        width = 4, title = "Average Phone Price by Storage", status = "success",
        solidHeader = TRUE, collapsible = TRUE,
        highcharter::highchartOutput(ns("price_by_rom"), height = 250)
      )
    )
  )
}

explorer_server <- function(id, phones) {
  shiny::moduleServer(id, function(input, output, session) {

    # One filtered dataset, read by all five outputs. Previously each output
    # re-derived it through its own eight-branch if/else, so a change to the
    # filtering had to be made in forty-eight places to be made at all.
    filtered <- shiny::reactive({
      shiny::req(input$brand, input$ram, input$rom)
      filter_phones(phones, input$brand, input$ram, input$rom)
    })

    # Narrow the RAM and STORAGE dropdowns to what the current brand actually
    # offers. The old version replaced the choices with a raw, unsorted,
    # duplicated column slice and dropped the "ALL" option entirely, so once
    # you picked a brand you could not get back to all sizes.
    shiny::observeEvent(input$brand, {
      available <- filter_phones(phones, brand_sel = input$brand)

      shiny::updateSelectInput(
        session, "ram",
        choices = filter_choices(available$ram_space2),
        selected = if (input$ram %in% filter_choices(available$ram_space2)) input$ram else ALL
      )
      shiny::updateSelectInput(
        session, "rom",
        choices = filter_choices(available$rom_space),
        selected = if (input$rom %in% filter_choices(available$rom_space)) input$rom else ALL
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ram, {
      available <- filter_phones(phones, brand_sel = input$brand, ram_sel = input$ram)
      choices <- filter_choices(available$rom_space)

      shiny::updateSelectInput(
        session, "rom",
        choices = choices,
        selected = if (input$rom %in% choices) input$rom else ALL
      )
    }, ignoreInit = TRUE)

    # Every output guards against an empty filter combination. Without this,
    # an empty data frame reaches ggplot and highcharter as a cryptic error in
    # the middle of the dashboard.
    has_rows <- function(data) {
      shiny::validate(shiny::need(
        nrow(data) > 0,
        "No phones match this combination of brand, RAM and storage."
      ))
      data
    }

    output$brand_mix <- shiny::renderPlot({
      brand_treemap(has_rows(filtered()))
    })

    output$price_by_brand <- highcharter::renderHighchart({
      avg_price_chart(has_rows(filtered()), "brand")
    })

    output$price_by_ram <- highcharter::renderHighchart({
      avg_price_chart(has_rows(filtered()), "ram_space2")
    })

    output$price_by_rom <- highcharter::renderHighchart({
      avg_price_chart(has_rows(filtered()), "rom_space")
    })

    # valueBoxSpark() calls hc_size() on its spark, which cannot take NULL, so
    # an empty selection falls back to a plain box rather than erroring.
    spark_box <- function(extremes, color, minititle) {
      if (is.null(extremes$chart)) {
        return(shinydashboard::valueBox(
          value = extremes$label, subtitle = minititle, color = color
        ))
      }
      valueBoxSpark(
        value = extremes$label,
        subtitle = NULL,
        color = color,
        icon = NULL,
        spark = extremes$chart,
        minititle = minititle
      )
    }

    output$most_expensive <- shinydashboard::renderValueBox({
      spark_box(
        price_extremes(filtered(), "top"),
        "red", "Top 7 Most Expensive Jumia Phones"
      )
    })

    output$least_expensive <- shinydashboard::renderValueBox({
      spark_box(
        price_extremes(filtered(), "bottom"),
        "yellow", "Bottom 7 Least Expensive Jumia Phones"
      )
    })
  })
}

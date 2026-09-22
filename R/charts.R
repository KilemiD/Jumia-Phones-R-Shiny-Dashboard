# The five charts.
#
# Each of these was previously written out eight times - once per combination
# of the three filters being "ALL" or not - inside its own render function.
# The chart never varied; only the data going into it did. With filtering
# pulled out into filter_phones(), each chart is written once.

#' Treemap of how many listings each brand has.
brand_treemap <- function(phones, n = 10) {
  counts <- phones |>
    dplyr::count(.data$brand) |>
    dplyr::slice_max(.data$n, n = n, with_ties = FALSE)

  ggplot2::ggplot(
    counts,
    ggplot2::aes(
      area = .data$n,
      fill = .data$brand,
      label = paste(toupper(.data$brand), .data$n, sep = "\n")
    )
  ) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white", place = "centre", size = 15) +
    ggplot2::theme(legend.position = "none")
}

#' Average price by one grouping column, as a highcharter bar chart.
#'
#' `column` is "brand", "ram_space2" or "rom_space" - the three versions that
#' used to be three separate render functions with identical bodies.
avg_price_chart <- function(phones, column) {
  averages <- phones |>
    dplyr::group_by(dplyr::across(dplyr::all_of(column))) |>
    dplyr::summarise(avg_price = round(mean(.data$Price, na.rm = TRUE)), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$avg_price)) |>
    dplyr::mutate(group = toupper(as.character(.data[[column]])))

  averages |>
    highcharter::hchart(
      "bar",
      highcharter::hcaes(x = .data$group, y = .data$avg_price, color = .data$avg_price)
    ) |>
    highcharter::hc_xAxis(
      title = list(text = NULL),
      categories = as.list(averages$group)
    ) |>
    highcharter::hc_tooltip(valuePrefix = "Ksh ") |>
    highcharter::hc_add_theme(hc_theme_sparkline_vb())
}

#' The n most- or least-expensive phones, as a sparkline for a value box.
#'
#' @return a list of the headline label and the chart, since the value box
#'   needs both.
price_extremes <- function(phones, direction = c("top", "bottom"), n = 7) {
  direction <- match.arg(direction)

  picked <- phones |>
    dplyr::select(dplyr::all_of(c("Name", "Price"))) |>
    dplyr::filter(!is.na(.data$Price))

  picked <- if (direction == "top") {
    dplyr::slice_max(picked, .data$Price, n = n, with_ties = FALSE)
  } else {
    dplyr::slice_min(picked, .data$Price, n = n, with_ties = FALSE)
  }

  # An empty filter combination used to reach first() on nothing and render a
  # box reading "Ksh NA".
  if (nrow(picked) == 0) {
    return(list(label = "No phones match", chart = NULL))
  }

  headline <- paste0("Ksh ", scales::comma(dplyr::first(picked$Price)))

  chart <- picked |>
    highcharter::hchart(
      "bar",
      highcharter::hcaes(x = .data$Name, y = .data$Price, color = .data$Price)
    ) |>
    highcharter::hc_xAxis(
      title = list(text = NULL),
      categories = as.list(toupper(picked$Name))
    ) |>
    highcharter::hc_tooltip(valuePrefix = "Ksh ") |>
    highcharter::hc_add_theme(hc_theme_sparkline_vb2())

  list(label = headline, chart = chart)
}

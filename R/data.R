# Loading the scraped Jumia listings, once at startup.

DATA_FILE <- "Jumia Phones Cleanest Data v3.csv"

# The value every filter uses to mean "don't filter on this".
ALL <- "ALL"

#' Read and tidy the phone listings.
#'
#' Rows with no RAM or storage are dropped, as before: every chart in the
#' dashboard groups by one of those two, so a missing value can only show up
#' as an unlabelled bar.
load_phones <- function(path = DATA_FILE) {
  if (!file.exists(path)) {
    stop("Phone data not found at '", path, "'.", call. = FALSE)
  }

  phones <- readr::read_csv(path, show_col_types = FALSE)

  required <- c("Name", "Price", "brand", "ram_space2", "rom_space")
  missing <- setdiff(required, names(phones))
  if (length(missing)) {
    stop("'", path, "' is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  phones |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(x) dplyr::na_if(x, ""))) |>
    dplyr::filter(
      !is.na(.data$ram_space2),
      !is.na(.data$rom_space),
      !is.na(.data$Price)
    ) |>
    dplyr::mutate(
      # Order the memory sizes numerically rather than alphabetically, so
      # "8GB" does not sort between "6GB" and "1GB" on the axes.
      ram_space2 = factor(.data$ram_space2, levels = size_order(.data$ram_space2)),
      rom_space  = factor(.data$rom_space,  levels = size_order(.data$rom_space))
    )
}

#' Sort size labels like "128GB" by their number, not as text.
size_order <- function(x) {
  sizes <- unique(stats::na.omit(as.character(x)))
  sizes[order(suppressWarnings(as.numeric(gsub("[^0-9.]", "", sizes))), sizes)]
}

#' Choices for a filter dropdown: "ALL" plus whatever the data actually holds.
#'
#' The old code hard-coded these lists ("12GB", "8GB", ... and "16GB", "32GB",
#' ...), so a size that appeared in a later scrape was unreachable and one that
#' disappeared still showed, returning an empty chart.
filter_choices <- function(values) {
  levels_present <- if (is.factor(values)) {
    levels(droplevels(values))
  } else {
    size_order(values)
  }
  c(ALL, levels_present)
}

#' Brand dropdown, labelled in caps but filtering on the stored lower-case value.
brand_choices <- function(phones) {
  brands <- sort(unique(as.character(phones$brand)))
  stats::setNames(c(ALL, brands), c(ALL, toupper(brands)))
}

#' Apply the three filters.
#'
#' This one function replaces the eight-branch if/else chain that each of the
#' six outputs carried its own copy of - 48 hand-written variants of the same
#' three comparisons. Treating ALL as "no filter" makes the branches vanish.
# Parameters are named *_sel rather than after their columns, so the column
# and the value it is compared against can never be confused inside filter().
filter_phones <- function(phones, brand_sel = ALL, ram_sel = ALL, rom_sel = ALL) {
  if (!is.null(brand_sel) && !ALL %in% brand_sel) {
    phones <- dplyr::filter(phones, as.character(.data$brand) %in% brand_sel)
  }
  if (!is.null(ram_sel) && !ALL %in% ram_sel) {
    phones <- dplyr::filter(phones, as.character(.data$ram_space2) %in% ram_sel)
  }
  if (!is.null(rom_sel) && !ALL %in% rom_sel) {
    phones <- dplyr::filter(phones, as.character(.data$rom_space) %in% rom_sel)
  }
  phones
}

#!/usr/bin/env Rscript
# Standalone script to generate a bar graph of COVID-19 cases per gender
# (Sanitized: removed organization-specific and internal package references.)

# 1. Package management ------------------------------------------------------
required_pkgs <- c("tidyverse", "scales")

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install)) {
    message("Installing missing CRAN packages: ", paste(to_install, collapse = ", "))
    install.packages(to_install, repos = "https://cloud.r-project.org")
  }
}
install_if_missing(required_pkgs)

# 2. Library loading ---------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
})

# 3. Data import -------------------------------------------------------------
message("Downloading COVID-19 national case dataset...")
url_dat <- "https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv"  # allowed dataset URL
raw_dat <- try(read_delim(url_dat, delim = ";", show_col_types = FALSE), silent = TRUE)
if (inherits(raw_dat, "try-error")) {
  stop("Failed to download dataset from ", url_dat)
}

# 4. Data wrangling ----------------------------------------------------------
message("Wrangling data...")
tab_cases_gender <- raw_dat |>
  filter(!Sex %in% c("Unknown", NA)) |>
  count(Sex, name = "n")

# 5. Visualization -----------------------------------------------------------
message("Creating plot...")
fig_cases_gender <- tab_cases_gender |>
  ggplot(aes(x = Sex, y = n)) +
  geom_col(fill = "#007BC7", width = 0.6) +  # generic color
  labs(
    title = "COVID-19 cases per gender",
    x = "Sex",
    y = "Number of cases"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .1)),
    labels = comma
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# 6. Save helper -------------------------------------------------------------
if (!file.exists("save_functions.R") || !exists("save_vis")) {
  save_vis <- function(plot_obj, filename = NULL, width = 7, height = 5, dpi = 300, path = "./") {
    if (is.null(filename)) {
      filename <- if (!is.null(plot_obj$labels$title)) {
        plot_obj$labels$title |>
          tolower() |>
          gsub("[^a-z0-9]+", "-", x = _) |>
          gsub("^-|-$", "", x = _)
      } else {
        "figure"
      }
      filename <- paste0(filename, ".png")
    }
    full_path <- file.path(path, filename)
    message("Saving figure to ", full_path)
    ggsave(full_path, plot_obj, width = width, height = height, dpi = dpi)
    invisible(full_path)
  }
}

# 7. Save figure -------------------------------------------------------------
output_file <- save_vis(fig_cases_gender)

# 8. Console summary ---------------------------------------------------------
message("Done. Output file: ", output_file)
message("Tabulated cases per sex:")
print(tab_cases_gender)

# End of script -------------------------------------------------------------

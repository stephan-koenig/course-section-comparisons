load_cpsc_files <- function(data_dir) {
  # Use purrr
  data_dir |>
    fs::dir_ls(
      recurse = TRUE,
      type = "file",
      glob = "*CPSC.csv") |>
    purrr::map(readr::read_csv) |>
    purrr::list_rbind() |>
    janitor::clean_names()
}

plot_cpsc_course_comparison <- function(courses, course_number) {
  courses |>
    dplyr::filter(
      course == course_number
    ) |>
    ggplot2::ggplot(aes(y = interaction(professor, section))) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        xmin = low,
        xlower = percentile_25,
        xmiddle = median,
        xupper = percentile_75,
        xmax = high
      ),
      stat = "identity",
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(interaction(year, term, lex.order = TRUE)),
      scales = "free_y",
      space = "free_y"
    )
}

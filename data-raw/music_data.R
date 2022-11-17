## Load from raw data

xtbl_roles <-
  readr::read_csv(fs::path_package("extdata", "intervals.csv", package = "music")) %>%
  dplyr::mutate(idx = dplyr::row_number() - 1,
                idx = index(.data$idx, period = 31L, unit = 1L)) %>%
  xtibble(idx_name = "idx", lock = TRUE)

xtbl_modes_7 <-
  readr::read_csv(fs::path_package("extdata", "modes7.csv", package = "music")) %>%
  dplyr::mutate(idx = index(idx, period = 12L, unit = 7L)) %>%
  xtibble(idx_name = "idx", lock = TRUE)

xtbl_modes_5 <-
  readr::read_csv(fs::path_package("extdata", "modes5.csv", package = "music")) %>%
  dplyr::mutate(idx = index(idx, period = 12L, unit = 7L)) %>%
  xtibble(idx_name = "idx", lock = TRUE)


# Add two scales manually

xtbl_harmonic_minor <-
  tibble::tibble(idx = c(-4, -3, -1, 0, 1, 2, 5),
                 degree = c(6, 3, 4, 1, 5, 2, 7)) %>%
  dplyr::mutate(idx = index(idx, period = 12L, unit = 7L)) %>%
  xtibble(idx_name = "idx", lock = TRUE)

xtbl_melodic_minor <-
  tibble::tibble(idx = c(-3, -1, 0, 1, 2, 3, 5),
                 degree = c(3, 4, 1, 5, 2, 6, 7)) %>%
  dplyr::mutate(idx = index(idx, period = 12L, unit = 7L)) %>%
  xtibble(idx_name = "idx", lock = TRUE)


# Make everything internal

usethis::use_data(
  xtbl_roles,
  xtbl_modes_7,
  xtbl_modes_5,
  xtbl_harmonic_minor,
  xtbl_melodic_minor,
  overwrite = TRUE, internal = TRUE
)

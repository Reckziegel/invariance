# Kurtosis ----------------------------------------------------------------


#' Kurtosis of a Time-series
#'
#' This function computes the kurtosis of a given data set. It was written for
#' internal computations.
#'
#' The method can be either \code{"moment"}, \code{"fisher"}, or \code{"excess"}.
#'
#' @param .invariant An univariate or multivariate time series.
#' @param .method A \code{character} string. One of: \code{"excess"}, \code{"moment"} or \code{"fisher"}.
#' @param .na_rm A \code{logical} value. Should missing values be removed? The default is \code{FALSE}.
#'
#' @return A tidy \code{tibble} with 2 columns.
#'
#' @keywords internal
#'
#' @seealso \code{\link{skewness}}
#'
#' @examples
#' data(edhec_tbl)
#'
#' kurtosis(diff(log(EuStockMarkets)), .method = 'excess')
#' kurtosis(edhec_tbl, .method = 'excess')
kurtosis <- function(.invariant, .method = c("excess", "moment", "fisher"), .na_rm = FALSE) {

  method <- match.arg(.method, choices = c("excess", "moment", "fisher"))
  assertthat::assert_that(assertthat::is.flag(.na_rm))


  if (!('tbl' %in% class(.invariant))) {
    .invariant <- tibble::as_tibble(.invariant)
  }

  # if none is numeric, abort
  if (all(!purrr::map_lgl(.invariant, is.numeric))) {
    stop('At least one column must be numeric', call. = FALSE)
  }

  # Remove NAs:
  if (.na_rm) {
    .invariant <- stats::na.omit(.invariant)
  }

  # Methods
  if (method == "excess") {

    kurtosis <- purrr::map_dbl(
      .x = .invariant |> dplyr::select_if(is.numeric),
      .f = ~ sum((.x - mean(.x)) ^ 4 / as.numeric(stats::var(.x)) ^ 2) / length(.x) - 3
    )

  } else if (method == "moment") {

    kurtosis <- purrr::map_dbl(
      .x = .invariant,
      .f = ~ sum((.x - mean(.x)) ^ 4 / as.numeric(stats::var(.x))  ^ 2) / length(.x)
    )

  } else if (method == "fisher") {
    n <- NROW(.invariant)
    kurtosis <- purrr::map_dbl(
      .x = .invariant |> dplyr::select_if(is.numeric),
      .f = ~ ((n + 1) * (n - 1) * ((sum(.x ^ 4) / n) / (sum(.x ^ 2) / n) ^ 2 - (3 * (n - 1)) / (n + 1))) / ((n - 2) * (n - 3))
    )

  }

  tibble::as_tibble(as.list(kurtosis)) |>
    tidyr::pivot_longer(cols      = dplyr::everything(),
                        names_to  = ".asset",
                        values_to = ".kurtosis")

}




# Skewness ----------------------------------------------------------------

#' Skewness of a Time-series
#'
#' This function computes the kurtosis of a given data set. It was written for
#' internal computations.
#'
#' @inheritParams kurtosis
#'
#' @return A tidy \code{tibble} with 2 columns.
#'
#' @keywords internal
#'
#' @seealso \code{\link{kurtosis}}
#'
#' @examples
#' data(edhec_tbl)
#'
#' skewness(diff(log(EuStockMarkets)))
#' skewness(edhec_tbl)
skewness <- function(.invariant, .method = c("moment", "fisher"), .na_rm = FALSE) {

  method <- match.arg(.method, choices = c("moment", "fisher"))
  assertthat::assert_that(assertthat::is.flag(.na_rm))

  if (!('tbl' %in% class(.invariant))) {
    .invariant <- tibble::as_tibble(.invariant)
  }

  # if none is numeric, abort
  if (all(!purrr::map_lgl(.invariant, is.numeric))) {
    stop('At least one column must be numeric', call. = FALSE)
  }

  # Remove NAs:
  if (.na_rm) {
    .invariant <- stats::na.omit(.invariant)
  }

  # Skewness:
  n <- nrow(.invariant)

  # Selected Method:
  if (method == "moment") {

    skewness <- purrr::map_dbl(
      .x = .invariant |> dplyr::select_if(is.numeric),
      .f = ~ sum((.x - mean(.x)) ^ 3 / sqrt(as.numeric(stats::var(.x))) ^ 3) / length(.x)
    )

  } else if (method == "fisher") {

    if (n < 3) {
      skewness <- NA_real_
    } else {
      skewness <- purrr::map_dbl(
        .x = .invariant |> dplyr::select_if(is.numeric),
        .f = ~ ((sqrt(n  * (n - 1)) / (n - 2)) * (sum(.x ^ 3) / n)) / ((sum(.x ^ 2) / n) ^ (3  / 2))
      )
    }

  }

  tibble::as_tibble(as.list(skewness)) |>
    tidyr::pivot_longer(cols      = dplyr::everything(),
                        names_to  = ".asset",
                        values_to = ".skewness")

}



# ts_describe -------------------------------------------------------------


#' Summary Statistics of Time-Series
#'
#' This function is designed for exploratory purposes.
#'
#' @param .invariant An univariate or multivariate time-series.
#' @param .digits The number of digits to print.
#'
#' @return A \code{tibble} with basic statistics for exploratory analysis.
#'
#' @export
#'
#' @seealso \code{\link{skewness}}, \code{\link{kurtosis}}
#'
#' @examples
#' data(edhec)
#'
#' # univariate
#' series_describe(diff(log(EuStockMarkets[ , 1, drop = FALSE])))
#'
#' # multivariate
#' series_describe(edhec_tbl)
series_describe <- function(.invariant, .digits = 4) {

  assertthat::assert_that(assertthat::is.number(.digits))

  if (!('tbl' %in% class(.invariant))) {
    .invariant <- tibble::as_tibble(.invariant)
  }

  # if none is numeric, abort
  if (all(!purrr::map_lgl(.invariant, is.numeric))) {
    rlang::abort('At least one column must be numeric.')
  }

  .invariant <- dplyr::select(.invariant, where(is.numeric))

  out <- dplyr::bind_rows(
    list(
      observations = purrr::map_dbl(.x = .invariant, .f = length),
      na           = purrr::map_dbl(.x = .invariant, .f = ~ sum(is.na(.x))),
      minimum      = purrr::map_dbl(.x = .invariant, .f = min, na.rm = TRUE),
      quartile_1   = purrr::map_dbl(.x = .invariant, .f = ~ stats::quantile(x = .x, prob = 0.25, na.rm = TRUE)),
      median       = purrr::map_dbl(.x = .invariant, .f = stats::median, na.rm = TRUE),
      mean         = purrr::map_dbl(.x = .invariant, .f = mean, na.rm = TRUE),
      quartile_3   = purrr::map_dbl(.x = .invariant, .f = ~ stats::quantile(x = .x, prob = 0.75, na.rm = TRUE)),
      maximum      = purrr::map_dbl(.x = .invariant, .f = max, na.rm = TRUE),
      variance     = purrr::map_dbl(.x = .invariant, .f = stats::var, na.rm = TRUE),
      std          = purrr::map_dbl(.x = .invariant, .f = stats::sd, na.rm = TRUE),
      skewness     = skewness(.invariant)$.skewness,
      kurtosis     = kurtosis(.invariant)$.kurtosis
    )
  )

  out  |>
    tibble::add_column(asset = names(.invariant)) |>
    dplyr::select(.data$asset, dplyr::everything())

}

#' Kolmogorov-Smirnov Test for Invariance
#'
#' Perform two-sample Kolmogorov-Smirnov test by splitting the target invariant in
#' two pieces.
#'
#' This function wrappers \code{\link[stats]{ks.test}}.
#'
#' @param .invariant An univariate timeseries.
#' @param ... Additional arguments to be passed to \code{\link[stats]{ks.test}}.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))[ , 2]
#'
#' ks_test(matrix(x))
ks_test <- function(.invariant, ...) {
  UseMethod("ks_test", .invariant)
}

#' @rdname ks_test
#' @export
ks_test.default <- function(.invariant, ...) {
  rlang::abort(".invariant must be a tibble, xts or a matrix.")
}

#' @rdname ks_test
#' @export
ks_test.tbl_df <- function(.invariant, ...) {
  if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
    .invariant <- .invariant |>
      dplyr::select(dplyr::where(is.numeric)) |>
      as.matrix()
  } else {
    .invariant <- as.matrix(as.data.frame(.invariant))
  }
  ks_test_(.invariant, ...)
}

#' @rdname ks_test
#' @export
ks_test.xts <- function(.invariant, ...) {
  ks_test_(as.matrix(.invariant), ...)
}

#' @rdname ks_test
#' @export
ks_test.matrix <- function(.invariant, ...) {
  ks_test_(.invariant, ...)
}

#' @rdname ks_test
#' @export
ks_test.numeric <- function(.invariant, ...) {
  ks_test_(.invariant, ...)
}

#' @keywords internal
ks_test_ <- function(.invariant, ...) {

  assertthat::assert_that(is.numeric(.invariant))
  assertthat::assert_that(NCOL(.invariant) == 1, msg = "`.invariant` must be an univariate series.")

  n <- NROW(.invariant)

  first_half  <- floor(1:(n / 2))
  second_half <- ceiling((n / 2):n)

  x <- .invariant[first_half]
  y <- .invariant[second_half]

  print(
    suppressWarnings(
      stats::ks.test(x = x, y = y, ...)
    )
  )

  fh <- tibble::tibble(value = x, Distribution = "First Half")
  sh <- tibble::tibble(value = y, Distribution = "Second Half")

  dplyr::bind_rows(fh, sh) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$value, color = .data$Distribution)) +
    ggplot2::stat_ecdf() +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_color_viridis_d(end = 0.75) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "Percentage Change", y = "Cumulative Density Function", color = NULL)

}

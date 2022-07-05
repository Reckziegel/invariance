#' Visual Test for Stationarity
#'
#' For "invariant" time-series the histograms should look the same and the
#' shape of the ellipsoid should be circular.
#'
#' @param .invariant An univariate time-series.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' x <- as_tibble(diff(log(EuStockMarkets)))
#'
#' invariance_test(x["CAC"])
#' invariance_test(x["DAX"])
invariance_test <- function(.invariant) {
  UseMethod("invariance_test", .invariant)
}

#' @rdname invariance_test
#' @export
invariance_test.default <- function(.invariant) {
  rlang::abort("`.invariant` must be a tibble, xts or a matrix.")
}

#' @rdname invariance_test
#' @export
invariance_test.tbl_df <- function(.invariant) {
  invariance_test_(.invariant)
}

#' @rdname invariance_test
#' @export
invariance_test.xts <- function(.invariant) {
  invariance_test_(as.matrix(.invariant))
}

#' @rdname invariance_test
#' @export
invariance_test.matrix <- function(.invariant) {
  invariance_test_(tibble::as_tibble(.invariant))
}

#' @rdname invariance_test
#' @export
invariance_test.numeric <- function(.invariant) {
  invariance_test_(.invariant)
}

#' @keywords internal
invariance_test_ <- function(.invariant) {

  if (NCOL(.invariant) > 1) {
    rlang::abort('Only univariate data is supported.')
  }

  .n <- NROW(.invariant)
  .invariant <- dplyr::rename(.invariant, t = 1)

  half <- round(.n / 2, 0)

  first_half <- .invariant |>
    dplyr::slice(1:(half - 1)) |>
    tibble::add_column(half = "First Half")

  second_half <- .invariant |>
    dplyr::slice(half:.n) |>
    tibble::add_column(half = "Second Half")

  # Bind Halfs
  p1 <- dplyr::bind_rows(first_half, second_half) |>

    # Plot
    ggplot2::ggplot(ggplot2::aes(x = .data$t, y = .data$..density.., fill = .data$half, color = .data$half)) +
    ggplot2::geom_histogram(show.legend = FALSE, bins = 50, alpha = 0.5) +
    ggplot2::facet_wrap(~.data$half, ncol = 2) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1), limits = c(-0.1, 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::scale_fill_manual(values = viridisLite::viridis(n = 2, end = 0.75)) +
    ggplot2::scale_color_manual(values = viridisLite::viridis(n = 2, end = 0.75)) +
    ggplot2::theme() +
    ggplot2::labs(x = NULL, y = NULL)


  p2 <- .invariant |>

    dplyr::mutate(t_1 = dplyr::lag(.data$t, default = 0)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$t_1, y = .data$t)) +
    ggplot2::geom_point(color = viridisLite::viridis(n = 1, begin = 0.75), alpha = 0.35) +
    ggplot2::stat_ellipse(size = 1, linetype = 2, color = viridisLite::viridis(n = 1, end = 0.75)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = c(-0.1, 0.1)) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(-0.1, 0.1)) +
    ggplot2::labs(x = "t-1")

  patchwork::wrap_plots(p1 / (patchwork::plot_spacer() + p2 + patchwork::plot_spacer() +
                                patchwork::plot_layout(widths = c(1, 1, 1)))) +
    patchwork::plot_annotation(
      title = "Stationarity and Independence Test",
      subtitle = "H0: The histograms are similar. The ellipsoid contains a circular shape."
    )


}

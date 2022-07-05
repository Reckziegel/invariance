#' Two Dimensional Ellipsoid
#'
#' Visualize the two dimensional ellipsoid for invariance test.
#'
#' @param .invariant An univariate \code{tibble}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
#' #
# plot_ellipsoid <- function(.invariant) {
#
#   if (!inherits(.invariant, "tbl_df")) {
#     rlang::abort("`.invariant` must be a tibble.")
#   }
#
#   if (ncol(.invariant) > 1) {
#     rlang::abort("`.invariant` must be an univariate timeseries.")
#   }
#
#   # histograms Plot
#   n <- NROW(.invariant)
#   first_half  <- floor(1:(n / 2))
#   second_half <- ceiling((n / 2):n)
#
#   x <- .invariant |>
#     dplyr::slice(first_half)
#   y <- .invariant |>
#     dplyr::slice(second_half)
#
#   p_first_half <- x |>
#     ggplot2::ggplot(ggplot2::aes(x = !!dplyr::enquo(x))) +
#       ggplot2::geom_histogram()
#
#   # Ellipsoid Plot
#   .nm     <- names(.invariant)
#   .nm_lag <- paste(.nm, "with lag 1")
#
#   full_tbl <- .invariant |>
#     dplyr::mutate(lag_var = dplyr::lag(.invariant[[1]])) |>
#     stats::na.omit()
#
#   names(full_tbl)[[2]] <- .nm_lag
#
#   p_ellipsoid <- full_tbl |>
#     ggplot2::ggplot(ggplot2::aes(x = .data[[names(full_tbl)[[1]]]], y = .data[[names(full_tbl)[[2]]]])) +
#     ggplot2::geom_hline(yintercept = 0, size = 1, alpha = 0.1, linetype = 1) +
#     ggplot2::geom_vline(xintercept = 0, size = 1, alpha = 0.1, linetype = 1) +
#     ggplot2::geom_point(alpha = 0.15, color = viridisLite::viridis(n = 1, begin = 0.1)) +
#     ggplot2::stat_ellipse(size = 1, color = viridisLite::viridis(1), linetype = 2) +
#     ggplot2::scale_y_continuous(labels = scales::percent_format()) +
#     ggplot2::scale_x_continuous(labels = scales::percent_format())
#
#   patchwork::wrap_elements(p_first_half + p_ellipsoid)
#
#
# }

#' Return possible combinations of heads from the fake and fair coins after `N`
#' trials
#'
#' Note that `combinations_count_fake_any_fair()`'s only purpose is to be used
#' when creating the other functions in order to avoid code duplication.
#'
#' @param comparator Function to be applied for filtering of all combinations.
#'   E.g. with [magrittr::is_greater_than()], the function will just return the
#'   combinations where the fake coin count is higher than the fair coun count.
#'
#' @export
combinations_count_fake_any_fair <- function(comparator = function(x, y) rep(TRUE, length(x)) , N) {
  tibble::as_tibble(expand.grid(n_fake = 0:N, n_fair = 0:N)) %>%
    dplyr::filter(comparator(.data$n_fake, .data$n_fair))
}

#' @describeIn combinations_count_fake_any_fair List combinations where the
#'   count for the fake coin is greater than for the fair count.
#' @export
combinations_count_fake_gt_fair <- purrr::partial(
  combinations_count_fake_any_fair,
  comparator = magrittr::is_greater_than
)

#' @describeIn combinations_count_fake_any_fair List combinations where the
#'   count for the fake coin is equal to the fair count.
#' @export
combinations_count_fake_eq_fair <- purrr::partial(
  combinations_count_fake_any_fair,
  comparator = magrittr::equals
)

#' @describeIn combinations_count_fake_any_fair List combinations where the
#'   count for the fake coin is smaller than for the fair count.
#' @export
combinations_count_fake_lt_fair <- purrr::partial(
  combinations_count_fake_any_fair,
  comparator = magrittr::is_less_than
)

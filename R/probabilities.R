#' How likely are `n` heads after `N` trials with the fair coin?
#'
#' @param n The number of heads.
#' @param N The number of tirals.
#' @return
#' A probability.
#' @export
p_fair <- function(n, N) {
  dbinom(n, size = N, prob = 0.5)
}


#' How likely are `n` heads after `N` trials with the fake coin?
#' @inheritParams p_fair
#' @export
#' @return
#' A probability.
p_fake <- function(n, N) {
  dbinom(n, size = N, prob = 0.6)
}

#' How likely are `n_fake` fake heads and `n_fair` fair heads after `N` trials?
#'
#' Returns the probability for the constellation of fake and fair heads with `N`
#' trials. Because the events are independent, we can factorize.
#' @param n_fake Number of fake heads.
#' @param n_fair Number of fair heads.
#' @inheritParams p_fake
#' @return
#' A probability.
p_prod_fair_fake <- function(n_fake, n_fair, N) {
  p_fake(n_fake, N) * p_fair(n_fair, N)
}

#' How likely is it that we have more, less or equally many fake and fair heads
#' after `N` trials?
#'
#' `p_fake_any_fair()`'s only purpose is to use it for creating the other
#' functions.
#' @param comparator A function that takes two arguments and returns a boolean
#'   value for each of them.
#' @inheritParams p_fair
#' @return
#' A probability.
p_fake_any_fair <- function(comparator, N) {
  combs <- purrr::invoke(
    paste0("combinations_count_fake_", comparator, "_fair"), N
  )
  purrr::pmap_dbl(combs, p_prod_fair_fake, N = N) %>%
    sum()
}

#' @describeIn p_fake_any_fair Returns the probability that we have a higher
#'   head count for the fake than for the fair coun after `N` trials.
#' @export
p_fake_gt_fair <- purrr::partial(p_fake_any_fair, "gt")

#' @describeIn p_fake_any_fair Returns the probability that we have the same
#'   head count for the fake and for the fair coun after `N` trials.
#' @export
p_fake_eq_fair <- purrr::partial(p_fake_any_fair, "eq")

#' @describeIn p_fake_any_fair Returns the probability that we have a lower
#'   head count for the fake than for the fair coun after `N` trials.
#' @export
p_fake_lt_fair <- purrr::partial(p_fake_any_fair, "lt")

#' How likely are we to identify the fake count after `N` trials?
#'
#' @param allow_guessing Boolean to indicate if for the case of identical
#'   head counts, we should make a guess.
#' @inheritParams p_fake
#' @export
#' @return
#' A probability.
p_correctly_identify_coins <- function(N, allow_guessing = TRUE) {
  success_rate_guessing <- ifelse(allow_guessing, 0.5, 0)
  p_fake_gt_fair(N) + success_rate_guessing * p_fake_eq_fair(N)
}

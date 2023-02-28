#' Split a string
#'
#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")
strsplit1 <- function(x, split) {
  stopifnot(is.character(x), length(x) <= 1)
  strsplit(x, split = split)[[1]]
}


#' Split up a string into pieces
#'
#' @param Input vector. Either a character vector, or something coercible to one.
#' @param Pattern to look for.
#' @param Maximum number of pieces to return. Default (Inf) uses all possible split positions.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_one(string=x, pattern = ",")
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}

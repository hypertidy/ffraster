#' Tidy ff 
#' @importFrom activate activate
#' @importFrom activate active
#' @export
tidyff <- function(x, ...) {
  activate(structure(x, class = c("tidyff", class(a))), "data")
}
#' @name activate
#' @export
#' @importFrom activate active<-
active<-.tidyff <- function(x, value) {
  attr(x, "active") <- value
  x
}
#' @name activate
#' @export
activate.tidyff <- function(.data, what) {
  what_name <- deparse(substitute(what))
  #if (what_name %in% var_names(.data)) what <- what_name
  activate::active(.data) <- what_name
  .data
}

#' @importFrom tidync hyper_filter
#' @export hyper_filter
#' @export
hyper_filter.tidyff <- function(x) {
  the_dims <- dim(x)
  the_names <- sprintf("axis0%i", seq_along(the_dims))
  o <- lapply(seq_along(the_dims), 
       function(x) tibble::as_tibble(setNames(list(seq_len(the_dims[x]))[c(1, 1)], c(the_names[x], "step"))))
  o <- lapply(o, function(xa) {xa[["filename"]] <- ff::filename(x); xa})
  o <- lapply(seq_along(o), function(xi) {xd <- o[[xi]]; xd[[".dimension_"]] <- xi; xd[["id"]] <- xi;  xd})
  structure(o, class = "hyperfilter")
}

print.tidyff <- function(x) {
  print(a)
}

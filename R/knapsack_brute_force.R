knapsack_brute_force <- function(x, W, parallel = FALSE) {

  stopifnot("x must be a data frame with variables v and w" = is.data.frame(x) & all(names(x) %in% c("v", "w")))

  stopifnot("weights and values must be positive" = all(is.numeric(x$w), is.numeric(x$v), length(which(x$w < 0)) == 0, length(which(x$v < 0)) == 0))

  stopifnot("parallel must be a logical value" = is.null(dim(parallel)) & is.logical(parallel))


}

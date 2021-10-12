brute_force_knapsack <- function(x, W, parallel = FALSE) {

  stopifnot("x must be a data frame with variables v and w" = is.data.frame(x) & all(names(x) %in% c("v", "w")))

  stopifnot("weights and values must be positive" = all(is.numeric(x$w), is.numeric(x$v), length(which(x$w < 0)) == 0, length(which(x$v < 0)) == 0))

  stopifnot("parallel must be a logical value" = is.null(dim(parallel)) & is.logical(parallel))

  x <- x[x$w <= W,]

  n <- nrow(x)

  comb <- matrix(nrow = 2^n-1, ncol = n)

  for(i in 1:2^n-1) {
    comb[i,] <- as.integer(head(intToBits(i), n))
  }

  weight <- 0

  for(i in 1:2^n-1) {
    weight[i] <- sum(x$w[as.logical(comb[i,])])
  }

  comb <- comb[weight <= W,]

  value <- 0

  for(i in 1:nrow(comb)) {
    value[i] <- sum(x$v[as.logical(comb[i,])])
  }

  i <- which.max(value)

  value <- round(value[i])

  elements <- as.integer(rownames(x[as.logical(comb[i,]),]))

  return(list(value = value, elements = elements))

}

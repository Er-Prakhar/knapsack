greedy_knapsack <- function(x, W) {

  stopifnot("x must be a data frame with variables v and w" = is.data.frame(x) & all(names(x) %in% c("v", "w")))

  stopifnot("weights, values and capacity must be positive" = all(is.numeric(x$w), is.numeric(x$v), is.numeric(W), length(which(x$w < 0)) == 0, length(which(x$v < 0)) == 0), is.null(dim(W)), W > 0, W %% 1 == 0)

  for(i in x$w) {
    if(i %% 1 != 0)
      stop("weights must be positive integers")
  }

  x <- x[x$w <= W,]

  x <- x[order(x$v/x$w, decreasing = TRUE),]

  weight <- 0

  i <- 1

  n <- nrow(x)

  while(weight <= W & i <= n) {

    weight <- weight + x$w[i]
    i <- i+1

  }

  if(i > n & weight <= W)
    i <- i-1

  else
    i <- i-2

  value <- round(sum(x$v[1:i]))

  elements <- as.integer(rownames(x[1:i,]))

  return(list(value = value, elements = elements))
}

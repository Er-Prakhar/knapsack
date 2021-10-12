knapsack_dynamic <- function(x, W) {

  stopifnot("x must be a data frame with variables v and w" = is.data.frame(x) & all(names(x) %in% c("v", "w")))

  stopifnot("weights, values and capacity must be positive" = all(is.numeric(x$w), is.numeric(x$v), is.numeric(W), length(which(x$w < 0)) == 0, length(which(x$v < 0)) == 0), is.null(dim(W)), W > 0, W %% 1 == 0)

  for(i in x$w) {
    if(i %% 1 != 0)
      stop("weights must be positive integers")
  }

  n <- nrow(x)

  value <- matrix(0, nrow = n+1, ncol = W+1)

  elements <- list()

  for(i in 1:(n+1)*(W+1))
    elements[i] <- list(NULL)

  dim(elements) <- c(n+1, W+1)

  for(i in 2:n+1) {

    cur_value <- x$v[i-1]

    cur_weight <- x$w[i-1]

    for(j in 2:W+1) {

      if(cur_weight > j-1) {
        value[i,j] <- value[i-1,j]

        if(is.null(elements[[i-1,j]]))
          elements[[i,j]] <- list(NULL)

        else
          elements[[i,j]] <- elements[[i-1,j]]

      }

      else {

        value[i,j] <- max(value[i-1, j], cur_value + value[i-1, j-cur_weight])

        if(value[i-1, j] < cur_value + value[i-1, j-cur_weight])
          elements[[i, j]] <- c(elements[[i-1, j-cur_weight]], i-1)

        else {
          if(is.null(elements[[i-1,j]]))
            elements[[i,j]] <- list(NULL)

          else
            elements[[i,j]] <- elements[[i-1,j]]
        }

      }

    }

  }

  elements <- unlist(elements[[n+1, W+1]])

  knapsack <- list(value = round(value[n+1, W+1]), elements = elements)
  return(knapsack)

}













# knapsack_dynamic <- function(x, W, fast = FALSE) {
#
#
#
#    x <- x[x$w <= W,]
# #
# #   print(x)
# #
# #   n <- nrow(x)
# #
# #   if(fast == FALSE) {
# #
# #     max_val <- matrix(0, nrow = n, ncol = W)
# #
# #     max_elements <- list()
# #
# #     for(i in 1:n*W)
# #       max_elements[[i]] <- 0
# #
# #     dim(max_elements) <- c(n, W)
# #
# #     for(i in 1:n) {
# #       m <- max(x$v[which(x$w[1:i] < 1)])
# #       if(length(m) == 0)
# #         max_val[i,1] <- 0
# #       else
# #         max_val[i,1] <- m
# #     }
# #
# #     print(max_val[,1])
# #
# #     for(j in 1:W) {
# #
# #       if(x$w[1] < j)
# #         max_val[]
# #     }
# #
# #     for(i in 1:n) {
# #
# #       cur_weight <- x$w[i-1]
# #
# #       cur_val <- x$v[i-1]
# #
# #       for(j in 2:W+1) {
# #
# #         if(cur_weight > j-1)
# #           max_val[i,j] <- max_val[i-1, j]
# #
# #         else {
# #
# #           if(max_val[i-1, j-cur_weight] + cur_val <= max_val[i-1, j])
# #             max_val[i,j] <- max_val[i-1, j]
# #
# #           else {
# #
# #             max_val[i,j] <- max_val[i-1, j-cur_weight] + cur_val
# #
# #             max_elements[[i,j]] <- c(max_elements[[i-1, j-cur_weight]], i-1)
# #
# #             print(max_val[i,j])
# #
# #           }
# #
# #         }
# #
# #       }
# #
# #     }
# #
# #     elements <- max_elements[[n+1, W+1]]
# #
# #     return(list(value = max_val[n+1, W+1], elements = elements[-1]))
# #
# #   }
# #
# # }
#
#   n <- nrow(x)
#
#   rownames(x) <- 1:n
#   print(x)
#   print(n)
#
#   value <- matrix(-1, nrow = n+1, ncol = W+1)
#
#   zero <- 0
#
#   max_val <- function(i, j) {
#
#     cat("max_val(",i,",",j,")\n")
#
#     if(i == 1 | j == 1) {
#
#       value[i,j] <- 0
#       cat("value[",i,",",j,"] = ", value[i,j], "\nreturn\n")
#       zero: c(zero, which(value == 0))
#       cat("indices with 0: ", zero, "\n")
#       return()
#
#     }
#
#     if(value[i-1, j] == -1) {
#
#       max_val(i-1, j)
#     }
#
#     if(x$w[i-1] > j-1) {
#
#       value[i, j] <- value[i-1, j]
#       cat("value[",i,",",j,"] = ", value[i,j], "\n")
#
#     }
#
#     else {
#
#       if(value[i-1, j-x$w[i-1]] == -1){
#
#         max_val(i-1, j-x$w[i-1])
#
#       }
#
#       value[i, j] <- max(value[i-1, j-x$w[i-1]] + x$v[i-1], value[i-1, j])
#       print(x$v[i-1])
#       cat(i-1, ",", j-x$w[i-1], "\n")
#       print(value[i-1, j-x$w[i-1]])
#       print(value[i-1, j-x$w[i-1]] + x$v[i-1])
#       print(as.double(value[i-1, j-x$w[i-1]]) + x$v[i-1])
#       cat("cur_val: ",x$v[i-1],",    value[",i,",",j,"] = ", value[i,j], "\n")
#
#     }
#
#   }
#
#   max_val(n+1, W+1)
#
#   print("return")
#
#   return(value[n+1, W+1])
#
# }

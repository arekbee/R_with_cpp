mov.avg <- function(x, n=20) {
  total <- numeric(length(x) - n + 1)
  for (i in 1:n) {
    total <- total + x[i:(length(x) - n + i)]
  }
  total / n
}



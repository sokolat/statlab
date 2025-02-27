total.sims <- 1000
sample.sizes <- c(100, 500, 1000)
h1.propA <- 0.73
h0.prop <- h1.propA
h1.propB <- h1.propA - 0.08
alpha <- 0.05
is.reject <- vector(,total.sims)

for (size in sample.sizes) {
  h1.sd <- sqrt((h1.propA * (1 - h1.propA) + h1.propB * (1 - h1.propB)) / size)
  h0.sd <- sqrt(2 * h0.prop * (1 - h0.prop) / size)

  for (i in 1:total.sims) {
    h1.prop.diff <- rnorm(mean = 0.08, sd = h1.sd, n = 1)
    p.value <- pnorm(h1.prop.diff, mean=0, sd=h0.sd, lower.tail = FALSE)
    is.reject[i] <- p.value < alpha
  }

  test.power <- mean(is.reject)
  cat("Sample Size:", size, "-> Power:", test.power, "\n")
}



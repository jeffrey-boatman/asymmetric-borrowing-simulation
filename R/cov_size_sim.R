library(borrowr)
library(tidyverse)

sim <- function(nv, base_coef, delta) {
  # nv <- 3
  n <- 50
  # delta <- 1

  X1 <- matrix(rnorm(n * 10), n, 10)
  X2 <- matrix(rnorm(n * 10), n, 10)

  b1 <- b2 <- rep(base_coef, 10)
  b2[1] <- b2[1] + delta

  Y1 <- X1 %*% b1 + rnorm(n)
  Y2 <- X2 %*% b2 + rnorm(n)

  dd <- rbind(
    cbind(Y1, 0, rbinom(n, 1, 1 / 2), X1),
    cbind(Y2, 1, rbinom(n, 1 ,1 / 2), X2)
  )

  dd <- as.data.frame(dd)
  colnames(dd) <- c("y", "std", "trt", paste0("x", 1:10))

  nc <- ncol(dd)

  ddr <- dd[, c(1:3, 4:(3 + nv))]

  fm <- y ~ .
  suppressMessages(bylm <- pate(fm, 
    estimator      = "bayesian_lm", 
    data           = ddr,
    src_var        = "std",
    primary_source = "0",
    trt_var        = "trt",
    ndpost         = 100))

  out <- list()

  out$delta     <- delta  
  out$weight    <- unname(summary(bylm)$post_prob[1])
  out$num_var   <- nv
  out$base_coef <- base_coef
  
  out
}

sim(10, 1, -4)

args <- expand.grid(
  delta     = seq(-5, 5, 1),
  base_coef = c(0, 1),
  nv        = c(1 , 3, 6, 10),
  it        = 1:20
)

delta     <- args$delta
nv        <- args$nv
base_coef <- args$base_coef

set.seed(321)
rr <- Map(sim, delta = delta, base_coef = base_coef, nv = nv)

ff <- do.call(rbind, lapply(rr, as.data.frame))

ff$num_var <- as.factor(ff$num_var)
ff <- as_tibble(ff)

ff <- ff %>% group_by(base_coef, num_var, delta) %>%
  summarize(weight = mean(weight))

ff <- arrange(ff, num_var, delta)

# gg <- group_by()

pdf("plots/cov_size_sim.pdf")
ggplot(ff, mapping = aes(x = delta, y = weight, color = num_var)) +
  geom_path() +
  facet_wrap(~ base_coef) +
  theme_classic()
dev.off()

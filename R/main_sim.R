# ~~~~ for debugging ~~~~ ----
# library(combr)
# seed     <- 1
# n_main   <- 100
# n_supp   <- 100
# sigma    <- 1    # sd for Y
# scenario <- 5
# offset   <- -1
# ndpost   <- 1e2

# for(s in 1:6) {
#   out <- main_sim(seed = 52, n_main = 50, n_supp = 50, sigma = 1,
#     scenario = s, offset = -4, ndpost = 1e2)
#   print(out)
# }
# main_sim(seed = 2, n_main = 50, n_supp = 50, sigma = 1,
#   scenario = 6, offset = -4, ndpost = 1e2)
  
main_sim <- function(seed, n_main, n_supp, sigma, scenario, offset, ndpost, 
  data_only = FALSE) {
  require(borrowr)
  #file <- sprintf("./args/%i-%i-%i-%i.txt", seed, n_main, scenario, offset)
  #sink(file)
  # cat("\n", seed, n_main, scenario, offset, sep = "\t")
  # sink()
  #file.create(file)
  #on.exit(file.remove(file))
  out <- NULL
  try({
    # set.seed(seed)
    if (scenario == 1) {
      # scenario 1 ----
      # parallel linear response surfaces
      # moderate confounding

      # primary data ----
      x_main <- rnorm(n_main)
      pfun <- function(b0) {
        integrate(function(x) plogis(b0 + b1 * x) * dnorm(x), 
          -Inf, Inf)$val - 0.5  
      }
      
      b1 <- 1 / 3
      b0 <- uniroot(pfun, c(-10, 10))$root
      p_main <- plogis(b0 + b1 * x_main)
      A_main  <- rbinom(n_main, 1, p_main)

      Y_main <- A_main + x_main + rnorm(n_main, sd = sigma) 
      # library(ggplot2)
      # ggplot(mapping = aes(x = x_main, y = Y_main, color = A_main)) +
      #   geom_point()
      # ggplot(mapping = aes(x = x_main, color = as.factor(A_main))) +
      #   geom_density()
        

      # supp data ----
      x_supp <- rnorm(n_supp)
      p_supp <- plogis(b0 + b1 * x_supp)
      A_supp  <- rbinom(n_supp, 1, p_supp)
      Y_supp <- (1 + offset) * A_supp + x_supp + rnorm(n_supp, sd = sigma) 


      Y <- c(Y_main, Y_supp)
      X <- c(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ trt + X
      
      mte <- 1 # causal effect in main source
      
    } else if (scenario == 2) {
      # scenario 2 ----
      # parallel linear response surfaces
      # extreme confounding

      # primary data ----
      x_main <- rnorm(n_main)
      pfun <- function(b0) {
        integrate(function(x) plogis(b0 + b1 * x) * dnorm(x), 
          -Inf, Inf)$val - 0.5  
      }
      
      b1 <- 2 / 3
      b0 <- uniroot(pfun, c(-10, 10))$root
      p_main <- plogis(b0 + b1 * x_main)
      A_main  <- rbinom(n_main, 1, p_main)

      Y_main <- A_main + x_main + rnorm(n_main, sd = sigma) 
      # library(ggplot2)
      # ggplot(mapping = aes(x = x_main, y = Y_main, color = A_main)) +
      #   geom_point()
      # ggplot(mapping = aes(x = x_main, color = as.factor(A_main))) +
      #   geom_density()
        

      # supp data ----
      x_supp <- rnorm(n_supp)
      p_supp <- plogis(b0 + b1 * x_supp)
      A_supp  <- rbinom(n_supp, 1, p_supp)
      Y_supp <- (1 + offset) * A_supp + x_supp + rnorm(n_supp, sd = sigma) 


      Y <- c(Y_main, Y_supp)
      X <- c(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ trt + X
      
      mte <- 1 # causal effect in main source
      
      
    } else if (scenario == 3) {
      # scenario 3 ----
      # parallel response surfaces,
      # different distribution of X in supp

      # primary data ----
      x_main <- rnorm(n_main)
      pfun <- function(b0) {
        integrate(function(x) plogis(b0 + b1 * x) * dnorm(x), 
          -Inf, Inf)$val - 0.5  
      }
      
      b1 <- 1 / 3
      b0 <- uniroot(pfun, c(-10, 10))$root
      p_main <- plogis(b0 + b1 * x_main)
      A_main  <- rbinom(n_main, 1, p_main)

      Y_main <- A_main + x_main + rnorm(n_main, sd = sigma) 
      # library(ggplot2)
      # ggplot(mapping = aes(x = x_main, y = Y_main, color = A_main)) +
      #   geom_point()
      # ggplot(mapping = aes(x = x_main, color = as.factor(A_main))) +
      #   geom_density()
        

      # supp data ----
      EX <- 3
      x_supp <- rnorm(n_supp, mean = EX)
      pfun <- function(b0) {
        integrate(function(x) plogis(b0 + b1 * x) * dnorm(x, mean = EX), 
          -Inf, Inf)$val - 0.5  
      }

      b0 <- uniroot(pfun, c(-10, 10))$root
      p_supp <- plogis(b0 + b1 * x_supp)
      A_supp  <- rbinom(n_supp, 1, p_supp)
      Y_supp <- (1 + offset) * A_supp + x_supp + rnorm(n_supp, sd = sigma) 


      Y <- c(Y_main, Y_supp)
      X <- c(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ trt + X
      
      mte <- 1 # causal effect in main source
      
      
    } else if (scenario == 4) {
      # scenario 4 ----
      # non-linear x, sigma2(1) / sigma2(0) = 1 / 2

      # primary data ----
      A_main  <- rbinom(n_main, 1, 0.5)
      x0_main <- rnorm(n_main, 0, sd = sqrt(4 / 3))
      x1_main <- rnorm(n_main, 0, sd = sqrt(2 / 3))
      
      a <- 1
      y0_main <- exp(x0_main / a) + rnorm(n_main, sd = sigma)
      y1_main <- 1 + exp(x1_main / a) + rnorm(n_main, sd = sigma)
      
      x_main <- A_main * x1_main + (1 - A_main) * x0_main
      Y_main <- A_main * y1_main + (1 - A_main) * y0_main    
      #plot(Y_main ~ x_main)
      
      # supp data ----
      A_supp  <- rbinom(n_supp, 1, 0.5)
      x0_supp <- rnorm(n_supp, 0, sd = sqrt(4 / 3))
      x1_supp <- rnorm(n_supp, 0, sd = sqrt(2 / 3))
      
      y0_supp <- exp(x0_supp / a) + rnorm(n_supp, sd = sigma)
      y1_supp <- (1 + offset) + exp(x1_supp / a) + rnorm(n_supp, sd = sigma)
      
      x_supp <- A_supp * x1_supp + (1 - A_supp) * x0_supp
      Y_supp <- A_supp * y1_supp + (1 - A_supp) * y0_supp    


      Y <- c(Y_main, Y_supp)
      X <- c(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ trt + X
      
      mte <- 1 # causal effect in main source
    }   else if (scenario == 5) {
      # scenario 5 ----
      # parallel linear response surfaces
      # multiple predictors associated with treatment,
      # including additional predictors not associated with
      # outcome
      
      # logistic regression parameters ----
      x_main_t <- matrix(rnorm(1e5 * 10), nrow = 1e5, ncol = 10)
      beta <- c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0, 0, 0) / 3
      pfun <- function(b0) {
        mean(plogis(c(b0 + x_main_t %*% beta))) - 0.5
      }
      #b1 <- 0.5
      b0 <- uniroot(pfun, c(-10, 10))$root

      # primary data ----
      # x_main <- rnorm(n_main)
      
      x_main <- matrix(rnorm(n_main * 10), nrow = n_main, ncol = 10)
      p_main <- c(plogis(b0 + x_main %*% beta))
      A_main  <- rbinom(n_main, 1, p_main)

      xi <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
      Y_main <- A_main + c(x_main %*% xi) + rnorm(n_main, sd = sigma) 
      # library(ggplot2)
      # ggplot(mapping = aes(x = x_main, y = Y_main, color = A_main)) +
      #   geom_point()
      # ggplot(mapping = aes(x = x_main, color = as.factor(A_main))) +
      #   geom_density()
        

      # supp data ----
      x_supp <- matrix(rnorm(n_supp * 10), nrow = n_supp, ncol = 10)
      p_supp <- c(plogis(b0 + x_supp %*% beta))
      A_supp  <- rbinom(n_supp, 1, p_supp)

      Y_supp <- (1 + offset) * A_supp + c(x_supp %*% xi) + rnorm(n_supp, sd = sigma) 


      Y <- c(Y_main, Y_supp)
      X <- rbind(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ .
      
      mte <- 1 # causal effect in main source
      
      dat <- as.data.frame(X)
      #dat <- as.data.frame(X)
      #dat$X <- X
      dat$Y <- Y
      dat$src <- source
      dat$trt <- A
      dat$src[dat$src == 0] <- "a"
      dat$src[dat$src == 1] <- "b"

      
    } else if (scenario == 6) {
      # scenario 6 ----
      # multiple covariates and interactions
      # logistic regression parameters ----
      x_main_t <- matrix(rnorm(1e5 * 3), nrow = 1e5, ncol = 3)
      beta <- c(1 / 3, 1 / 3, 1 / 3) / 2
      pfun <- function(b0) {
        mean(plogis(c(b0 + x_main_t %*% beta))) - 0.5
      }
      #b1 <- 0.5
      b0 <- uniroot(pfun, c(-10, 10))$root

      # primary data ----
      # x_main <- rnorm(n_main)
      
      x_main <- matrix(rnorm(n_main * 3), nrow = n_main, ncol = 3)
      colnames(x_main) <- paste0("x", 1:3)
      p_main <- c(plogis(b0 + x_main %*% beta))
      A_main  <- rbinom(n_main, 1, p_main)

      xd_main <- data.frame(trt = A_main)
      xd_main <- cbind(xd_main, x_main)
      xm_main <- model.matrix( ~ 0 + . ^ 4, xd_main)
      xi <- c(1, rep(1 / 4, ncol(xm_main) - 1))
      Y_main <- c(xm_main %*% xi) + rnorm(n_main, sd = sigma) 


      # supp data ----
      x_supp <- matrix(rnorm(n_supp * 3), nrow = n_supp, ncol = 3)
      colnames(x_supp) <- paste0("x", 1:3)
      p_supp <- c(plogis(b0 + x_supp %*% beta))
      A_supp  <- rbinom(n_supp, 1, p_supp)

      xd_supp <- data.frame(trt = A_supp)
      xd_supp <- cbind(xd_supp, x_supp)
      xm_supp <- model.matrix( ~ 0 + . ^ 4, xd_supp)
      xi <- c(1 + offset, rep(1 / 4, ncol(xm_supp) - 1))
      Y_supp <- c(xm_supp %*% xi) + rnorm(n_supp, sd = sigma) 


      Y <- c(Y_main, Y_supp)
      X <- rbind(x_main, x_supp)
      A <- c(A_main, A_supp)
      source <- rep(c(0, 1), c(n_main, n_supp))
      
      fm <- Y ~ .
      
      mte <- 1 # causal effect in main source
      
      dat <- as.data.frame(X)
      #dat <- as.data.frame(X)
      #dat$X <- X
      dat$Y <- Y
      dat$src <- source
      dat$trt <- A
      dat$src[dat$src == 0] <- "a"
      dat$src[dat$src == 1] <- "b"

      
    }

    if (data_only) {
      stop("'data_only = TRUE' is not implemented")
      # out <- list()
      # out$A <- as.character(c(A_main, A_supp))
      # out$X <- c(x_main, x_supp)
      # out$Y <- c(Y_main, Y_supp)
      # 
      # return(out)
    }
    



    # effect size
    #pooled_sd <- sqrt(((n_main - 1) * var(Y_main) + (n_supp - 1) * var(Y_supp)) / (n_main + n_supp - 2))

    #effect_size <- (mte - ste) / pooled_sd

    if(!exists("dat")) {
      dat <- data.frame(X = X)
      #dat <- as.data.frame(X)
      #dat$X <- X
      dat$Y <- Y
      dat$src <- source
      dat$trt <- A
      dat$src[dat$src == 0] <- "a"
      dat$src[dat$src == 1] <- "b"
    }
    
    # with(dat, tapply(X, A, summary))

    # ~~~~ marginal MEM ~~~~ ----
    suppressMessages(mm <- pate(Y ~ trt, 
      estimator      = "bayesian_lm", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100))
    # ~~~~ Bayesian LM ~~~~ ----
    # p <- (1 / 2) ^ ncol(attributes(terms(fm))$factors)
    p <- if (scenario < 5) {
      (1 / 2) ^ 2
    } else if (scenario == 5) {
      (1 / 2) ^ 10
    } else (1 / 2) ^ 3
    
    suppressMessages(bylm <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100,
      exch_prob      = p))
    # ~~~~ BART ~~~~ ----
    suppressMessages(bfit <- pate(fm, 
      estimator      = "BART", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100,
      theta          = 100))
    # ~~~~ Causal, no borrowing
    suppressMessages(cnb <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = subset(dat, src == "a"),
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100))
    
    

    (mm_error   <- mean(mm$pate_post) - mte)
    (bylm_error <- mean(bylm$pate_post) - mte)
    (bart_error <- mean(bfit$pate_post) - mte)
    (cnb_error   <- mean(cnb$pate_post) - mte)

    (mm_weight   <- mm$post_probs[1])
    (bylm_weight <- bylm$post_probs[1])
    (bart_weight <- bfit$post_probs[1])
    # don't need with for causal estimator without borrowing


    out <- data.frame(
      seed        = seed,
      n_main      = n_main,
      n_supp      = n_supp,
      scenario    = scenario,
      sigma       = sigma,
      ndost       = ndpost,
      offset      = offset,
      #effect_size = effect_size,
      bylm_weight = bylm_weight,
      bart_weight = bart_weight,
      bylm_error  = bylm_error,   
      bart_error  = bart_error)
    
    out$mm_weight <- mm_weight
    out$mm_error  <- mm_error
    out$cnb_error <- cnb_error
    
    out$bylm_esss <- var(bylm$mem_pate_post[, 2]) / var(bylm$mem_pate_post[, 1]) - 1
    out$bart_esss <- var(bfit$mem_pate_post[, 2]) / var(bfit$mem_pate_post[, 1]) - 1
    


  }, silent = TRUE)
  
  out

}


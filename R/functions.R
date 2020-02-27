# ~~~~ for debugging ~~~~ ----
# args <- list(
#   seed     = 1,
#   n_main   = 100,
#   n_supp   = 100,
#   sigma    = 1,    # sd for Y
#   scenario = 3,
#   offset   = - 1.75,
#   ndpost   = 1e2
# )
# 
# debug(main_sim)
# for(s in 1:6) {
#   args$scenario <- s
#   out <- do.call(main_sim, args)
#   print(out)
# }
# out <- do.call(main_sim, args)

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
      
      b1 <- 1 / 2
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
      
      b1 <- 1 / 2
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
      
      b1 <- 1 / 2
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
      beta <- c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0, 0, 0) / 2
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
    suppressMessages(blm <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100))
    # ~~~~ Bayesian LM power prior ~~~~ ----
    suppressMessages(blmp <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100, 
      model_prior    = "power"))
    # ~~~~ Bayesian LM power log prior ~~~~ ----
    suppressMessages(blmlogp <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100, 
      model_prior    = "powerlog"))
    # ~~~~ BART ~~~~ ----
    suppressMessages(bfit <- pate(fm,
      estimator      = "BART",
      data           = dat,
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100
      # , theta          = 100
      ))
    lml <- bfit$log_marg_like
    p <- ncol(dat) - 2
    # ~~~~ BART power ~~~~ ----
    # under (1/2) ^ p prior
    pe <- (1 / 2) ^ (p)
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartp_est <- mean(pate_post)
    bartp_weight <- unname(mp[1])
    # under (1/2) ^ (p / 2) prior
    pe <- if (p > 1) {
      (1 / 2) ^ (p / 2) 
    } else {
      1 / 2
    }
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartp2_est <- mean(pate_post)
    bartp2_weight <- unname(mp[1])

    # under (1/2) ^ log2(p) prior
    pe <- (1 / 2) ^ (log2(p))
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartlogp_est <- mean(pate_post)
    bartlogp_weight <- unname(mp[1])

    # suppressMessages(bartp <- pate(fm,
    #   estimator      = "BART",
    #   data           = dat,
    #   src_var        = "src",
    #   primary_source = "a",
    #   trt_var        = "trt",
    #   ndpost         = 100,
    #   model_prior    = "power"
    #   # , theta          = 100
    #   ))
    # ~~~~ Causal, no borrowing
    suppressMessages(nbblm <- pate(fm, 
      estimator      = "bayesian_lm", 
      data           = subset(dat, src == "a"),
      src_var        = "src",
      primary_source = "a",
      trt_var        = "trt",
      ndpost         = 100))
    
    

    (mm_error   <- mean(mm$pate_post) - mte)
    (blm_error <- mean(blm$pate_post) - mte)
    (blmp_error <- mean(blmp$pate_post) - mte)
    (blmlogp_error <- mean(blmlogp$pate_post) - mte)
    (bart_error <- mean(bfit$pate_post) - mte)
    (bartp_error <- bartp_est - mte)
    (bartp2_error <- bartp2_est - mte)
    (bartlogp_error <- bartlogp_est - mte)
    (nbblm_error   <- mean(nbblm$pate_post) - mte)

    (mm_weight   <- mm$post_probs[1])
    (blm_weight <- blm$post_probs[1])
    (blmp_weight <- blmp$post_probs[1])
    (blmlogp_weight <- blmlogp$post_probs[1])
    (bart_weight <- bfit$post_probs[1])
    # (bartp_weight <- bartp$post_probs[1])
    # (bartp2_weight <- bartp2$post_probs[1])
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
      blm_weight = blm_weight,
      blmp_weight = blmp_weight,
      blmlogp_weight = blmlogp_weight,
      bart_weight = bart_weight,
      bartp_weight = bartp_weight,
      bartp2_weight = bartp2_weight,
      bartlogp_weight = bartlogp_weight,
      blm_error  = blm_error,
      blmp_error  = blmp_error,
      blmlogp_error = blmlogp_error
      , bart_error  = bart_error
      , bartp_error  = bartp_error
      , bartp2_error  = bartp2_error
      , bartlogp_error = bartlogp_error
      )
    
    out$mm_weight <- mm_weight
    out$mm_error  <- mm_error
    out$nbblm_error <- nbblm_error
    
    # out$blm_esss <- var(blm$mem_pate_post[, 2]) / var(blm$mem_pate_post[, 1]) - 1
    # out$bart_esss <- var(bfit$mem_pate_post[, 2]) / var(bfit$mem_pate_post[, 1]) - 1
    


  }, silent = FALSE)
  
  out

}

# for debugging
# scenario <- 1
# seed     <- 2
# n        <- 100
# varying  <- "intercept"
# args <- list(
#   scenario = 1,
#   seed     = 1,
#   delta    = -2.5,
#   varying  = "intercept"
# )
  # library(aciccomp2016)
  # library(glmnet)
  # library(borrowr)

# debugonce(acic_sim)
# system.time(out <- do.call(acic_sim, args))

# ACIC ----
# library(borrowr)
# library(aciccomp2016)
# library(glmnet)
# scenario <- 11
# seed <- 1
# varying <- "treatment"
# delta <- -2.25
# debugonce(acic_sim)
# oo <- acic_sim(scenario = scenario, seed = seed, varying = varying, delta = delta)
# oo <- do.call(acic_sim, args[1, , drop = TRUE])

# BLM uses lasso regression for variable selection
acic_sim <- function(scenario, seed, n = 100, delta,
  varying = c("intercept", "treatment")) {
  # require(aciccomp2016)
  # require(glmnet)
  # require(borrowr)
  out <- NULL
  varying = match.arg(varying)
  # require(aciccomp2016)
  # require(borrowr)
  # generate data
  sim <- dgp_2016(input_2016, scenario, seed)
  set.seed(seed)
  # randomly sample n * 2 treated, n * 2 untreated
  con_indx <- which(sim$z == 0)
  trt_indx <- which(sim$z == 1)
  # the first 100 in each will be primary source
  con_keepers <- sort(sample(con_indx, n * 2))
  trt_keepers <- sort(sample(trt_indx, n * 2))
  # get SATE for primary source
  # po_diffs <- (sim$y.1 - sim$y.0)[c(con_keepers, trt_keepers)]
  po_diffs <- (sim$y.1 - sim$y.0)[c(con_keepers[seq_len(n)], 
    trt_keepers[seq_len(n)])]
  # SATE <- mean(po_diffs[c(con_keepers[seq_len(n)],
  #   trt_keepers[seq_len(n)])])
  SATE <- mean(po_diffs)

  # want to vary either treatment or intercept
  # want to loop over a range of effect sizes
  dd <- input_2016[c(con_keepers, trt_keepers), ]
  dd$y <- sim$y[c(con_keepers, trt_keepers)]
  dd$trt <- rep(c(0, 1), c(n * 2, n * 2))
  dd$src <- rep(c("prim", "supp", "prim", "supp"), rep(n, 4))
  sdy <- sd(dd$y)
  if (varying == "intercept") {
    td <- dplyr::mutate(dd,
      y = y * (src == 'prim') + (sdy * delta + y) * (src == 'supp'))
    with(td, tapply(y, src, summary))
  } else if (varying == "treatment") {
    td <- dplyr::mutate(dd,
      y = y * (src == 'prim' | trt == 0) + (sdy * delta + y) * (src == 'supp' & trt == 1))
  }
  try({
    # BLM with variable selection ----
    mm <- td[, -match("y", names(td))]
    mm <- model.matrix(~ 0 + ., data = mm)
    # eliminate multicolinearity
    # lasso <- cv.glmnet(x = mm[td$src == "prim", ],
    #     y           = td$y[td$src == "prim"],
    #     family      = "gaussian",
    #     alpha       = 1,
    #     standardize = TRUE)
    lasso <- cv.glmnet(x = mm,
        y           = td$y,
        family      = "gaussian",
        alpha       = 1,
        standardize = TRUE)
    rn <- rownames(coef(lasso))[as.numeric(coef(lasso)) != 0]
    rn <- setdiff(rn, c("(Intercept)", "srcsupp", "trt"))  
    
    if (length(rn) == 0) {
      # mm$y <- dd$y
      mm <- td[, 'y', drop = FALSE]
      mm$src <- dd$src
      mm$trt <- dd$trt
    } else {
      # new ----
      mm <- as.data.frame(mm)
      tm <- mm[c(rn, "srcsupp")]
      ag <- aggregate(tm, list(tm$srcsupp), sd)
      zsd <- apply(ag[, -1], 2, function(x) any(x == 0))
      zsd <- zsd[-match("srcsupp", names(zsd))]
      rn <- rn[!zsd]
      mm <- mm[rn]
      # new ----
      mm$y <- td$y
      mm$src <- dd$src
      mm$trt <- dd$trt
    }
    # bayesian lm with power prior and variable selection
    blmpvs         <- NA
    blmpvs_weight  <- NA
    blmpvs_est     <- NA
    try ({
      suppressMessages(blmpvs <- pate(y ~ . * trt,
        estimator      = "bayesian_lm",
        data           = mm,
        src_var        = "src",
        primary_source = "prim",
        trt_var        = "trt",
        ndpost         = 100,
        model_prior    = "power"))
      blmpvs_weight  <- blmpvs$post_probs[1]
      blmpvs_est     <- mean(blmpvs$pate_post)
    }, silent = TRUE)
    nbblmpvs        <- NA
    nbblmpvs_est    <- NA
    try({
      suppressMessages(nbblmpvs <- pate(y ~ . * trt,
        estimator      = "bayesian_lm",
        data           = subset(mm, src == "prim"),
        src_var        = "src",
        primary_source = "prim",
        trt_var        = "trt",
        ndpost         = 100,
        model_prior    = "power"))
      nbblmpvs_est <- mean(nbblmpvs$pate_post)
    }, silent = TRUE)
    # BLM with no variable selection ----
    # no borrowing, BLM
    mm <- td[, -match("y", names(td))]
    mm <- model.matrix(~ 0 + ., data = mm)
    mm <- as.data.frame(mm)
    tm <- mm
    ag <- aggregate(tm, list(tm$srcsupp), sd)
    zsd <- apply(ag[, -1], 2, function(x) any(x == 0))
    #zsd <- zsd[-match("srcsupp", names(zsd))]
    #rn <- rn[!zsd]
    mm <- mm[names(zsd)[!zsd]]
    mm$y <- td$y
    mm$src <- dd$src
    mm$trt <- dd$trt
    
    blmp         <- NA
    blmp_weight  <- NA
    blmp_est     <- NA
    try({
      suppressMessages(blmp <- pate(y ~ .,
        estimator      = "bayesian_lm",
        data           = mm,
        src_var        = "src",
        primary_source = "prim",
        trt_var        = "trt",
        ndpost         = 100,
        model_prior    = "power"))
      blmp_weight  <- blmp$post_probs[1]
      blmp_est <- mean(blmp$pate_post)
    }, silent = TRUE)

    nbblm     <- NA
    nbblm_est <- NA
    try({
      suppressMessages(nbblm <- pate(y ~ .,
        estimator      = "bayesian_lm",
        data           = subset(mm, src == "prim"),
        src_var        = "src",
        primary_source = "prim",
        trt_var        = "trt",
        ndpost         = 100,
        model_prior    = "power"))
      nbblm_est      <- mean(nbblm$pate_post)    
    }, silent = TRUE)
    
    td <- model.matrix(~ 0 + ., data = td)
    td <- as.data.frame(td)
    td$srcsupp <- NULL
    td$src <- dd$src
    
    suppressMessages(bfit <- pate(y ~ .,
      estimator      = "BART",
      data           = td,
      src_var        = "src",
      primary_source = "prim",
      trt_var        = "trt",
      ndpost         = 100
      # , theta          = 100
      ))
    lml <- bfit$log_marg_like
    p <- ncol(td) - 2
    # under (1/2) ^ log2p prior
    pe <- (1 / 2) ^ log2(p)
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartlogp_est <- mean(pate_post)
    bartlogp_weight <- unname(mp[1])
    # under (1/2) ^ p prior
    pe <- (1 / 2) ^ (p)
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartp_est <- mean(pate_post)
    bartp_weight <- unname(mp[1])
    # under (1/2) ^ (p / 2) prior
    pe <- (1 / 2) ^ (p / 2)
    pp <- c(pe, 1 - pe)
    cons <- -max(lml)
    mp <- exp(cons + lml) * pp / sum(exp(cons + lml) * pp) 
    pate_post <- sample_posterior(bfit$mem_pate_post, mp)
    bartp2_est <- mean(pate_post)
    bartp2_weight <- unname(mp[1])
    # suppressMessages(bartlogp <- pate(y ~ .,
    #   estimator      = "BART",
    #   data           = td,
    #   src_var        = "src",
    #   primary_source = "prim",
    #   trt_var        = "trt",
    #   ndpost         = 100,
    #   model_prior    = "powerlog"
    #   # , theta          = 100
    #   ))
    # --- no borrowing ----
    nbbart_est <- mean(bfit$mem_pate_post[, 2])
    # suppressMessages(nbbart <- pate(y ~ .,
    #   estimator      = "BART",
    #   data           = subset(td, src == "prim"),
    #   src_var        = "src",
    #   primary_source = "prim",
    #   trt_var        = "trt",
    #   ndpost         = 100
    #   # , theta          = 100
    #   ))
    pars <- parameters_2016[scenario, ]

    bart_weight     <- bfit$post_probs[1]
    # bartp2_weight     <- bartlogp$post_probs[1]

    bart_est     <- mean(bfit$pate_post)
    # bartp2_est    <- mean(bartlogp$pate_post)
    # nbbart_est      <- mean(nbbart$pate_post)
    out <- data.frame(scenario, seed, varying,
      delta, pars, nbbart_est, nbblm_est, nbblmpvs_est, blmpvs_est, 
      blmpvs_weight, blmp_est, blmp_weight, bart_est, bart_weight, 
      bartp_est, bartp_weight, bartp2_est, bartp2_weight, 
      bartlogp_est, bartlogp_weight,
      SATE)
    outfile <- sprintf("out-%s-%s.txt", 
      Sys.info()['nodename'], 
      Sys.getpid())
    filepath <- sprintf("../outfiles/acic/%s", outfile)
    fe <- outfile %in% dir("../outfiles/acic/")
    write.table(out,
      file      = filepath,
      row.names = FALSE, 
      col.names = !fe,
      quote     = FALSE,
      append    = fe)
  })
  out
}


# ACIC plot function ----
acic_plot <- function(data, title) {
  bdata <- filter(data, estimator == "bart")
  ldata <- filter(data, estimator == "blmpvs")
  ndata <- filter(data, estimator == "nb")
  delta <- bdata$delta
  # ~~~ weight ----
  par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
  plot(bdata$weight ~ delta, type = 'l', ylim = c(0, 1),
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Posterior Weight", cex = axis_size),
    col = cols[1],
    lwd = line_size,
    main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
    axes = FALSE)
  lines(ldata$weight ~ delta, 
    col = cols[match("blmpvs", estimators)],
    lwd = line_size)
  lines(ndata$weight ~ delta, 
    col = cols[match("nbblm", estimators)],
    lwd = line_size)
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)

  # ~~~ bias ----
  bias_range <- c(-1, 1) * ceiling(max(abs(data$bias)))
  plot(bdata$bias ~ delta, type = 'l', 
    ylim = bias_range,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Bias", cex = axis_size),
    col = cols[1],
    lwd = line_size,
    main = list("Estimated Bias", cex = title_size),
    axes = FALSE)
  lines(ldata$bias ~ delta, 
    col = cols[match("blmpvs", estimators)],
    lwd = line_size)
  lines(ndata$bias ~ delta, 
    col = cols[match("nbblm", estimators)],
    lwd = line_size)
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  legend_loc <- "topleft"
  legend(legend_loc,  
    legend = elabs[match(c("bart", "blmpvs", "nbblm"), estimators)],
    col = cols[match(c("bart", "blmpvs", "nbblm"), estimators)],
    lwd = line_size,
    bty = "n",
    cex = 1.2)

  # ~~~ MSE ----
  # rmse_range <- c(0, ceiling(max(data$rmse)))
  rmse_range <- c(min(data$rmse), ceiling(max(data$rmse)))
  plot(bdata$rmse ~ delta, type = 'l', 
    ylim = rmse_range,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Root MSE", cex = axis_size),
    col = cols[1],
    lwd = line_size,
    main = list("Estimated Root MSE", cex = title_size),
    axes = FALSE)
  lines(ldata$rmse ~ delta, 
    col = cols[match("blmpvs", estimators)],
    lwd = line_size)
  lines(ndata$rmse ~ delta, 
    col = cols[match("nbblm", estimators)],
    lwd = line_size)
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  mtext(title, cex = 1.5, outer = TRUE)
}

ratio_plot <- function(data) {
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  w0 <- group_by(data, estimator) %>%
    filter(weight == min(weight)) %>%
    rename(
      bl_rmse = rmse,
      bl_var  = var) %>%
    select(estimator, bl_rmse, bl_var)
  data <- data %>% left_join(w0) %>%
    mutate(
      mse_ratio = (rmse / bl_rmse) ^ 2,
      var_ratio = var / bl_var)
  bdata <- filter(data, estimator == "bart")
  ldata <- filter(data, estimator == "blmpvs")
  delta <- bdata$delta
  # mse ratio plot ----
  rr <- c(0, max(c(bdata$mse_ratio, ldata$mse_ratio)))
  plot(bdata$mse_ratio ~ delta, type = 'l', 
    ylim = rr,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("MSE Ratio", cex = axis_size),
    col = cols[1],
    lwd = line_size,
    main = list("Estimated MSE Ratio", cex = title_size),
    axes = FALSE)
  lines(ldata$mse_ratio ~ delta, 
    col = cols[match("blmpvs", estimators)],
    lwd = line_size)
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  # abline(h = 1 /2)
  lines(x = delta, y = rep(1 / 2, length(delta)),
    lty = 2)
  legend_loc <- "topleft"
  legend(legend_loc,  
    legend = elabs[match(c("bart", "blmpvs"), estimators)],
    col = cols[match(c("bart", "blmpvs"), estimators)],
    lwd = line_size,
    bty = "n",
    cex = 1.2)
  # var ratio plot ----
  rr <- c(0, max(c(bdata$var_ratio, ldata$var_ratio)))
  plot(bdata$var_ratio ~ delta, type = 'l', 
    ylim = rr,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Var Ratio", cex = axis_size),
    col = cols[1],
    lwd = line_size,
    main = list("Estimated Var Ratio", cex = title_size),
    axes = FALSE)
  lines(ldata$var_ratio ~ delta, 
    col = cols[match("blmpvs", estimators)],
    lwd = line_size)
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  # abline(h = 1 /2)
  lines(x = delta, y = rep(1 / 2, length(delta)),
    lty = 2)
}

sample_posterior <- function(posts, probs) {

  n_post <- nrow(posts)
  n_mems <- ncol(posts)

  row_select <- seq_len(n_post)
  col_select <- sample(seq_len(n_mems), n_post, replace = TRUE, prob = probs)

  out <- posts[cbind(row_select, col_select)]
  out

}

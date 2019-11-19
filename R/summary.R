# load("./RData/sim_out.RData")

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# message("update to 1:5?")
file_list <- paste0("./outfiles/out_", (1:5), ".txt") 
rr_list   <- lapply(file_list, read.table, header = TRUE)

rr <- do.call(rbind, rr_list)

# rr <- read.table("../outfiles/sim_out.txt", header = TRUE)
rr <- as_tibble(rr)

# ------------------------- #
# --- find missing rows --- #
# ------------------------- #
args <- expand.grid(
  seed              = 1:10,
  n_main            = c(50, 100),
  sigma             = 1,
  scenario          = 9:11,
  offset            = seq(-8, 8, 1),
  ndpost            = 100,
  prior_calibration = "none"
)
 
args <- as_tibble(args)

setdiff(names(args), names(rr))
args <- args %>% select(-ndpost)
which_serv_args <- rep(seq_len(5), nrow(args) / 5)
# warning("update to include all servers?")
# args <- args[which_serv_args %in% 3:5, ]
missing_args <- anti_join(args, rr)



# ------------------------------ #


rr <- mutate(rr, 
  n_main = as.character(n_main) #,
  # n_tree = as.character(n_tree)
)


mm <- rr %>%
  # mutate(effect_size = offset / sigma) %>%
  select(-n_supp, -sigma, -ndost, -contains("esss")) %>%
  gather(contains("weight"), contains("error"), key = est_est, value = value) %>%
  separate(est_est, into = c("estimator", "estimand")) %>%
  spread(key = estimand, value = value) %>%
  group_by(scenario, n_main, offset, estimator) %>%
  summarize(
    bias = mean(error),
    mse  = mean(error ^ 2),
    weight = mean(weight)
  ) %>%
  # arrange(scenario, estimator, effect_size)
  ungroup() %>%
  arrange(scenario, estimator, offset)



# ~ choose font size ~ ----
font_size <- 20



scens <- unique(rr$scenario)




# plot parameters ----


axis_size <- 2
# title_size <- 1.75
main_size <- 2.5
line_size <- 3
axis_title_size <- 2.5

# weights <- filter(weights, n_main == "100")
# bias    <- filter(bias,    n_main == "100")
# mse     <- filter(mse,     n_main == "100")
# esss    <- filter(esss,    n_main == "100")

# mag <- 1.25

# plots for paper ----
for(scen in scens) {

  # revision
  pdf(sprintf("./plots/scenario-%s.pdf", scen), height = 6, width = 18)
  # par(cex.axis = mag, cex.lab = mag, cex.main = mag)
  par(mfrow = c(1, 3), mar = c(6, 7, 4, 2) + 0.1)
  # ~ weights plot ----
  bart <- filter(mm, estimator == "bart", scenario == scen)
  bylm <- filter(mm, estimator == "bylm", scenario == scen)
  mem  <- filter(mm, estimator == "mm",   scenario == scen)
  cnb  <- filter(mm, estimator == "cnb",  scenario == scen)
  plot(bart$weight ~ bart$offset, type = 'l', ylim = c(0, 1),
    # xlab = list(expression(delta), cex = axis_title_size),
    # ylab = list("Posterior Weight", cex = axis_title_size),
    col = "steelblue2",
    lwd = line_size,
    # main = list("Posterior Weight in Favor of Borrowing", cex = main_size),
    axes = FALSE,
    ann = FALSE)
  # legend("topleft", legend = c("BART", "BLM", "No Causal", "No Borrowing"),
  #   col = c("steelblue2", "seagreen3", "gold", "salmon"),
  #   lwd = 2,
  #   bty = "n")
  axis(1, cex.axis = axis_size)
  axis(2, las = 1, cex.axis = axis_size)
  title(xlab = expression(delta), line = 4, cex.lab = axis_title_size)
  title(ylab = "Posterior Weight", line = 4, cex.lab = axis_title_size)
  title(main = "Posterior Weight in Favor of Borrowing", cex.main = main_size)
  lines(bylm$weight ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  lines(mem$weight ~ mem$offset,
    col = "gold",
    lwd = line_size)
  # dev.off()

  # ~ bias plot ----
  # pdf(sprintf("./plots/scenario-%s-bias.pdf", scen), height = 6, width = 6)
  # par(cex.axis = mag, cex.lab = mag, cex.main = mag)
  # dd <- filter(bias, estimator != "MEM", scenario == scen)
  dd <- filter(mm, scenario == scen)
  bias_range <- c(-1, 1) * ceiling(max(abs(range(dd$bias))))
  # bias_range <- c(-1, 1) * max(abs(range(dd$bias)))

  # bart <- filter(bias, estimator == "BART", scenario == scen)
  # bylm <- filter(bias, estimator == "BYLM", scenario == scen)
  plot(bart$bias ~ bart$offset, type = 'l', 
    ylim = bias_range,
    xlab = list(expression(delta), cex = axis_title_size),
    ylab = list("Bias", cex = axis_title_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Estimated Bias", cex = main_size),
    axes = FALSE)
  axis(1, cex.axis = axis_size)
  axis(2, las = 1, at = seq(min(bias_range), max(bias_range), 1), 
    cex.axis = axis_size)
  # axis(2, las = 1)
  lines(bylm$bias ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  lines(mem$bias ~ cnb$offset,
    col = "gold",
    lwd = line_size)
  lines(cnb$bias ~ cnb$offset,
    col = "salmon",
    lwd = line_size)
  # legend_loc <- ifelse(scen %in% c(2, 3), "bottomleft", "topleft")
  legend_loc <- "topleft"
  legend(legend_loc,  
    legend = c("BART", "BLM", "No Causal", "No Borrowing"),
    col = c("steelblue2", "seagreen3", "gold", "salmon"),
    lwd = 2,
    bty = "n")
  # lines(x = range(dd$offset), y = c(0, 0), lty = 2, 
  #   lwd = 2,
  #   col = gray(3/4))
  # dev.off()

  # ~ MSE plot ----
  # pdf(sprintf("./plots/scenario-%s-mse.pdf", scen), height = 6, width = 6)
  # par(cex.axis = mag, cex.lab = mag, cex.main = mag)
  # dd <- filter(mse, estimator != "MEM", scenario == scen)
  # dd <- filter(mm, scenario == scen)
  mse_range <- c(0, max(dd$mse))

  # bart <- filter(mse, estimator == "BART", scenario == scen)
  # bylm <- filter(mse, estimator == "BYLM", scenario == scen)
  plot(bart$mse ~ bart$offset, type = 'l', 
    ylim = mse_range,
    xlab = list(expression(delta), cex = axis_title_size),
    ylab = list("MSE", cex = axis_title_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Estimated MSE", cex = main_size),
    axes = FALSE)
  axis(1, cex.axis = axis_size)
  axis(2, las = 1, cex.axis = axis_size)
  lines(bylm$mse ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  lines(mem$mse ~ mem$offset,
    col = "gold",
    lwd = line_size)
  lines(cnb$mse ~ cnb$offset,
    col = "salmon",
    lwd = line_size)
  # dev.off()
  
  dev.off()
}


# marginal MEM bias at delta = 0 ----
# bias %>% 
#   filter(estimator == "MEM", offset == 0, n_main == "100")

mm %>% filter(offset == 0, estimator == "mm") %>%
  print(n = Inf)


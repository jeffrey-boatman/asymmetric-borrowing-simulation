# load("./RData/sim_out.RData")

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

message("update to 1:5?")
file_list <- paste0("../outfiles/out_", (1:5)[c(3,5)], ".txt") 
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

# outlier check ----
# rr %>%
#   filter(n_main == "50") %>%
#   ggplot(mapping = aes(x = bart_error)) +
#   geom_histogram() +
#   facet_wrap( ~ offset)
# rr %>%
#   filter(n_main == "100") %>%
#   ggplot(mapping = aes(x = bart_error)) +
#   geom_histogram() +
#   facet_wrap( ~ offset)
# rr %>%
#   filter(n_main == "200") %>%
#   ggplot(mapping = aes(x = bart_error)) +
#   geom_histogram() +
#   facet_wrap( ~ offset)


mm <- rr %>%
  group_by(scenario, n_main, offset) %>%
  summarize(
    # effect_size = mean(effect_size),
    effect_size = mean(offset / sigma),
    MEM_weight  = mean(mm_weight),
    BYLM_weight = mean(bylm_weight),
    BART_weight = mean(bart_weight),
    MEM_bias    = mean(mm_error),
    BYLM_bias   = mean(bylm_error),
    BART_bias   = mean(bart_error),
    MEM_mse     = mean(mm_error ^ 2),
    BYLM_mse    = mean(bylm_error ^ 2),
    BART_mse    = mean(bart_error ^ 2),
    BYLM_esss   = mean(bylm_esss),
    BART_esss   = mean(bart_esss)
  )

# ~ choose font size ~ ----
font_size <- 20

# ~ summary plots ~ ----

ests <- mm %>%
  gather(-scenario, -n_main, -offset, -effect_size,
    key = thing, value = value) %>%
  separate(thing, into = c("estimator", "estimand")) %>%
  arrange(effect_size)

# ~~~~ weights computation ~~~~ ----
# weights <- mm %>%
#   select(-contains("bias"), -contains("mse")) %>%
#   gather(-scenario, -n_main, -offset, -effect_size,  
#     key = estimator, value = weight) %>%
#   separate(estimator, into = c("estimator", "junk")) %>%
#   select(-junk) %>%
#   arrange(effect_size)

(weights <- filter(ests, estimand == "weight"))

# ~~~~ bias computation ~~~~ ----
# bias <- mm %>%
#   select(-contains("weight"), -contains("mse")) %>%
#   gather(-scenario, -n_main, -offset, -effect_size,  
#     key = estimator, value = bias) %>%
#   separate(estimator, into = c("estimator", "junk")) %>%
#   select(-junk) %>%
#   arrange(effect_size)

(bias <- filter(ests, estimand == "bias"))

# ~~~~ mse computaion ~~~~ ----
# mse <- mm %>%
#   select(-contains("weight"), -contains("bias")) %>%
#   gather(-scenario, -n_main, -offset, -effect_size,  
#     key = estimator, value = mse) %>%
#   separate(estimator, into = c("estimator", "junk")) %>%
#   select(-junk) %>%
#   arrange(effect_size)

(mse <- filter(ests, estimand == "mse"))

(esss <- filter(ests, estimand == "esss"))


scens <- unique(rr$scenario)

# plots for me ----
for(scen in scens) {
  wplot <- weights %>% 
    filter(scenario == scen) %>% 
    ggplot(mapping = aes(x = effect_size, y = value, color = n_main)) + 
    geom_path() +
    theme_classic() + 
    facet_wrap(~ estimator) +
    theme(text = element_text(size = font_size)) +
    labs(x = expression(gamma / sigma)) +
    ggtitle(paste0("Scenario ", scen))

  bplot <- bias %>%
    filter(scenario == scen) %>%
    ggplot(mapping = aes(x = effect_size, y = value, color = n_main)) +
    geom_path() +
    facet_wrap(~ estimator) +
    theme_classic() +
    theme(text = element_text(size = font_size)) +
    labs(x = expression(gamma / sigma))

  mplot <- mse %>%
    filter(scenario == scen) %>%
    ggplot(mapping = aes(x = effect_size, y = value, color = n_main)) +
    geom_path() +
    facet_wrap(~ estimator) +
    theme_classic() +
    theme(text = element_text(size = font_size)) +
    labs(x = expression(gamma / sigma))

  pdf(sprintf("../plots/summary-plots-scenario-%s.pdf", scen), height = 30, width = 20)
  gridExtra::grid.arrange(wplot, bplot, mplot, ncol = 1)
  dev.off()
}



# plots for paper ----

axis_size <- 1.5
title_size <- 1.5
line_size <- 2

weights <- filter(weights, n_main == "100")
bias    <- filter(bias,    n_main == "100")
mse     <- filter(mse,     n_main == "100")
esss    <- filter(esss,    n_main == "100")

for(scen in scens) {
  pdf(sprintf("../plots/scenario-%s-weights.pdf", scen), height = 6, width = 6)
  # ~ weights plot ----
  bart <- filter(weights, estimator == "BART", scenario == scen)
  bylm <- filter(weights, estimator == "BYLM", scenario == scen)
  plot(bart$value ~ bart$offset, type = 'l', ylim = c(0, 1),
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Posterior Weight", cex = axis_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
    axes = FALSE)
  legend("topleft", legend = c("BART", "BLM"),
    col = c("steelblue2", "seagreen3"),
    lwd = 2,
    bty = "n")
  axis(1)
  axis(2, las = 1)
  lines(bylm$value ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  dev.off()

  # ~ bias plot ----
  pdf(sprintf("../plots/scenario-%s-bias.pdf", scen), height = 6, width = 6)
  dd <- filter(bias, estimator != "MEM", scenario == scen)
  bias_range <- c(-1, 1) * ceiling(max(abs(range(dd$value))))
  # bias_range <- c(-1, 1) * max(abs(range(dd$bias)))

  bart <- filter(bias, estimator == "BART", scenario == scen)
  bylm <- filter(bias, estimator == "BYLM", scenario == scen)
  plot(bart$value ~ bart$offset, type = 'l', 
    ylim = bias_range,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("Bias", cex = axis_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Estimated Bias", cex = title_size),
    axes = FALSE)
  axis(1)
  axis(2, las = 1, at = seq(min(bias_range), max(bias_range), 1))
  # axis(2, las = 1)
  lines(bylm$value ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  lines(x = range(dd$offset), y = c(0, 0), lty = 2, 
    lwd = 2,
    col = gray(3/4))
  dev.off()

  # ~ MSE plot ----
  pdf(sprintf("../plots/scenario-%s-mse.pdf", scen), height = 6, width = 6)
  dd <- filter(mse, estimator != "MEM", scenario == scen)
  mse_range <- c(0, max(dd$value))

  bart <- filter(mse, estimator == "BART", scenario == scen)
  bylm <- filter(mse, estimator == "BYLM", scenario == scen)
  plot(bart$value ~ bart$offset, type = 'l', 
    ylim = mse_range,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("MSE", cex = axis_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Estimated MSE", cex = title_size),
    axes = FALSE)
  axis(1)
  axis(2, las = 1)
  lines(bylm$value ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  dev.off()
  
  # ~ esss plot ----
  pdf(sprintf("../plots/scenario-%s-esss.pdf", scen), height = 6, width = 6)
  dd <- filter(esss, estimator != "MEM", scenario == scen)
  esss_range <- range(dd$value * 100)

  bart <- filter(esss, estimator == "BART", scenario == scen)
  bylm <- filter(esss, estimator == "BYLM", scenario == scen)
  plot(bart$value * 100 ~ bart$offset, type = 'l', 
    ylim = esss_range,
    xlab = list(expression(delta), cex = axis_size),
    ylab = list("ESSS(%)", cex = axis_size),
    col = "steelblue2",
    lwd = line_size,
    main = list("Estimated ESSS(%)", cex = title_size),
    axes = FALSE)
  axis(1)
  axis(2, las = 1)
  lines(bylm$value * 100 ~ bylm$offset,
    col = "seagreen3",
    lwd = line_size)
  dev.off()

}

# marginal MEM bias at delta = 0 ----
bias %>% 
  filter(estimator == "MEM", offset == 0, n_main == "100")


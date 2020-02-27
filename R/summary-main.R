# load("./RData/sim_out.RData")

# primary sim ----
rm(list = ls())

source("R/summary-pars.R")

# source("R/functions.R")

library(tidyverse)

# message("update to 1:5?")
file_list <- paste0("./outfiles/main/main_out_", (1:5), ".txt") 
rr_list   <- lapply(file_list, read.table, header = TRUE)

rr <- do.call(rbind, rr_list)

# rr <- read.table("../outfiles/sim_out.txt", header = TRUE)
rr <- as_tibble(rr)

# msg <- "Verify that the scenarios are coded correctly in the plots!"
# # scenarios 1, 3, and 5 are re-coded as 1, 2, 3.
# warning(msg)


# ------------------------- #
# --- find missing rows --- #
# ------------------------- #
args <- expand.grid(
  seed              = 1:1000,
  n_main            = 100,
  sigma             = 1,
  scenario          = 1:6,
  offset            = seq(-2.5, 2.5, 1 / 2),
  ndpost            = 100
)

args <- as_tibble(args)

setdiff(names(args), names(rr))
args <- args %>% select(-ndpost)
# which_serv_args <- rep(seq_len(5), nrow(args) / 5)
# warning("update to include all servers?")
# args <- args[which_serv_args %in% 3:5, ]
missing_args <- anti_join(args, rr)
message('Number of missing rows: ', nrow(missing_args))


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
    rmse  = sqrt(mean(error ^ 2)),
    weight = mean(weight)
  ) %>%
  # arrange(scenario, estimator, effect_size)
  ungroup() %>%
  arrange(scenario, estimator, offset)

    

# chose estimators for plotting
# ests <- unique(mm$estimator)
# ests <- setdiff(ests, "mm")

ests <- c("nbblm", "bart", "bartp", "blm", "blmp")
ltys <- c(1, 1, 6, 1, 6)
scens <- unique(rr$scenario)

# combined plots ----

my_round <- function(x) {
  sign <- sign(x)
  x <- sign * x
  sign * ceiling(x * 2) / 2
}

# ~ pdfs ----
for (scen in scens) {
  pdf(sprintf("plots/main-combined/scenario-%s.pdf", scen), 
    height = 10 / 3, width = 10)
  par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
  # ~~ weight ----
  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(weight ~ offset, type = 'l', ylim = c(0, 1),
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Posterior Weight", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      lty = 1,
      main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(weight ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)

  # ~~ bias ----
  # bias_range <- c(- 1, 1) * ceiling(max(abs(range(filter(mm, 
  #   scenario == scen, estimator %in% ests)$bias))))
  # bias_range <- c(- 1, 1) * ceiling(max(abs(range(filter(mm, 
  #   scenario == scen, estimator %in% ests)$bias))))
  bias_range <- range(filter(mm, 
    scenario == scen, estimator %in% ests)$bias)
  bias_range <- my_round(bias_range)

  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(bias ~ offset, type = 'l',
      ylim = bias_range,
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Bias", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      main = list("Estimated Bias", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(bias ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
    # ~~ legend ----
  legend_loc <- "topleft"
  legend(legend_loc,  
    legend = elabs[match(ests, estimators)],
    col = main_cols[match(ests, estimators)],
    lwd = 2,
    lty = ltys,
    bty = "n",
    cex = 1.2)
  
  # ~~ MSE ----
  mse_range <- c(0, max(filter(mm, scenario == scen,
    estimator %in% ests)$rmse))
  # mse_range <- range(filter(mm, scenario == scen, 
  #   estimator %in% ests)$rmse)
  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(rmse ~ offset, type = 'l', 
      ylim = mse_range,
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Root MSE", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      main = list("Estimated Root MSE", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(rmse ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  # uncomment to add tile overall 3 plots
  # mtext(sprintf("Part 1, Scenario %s", scen), cex = 1.5, outer = TRUE)
  dev.off()
}

# ~ jpegs ----
for (scen in scens) {
  jpeg(sprintf("plots/combined/scenario-%s.jpeg", scen), 
    height = 10 / 3, width = 10, units = "in", 
    res = 72 * 4,
    quality = 100)
  par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
  # ~~ weight ----
  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(weight ~ offset, type = 'l', ylim = c(0, 1),
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Posterior Weight", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      lty = 1,
      main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(weight ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)

  # ~~ bias ----
  # bias_range <- c(- 1, 1) * ceiling(max(abs(range(filter(mm, 
  #   scenario == scen, estimator %in% ests)$bias))))
  bias_range <- c(- 1, 1) * ceiling(max(abs(range(filter(mm, 
    scenario == scen, estimator %in% ests)$bias))))

  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(bias ~ offset, type = 'l',
      ylim = bias_range,
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Bias", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      main = list("Estimated Bias", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(bias ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
    # ~~ legend ----
  legend_loc <- "topleft"
  legend(legend_loc,  
    legend = elabs[match(ests, estimators)],
    col = main_cols[match(ests, estimators)],
    lwd = 2,
    lty = ltys,
    bty = "n",
    cex = 1.2)
  
  # ~~ MSE ----
  mse_range <- c(0, max(filter(mm, scenario == scen,
    estimator %in% ests)$rmse))
  # mse_range <- range(filter(mm, scenario == scen, 
  #   estimator %in% ests)$rmse)
  with(filter(mm, estimator == ests[1], scenario == scen), {
    plot(rmse ~ offset, type = 'l', 
      ylim = mse_range,
      xlab = list(expression(delta), cex = axis_size),
      ylab = list("Root MSE", cex = axis_size),
      col = main_cols[match(ests[1], estimators)],
      lwd = line_size,
      main = list("Estimated Root MSE", cex = title_size),
      axes = FALSE)
  })
  for (idx in seq_along(ests)[-1]) {
    with(filter(mm, estimator == ests[idx], scenario == scen), {
      lines(rmse ~ offset, col = main_cols[match(ests[idx], estimators)], 
        lwd = line_size,
        lty = ltys[idx])
    })
  }
  axis(1, cex.axis = 1.2)
  axis(2, las = 1, cex.axis = 1.2)
  # uncomment to add tile overall 3 plots
  # mtext(sprintf("Part 1, Scenario %s", scen), cex = 1.5, outer = TRUE)
  dev.off()
}








# weights <- filter(weights, n_main == "100")
# bias    <- filter(bias,    n_main == "100")
# mse     <- filter(mse,     n_main == "100")
# esss    <- filter(esss,    n_main == "100")


# separate plots ----
# for(scen in scens) {
#   pdf(sprintf("./plots/separate/scenario-%s-weights.pdf", scen), height = 6, width = 6)
#   # ~~ weights plot ----
#   # bart <- filter(mm, estimator == "bart", scenario == scen)
#   bylm <- filter(mm, estimator == "bylm", scenario == scen)
#   mem  <- filter(mm, estimator == "mm",   scenario == scen)
#   cnb  <- filter(mm, estimator == "cnb",  scenario == scen)
#   # plot(bart$weight ~ bart$offset, type = 'l', ylim = c(0, 1),
#   #   xlab = list(expression(delta), cex = axis_size),
#   #   ylab = list("Posterior Weight", cex = axis_size),
#   #   col = "steelblue2",
#   #   lwd = line_size,
#   #   main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
#   #   axes = FALSE)
#   # legend("topleft", legend = c("BART", "BLM", "No Causal", "No Borrowing"),
#   #   col = c("steelblue2", "seagreen3", "gold", "salmon"),
#   #   lwd = 2,
#   #   bty = "n")
#   plot(bylm$weight ~ bylm$offset, type = 'l', ylim = c(0, 1),
#     xlab = list(expression(delta), cex = axis_size),
#     ylab = list("Posterior Weight", cex = axis_size),
#     col = "seagreen3",
#     lwd = line_size,
#     main = list("Posterior Weight in Favor of Borrowing", cex = title_size),
#     axes = FALSE)
# 
#   axis(1)
#   axis(2, las = 1)
#   # lines(bylm$weight ~ bylm$offset,
#   #   col = "seagreen3",
#   #   lwd = line_size)
#   lines(mem$weight ~ mem$offset,
#     col = "gold",
#     lwd = line_size)
#   dev.off()
# 
#   # ~ bias plot ----
#   pdf(sprintf("./plots/separate/scenario-%s-bias.pdf", scen), height = 6, width = 6)
#   # dd <- filter(bias, estimator != "MEM", scenario == scen)
#   dd <- filter(mm, scenario == scen)
#   bias_range <- c(-1, 1) * ceiling(max(abs(range(dd$bias))))
#   # bias_range <- c(-1, 1) * max(abs(range(dd$bias)))
# 
#   # bart <- filter(bias, estimator == "BART", scenario == scen)
#   # bylm <- filter(bias, estimator == "BYLM", scenario == scen)
#   # plot(bart$bias ~ bart$offset, type = 'l', 
#   #   ylim = bias_range,
#   #   xlab = list(expression(delta), cex = axis_size),
#   #   ylab = list("Bias", cex = axis_size),
#   #   col = "steelblue2",
#   #   lwd = line_size,
#   #   main = list("Estimated Bias", cex = title_size),
#   #   axes = FALSE)
#   plot(bylm$bias ~ bylm$offset, type = 'l',
#     ylim = bias_range,
#     xlab = list(expression(delta), cex = axis_size),
#     ylab = list("Bias", cex = axis_size),
#     col = "seagreen3",
#     lwd = line_size,
#     main = list("Estimated Bias", cex = title_size),
#     axes = FALSE)
#   axis(1)
#   axis(2, las = 1, at = seq(min(bias_range), max(bias_range), 1))
#   # axis(2, las = 1)
#   # lines(bylm$bias ~ bylm$offset,
#   #   col = "seagreen3",
#   #   lwd = line_size)
#   lines(mem$bias ~ cnb$offset,
#     col = "gold",
#     lwd = line_size)
#   lines(cnb$bias ~ cnb$offset,
#     col = "salmon",
#     lwd = line_size)
#   # legend_loc <- ifelse(scen %in% c(2, 3), "bottomleft", "topleft")
#   legend_loc <- "topleft"
#   legend(legend_loc,  
#     legend = c("BART", "BLM", "No Causal", "No Borrowing"),
#     col = c("steelblue2", "seagreen3", "gold", "salmon"),
#     lwd = 2,
#     bty = "n")
#   # lines(x = range(dd$offset), y = c(0, 0), lty = 2, 
#   #   lwd = 2,
#   #   col = gray(3/4))
#   dev.off()
# 
#   # ~~ MSE plot ----
#   pdf(sprintf("./plots/separate/scenario-%s-mse.pdf", scen), height = 6, width = 6)
#   # dd <- filter(mse, estimator != "MEM", scenario == scen)
#   # dd <- filter(mm, scenario == scen)
#   mse_range <- c(0, max(dd$mse))
# 
#   # bart <- filter(mse, estimator == "BART", scenario == scen)
#   # bylm <- filter(mse, estimator == "BYLM", scenario == scen)
#   # plot(bart$mse ~ bart$offset, type = 'l', 
#   #   ylim = mse_range,
#   #   xlab = list(expression(delta), cex = axis_size),
#   #   ylab = list("MSE", cex = axis_size),
#   #   col = "steelblue2",
#   #   lwd = line_size,
#   #   main = list("Estimated MSE", cex = title_size),
#   #   axes = FALSE)
#   plot(bylm$mse ~ bylm$offset, type = 'l', 
#     ylim = mse_range,
#     xlab = list(expression(delta), cex = axis_size),
#     ylab = list("MSE", cex = axis_size),
#     col = "seagreen3",
#     lwd = line_size,
#     main = list("Estimated MSE", cex = title_size),
#     axes = FALSE)
#   axis(1)
#   axis(2, las = 1)
#   # lines(bylm$mse ~ bylm$offset,
#   #   col = "seagreen3",
#   #   lwd = line_size)
#   lines(mem$mse ~ mem$offset,
#     col = "gold",
#     lwd = line_size)
#   lines(cnb$mse ~ cnb$offset,
#     col = "salmon",
#     lwd = line_size)
#   dev.off()
#   
#   # ~ esss plot ----
#   # pdf(sprintf("../plots/scenario-%s-esss.pdf", scen), height = 6, width = 6)
#   # dd <- filter(esss, estimator != "MEM", scenario == scen)
#   # esss_range <- range(dd$value * 100)
#   # 
#   # bart <- filter(esss, estimator == "BART", scenario == scen)
#   # bylm <- filter(esss, estimator == "BYLM", scenario == scen)
#   # plot(bart$value * 100 ~ bart$offset, type = 'l', 
#   #   ylim = esss_range,
#   #   xlab = list(expression(delta), cex = axis_size),
#   #   ylab = list("ESSS(%)", cex = axis_size),
#   #   col = "steelblue2",
#   #   lwd = line_size,
#   #   main = list("Estimated ESSS(%)", cex = title_size),
#   #   axes = FALSE)
#   # axis(1)
#   # axis(2, las = 1)
#   # lines(bylm$value * 100 ~ bylm$offset,
#   #   col = "seagreen3",
#   #   lwd = line_size)
#   # dev.off()
# 
# }

# marginal MEM bias at delta = 0 ----
# bias %>% 
#   filter(estimator == "MEM", offset == 0, n_main == "100")

mm %>% filter(offset == 0, estimator == "mm") %>%
  print(n = Inf)

# mse reduction plots ----

# get df with bl mse - this would be mse when weights ~= 0
# w0 <- mm %>% filter(round(weight, 4) == 0) %>%
#   group_by(scenario, estimator) %>%
#   summarize(bl_rmse = mean(rmse))
# need new approach since weights for bart are not ~= 0
# for some scenarios

# mse_df <- left_join(mm, w0) %>%
#   mutate(mse_ratio = (rmse  / bl_rmse) ^ 2)
w0 <- mm %>%
  group_by(scenario, estimator) %>%
  filter(weight == min(weight)) %>%
  mutate(bl_rmse = rmse) %>%
  select(scenario, estimator, bl_rmse)
mse_df <- left_join(mm, w0) %>%
  mutate(mse_ratio = (rmse  / bl_rmse) ^ 2)

tmp <- mse_df %>%
  filter(estimator %in% c("bart", "bylmpow")) 

tmp %>%
  ggplot(mapping = aes(x = offset, y = mse_ratio, color = estimator)) +
    geom_path() +
    facet_wrap(~ scenario) +
    theme_minimal()


# load("./RData/sim_out.RData")

# ACIC sim ----


message("update to 1:5?")
file_list <- paste0("./outfiles/acic_out_", (1:5)[1:3], ".txt") 
rr_list   <- lapply(file_list, read.table, header = TRUE)

rr <- do.call(rbind, rr_list)

# rr <- read.table("../outfiles/sim_out.txt", header = TRUE)
rr <- as_tibble(rr)
rr$varying <- as.character(rr$varying)

# ------------------------- #
# --- find missing rows --- #
# ------------------------- #
args <- expand.grid(
  seed     = 1:10,
  scenario = 1:77,
  delta    = seq(-2.5, 2.5, 0.5),
  varying  = c("treatment", "intercept")
)
args$varying <- as.character(args$varying)
  
args <- as_tibble(args)

setdiff(names(args), names(rr))

missing_args <- anti_join(args, rr)

# # check unique levels of simulation parameters
# nd <- rr %>% select(model.trt, root.trt, overlap.trt,
#   model.rsp, alignment, te.hetero)

vars <- c("model.trt", "root.trt", "overlap.trt", "model.rsp", "alignment",
  "te.hetero")
rr[vars] <- lapply(rr[vars], as.character)

dd <- rr %>% gather(contains("weight"), contains("est"),
  key = "est", value = "estimate") %>%
  separate(est, into = c("estimator", "estimand")) 
dd %>% select(estimator, estimand, estimate)
dd <- dd %>% spread(key = estimand, value = estimate)
dd %>% select(estimator, est, weight)
dd <- dd %>% group_by(model.trt, root.trt, 
  overlap.trt, model.rsp, alignment, te.hetero) %>%
  mutate(PATE = mean(SATE)) %>%
  ungroup() %>%
  mutate(error = est - SATE)
# dd %>% arrange(model.trt, root.trt, 
#   overlap.trt, model.rsp, alignment, te.hetero) %>%
#   head()
# dd <- dd %>% mutate(error = est - SATE)


for (ving in c("intercept", "treatment")) {
  for(vv in vars) {
    # vv <- vars[5]
    lvls <- unique(rr[, vv, drop = TRUE])

    gg <- eval(substitute(dd %>% group_by(vv, varying, estimator, delta), 
      list(vv = as.name(vv))))
    gg <- gg %>% summarize(
      bias   = mean(error),
      weight = mean(weight),
      mse    = mean(error ^ 2)
    )

    gg <- gg %>%
      gather(bias, weight, mse, key = estimand, value = estimate) %>%
      filter(varying == ving)
    gcall <- substitute(
      gg %>% ggplot(mapping = aes(x = delta, y = estimate, 
        color = estimator)) +
        geom_path() + 
        theme_classic() +
        facet_wrap(~ vv + estimand),
      list(vv = as.name(vv))
    )

    pdf(sprintf("plots/ACIC/manuscript/quick/ACIC-%s-%s.pdf", ving, vv))
    print(eval(gcall))
    dev.off()
  }
}

# ACIC manuscript plots ----

oo <- dd


# vv <- "treatment"

# ~ pdfs ----
for (vv in c("treatment", "intercept")) {
  gg <- oo %>% filter(varying == vv)

  # ~~ model.trt   ----

  dd <- gg %>% 
    group_by(model.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("linear", "polynomial", "step")
  flabs <- c("Linear", "Polynomial", "Step")

  for (ii in 1:3) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/model-trt-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, model.trt == dlabs[ii]),
        title = sprintf("Part 2, %s Treatment Assignment Mechanism", flabs[ii]))
    dev.off()
    # debug (ratio_plot)
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/model-trt-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, model.trt == dlabs[ii]))
    dev.off()
  }


  # ~~ root.trt    ----

  dd <- gg %>% 
    group_by(root.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c(0.35, 0.65)
  flabs <- c(35, 65)

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/root-trt-%s.pdf", 
      vv, flabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, root.trt == dlabs[ii]),
        title = sprintf("Part 2, Pr(Treatment = 1) = %s", dlabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/root-trt-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, root.trt == dlabs[ii]))
    dev.off()

  }

  # ~~ overlap.trt ----
  dd <- gg %>% 
    group_by(overlap.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("full", "one-term")
  flabs <- c("Full", "One-Term")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/overlap-trt-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, overlap.trt == dlabs[ii]),
        title = sprintf("Part 2, %s Overlap", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/overlap-trt-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, overlap.trt == dlabs[ii]))
    dev.off()
  }

  # ~~ model.rsp   ----
  dd <- gg %>% 
    group_by(model.rsp, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("linear", "exponential", "step")
  flabs <- c("Linear", "Exponential", "Step")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/model-rsp-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, model.rsp == dlabs[ii]),
        title = sprintf("Part 2, %s Response Surface", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/model-rsp-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, model.rsp == dlabs[ii]))
    dev.off()
  }

  # ~~ alignment   ----

  dd <- gg %>% 
    group_by(alignment, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c(0, 0.25, 0.75)
  flabs <- c(0, 25, 75)

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/alignment-%s.pdf", 
      vv, flabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, alignment == dlabs[ii]),
        title = sprintf("Part 2, Alignment = %s", dlabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/alignment-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, alignment == dlabs[ii]))
    dev.off()
  }

  # ~~ te.hetero   ----

  dd <- gg %>% 
    group_by(te.hetero, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("high", "med", "none")
  flabs <- c("High", "Medium", "No")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/te-hetero-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle,
      width  = 10,
      height = 10 / 3)
      acic_plot(filter(dd, te.hetero == dlabs[ii]),
        title = sprintf("Part 2, %s Treatment Effect Heterogeneity", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/te-hetero-%s.pdf", 
      vv, dlabs[ii])
    pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
    ratio_plot(filter(dd, te.hetero == dlabs[ii]))
    dev.off()
  }
}

# mse ratios for all scenarios
for (scen in 1:77) {
  # scen <- 1
  cl <- oo %>% filter(scenario == scen, varying == "treatment") %>%
    group_by(estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )
  ptitle <- sprintf("plots/ACIC/variance-ratios/by-scenario/scenario-%s.pdf", 
    scen)
  pdf(ptitle, width = 10 * 2 / 3, height = 10 / 3)
  ratio_plot(cl)
  dev.off()
}

# ~ jpegs ----
for (vv in c("treatment", "intercept")) {
  gg <- oo %>% filter(varying == vv)

  # ~~ model.trt   ----

  dd <- gg %>% 
    group_by(model.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("linear", "polynomial", "step")
  flabs <- c("Linear", "Polynomial", "Step")

  for (ii in 1:3) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/model-trt-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    acic_plot(filter(dd, model.trt == dlabs[ii]),
        title = sprintf("Part 2, %s Treatment Assignment Mechanism", flabs[ii]))
    dev.off()
    # debug (ratio_plot)
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/model-trt-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, model.trt == dlabs[ii]))
    dev.off()
  }


  # ~~ root.trt    ----

  dd <- gg %>% 
    group_by(root.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c(0.35, 0.65)
  flabs <- c(35, 65)

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/root-trt-%s.jpeg", 
      vv, flabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
      acic_plot(filter(dd, root.trt == dlabs[ii]),
        title = sprintf("Part 2, Pr(Treatment = 1) = %s", dlabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/root-trt-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, root.trt == dlabs[ii]))
    dev.off()

  }

  # ~~ overlap.trt ----
  dd <- gg %>% 
    group_by(overlap.trt, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("full", "one-term")
  flabs <- c("Full", "One-Term")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/overlap-trt-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
      acic_plot(filter(dd, overlap.trt == dlabs[ii]),
        title = sprintf("Part 2, %s Overlap", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/overlap-trt-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, overlap.trt == dlabs[ii]))
    dev.off()
  }

  # ~~ model.rsp   ----
  dd <- gg %>% 
    group_by(model.rsp, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("linear", "exponential", "step")
  flabs <- c("Linear", "Exponential", "Step")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/model-rsp-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    acic_plot(filter(dd, model.rsp == dlabs[ii]),
        title = sprintf("Part 2, %s Response Surface", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/model-rsp-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, model.rsp == dlabs[ii]))
    dev.off()
  }

  # ~~ alignment   ----

  dd <- gg %>% 
    group_by(alignment, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c(0, 0.25, 0.75)
  flabs <- c(0, 25, 75)

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/alignment-%s.jpeg", 
      vv, flabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    acic_plot(filter(dd, alignment == dlabs[ii]),
        title = sprintf("Part 2, Alignment = %s", dlabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/alignment-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, alignment == dlabs[ii]))
    dev.off()
  }

  # ~~ te.hetero   ----

  dd <- gg %>% 
    group_by(te.hetero, varying, estimator, delta) %>%
    summarize(
      bias    = mean(error),
      weight  = mean(weight),
      rmse    = sqrt(mean(error ^ 2)),
      var     = var(error)
    )


  dlabs <- c("high", "med", "none")
  flabs <- c("High", "Medium", "No")

  for (ii in seq_along(dlabs)) {
    ptitle <- sprintf("plots/ACIC/manuscript/varying-%s/te-hetero-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    acic_plot(filter(dd, te.hetero == dlabs[ii]),
        title = sprintf("Part 2, %s Treatment Effect Heterogeneity", flabs[ii]))
    dev.off()
    ptitle <- sprintf("plots/ACIC/variance-ratios/varying-%s/te-hetero-%s.jpeg", 
      vv, dlabs[ii])
    jpeg(ptitle,       
      width  = 10,       
      height = 10 / 3, 
      units = "in", 
      quality = 100, 
      res = 72 * 2)      
    ratio_plot(filter(dd, te.hetero == dlabs[ii]))
    dev.off()
  }
}


unique((oo %>% filter(te.hetero == "none"))[, "scenario", drop = TRUE])

# ACIC boxplots ----

dd <- rr %>% gather(contains("weight"), contains("est"),
  key = "est", value = "estimate") %>%
  separate(est, into = c("estimator", "estimand")) 
dd %>% select(estimator, estimand, estimate)
dd <- dd %>% spread(key = estimand, value = estimate)
dd %>% select(estimator, est, weight)
dd <- dd %>% group_by(scenario) %>%
  mutate(PATE = mean(SATE)) %>%
  ungroup() %>%
  mutate(error = est - SATE)

dd %>% select(estimator, weight, error, PATE)

acic_summary <- dd %>%
  group_by(scenario, estimator, delta) %>%
  summarize(
    bias   = mean(error),
    weight = mean(weight),
    mse    = mean(error ^ 2),
    rmse   = sqrt(mse))

dev.new()
par(mfrow = c(1, 3))
with(filter(acic_summary, estimator == "nb"),      boxplot(mse ~ delta))
with(filter(acic_summary, estimator == "bart"),    boxplot(mse ~ delta))
with(filter(acic_summary, estimator == "bylmpow"), boxplot(mse ~ delta))

dev.new()
par(mfrow = c(1, 3))
with(filter(acic_summary, estimator == "nb"),      boxplot(bias ~ delta))
with(filter(acic_summary, estimator == "bart"),    boxplot(bias ~ delta))
with(filter(acic_summary, estimator == "bylmpow"), boxplot(bias ~ delta))

dev.new()
par(mfrow = c(1, 2))
with(filter(acic_summary, estimator == "bart"),    boxplot(weight ~ delta))
with(filter(acic_summary, estimator == "bylmpow"), boxplot(weight ~ delta))

# look at high-error scenarios
arrange(dd, desc(abs(error))) %>%
  select(scenario, seed, delta, varying, estimator, error)

arrange(dd, abs(error)) %>%
  select(scenario, seed, delta, varying, estimator, error)

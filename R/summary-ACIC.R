library(tidyverse)

# source pars ----
source("R/summary-pars.R")


# message("update to 1:5?")
# file_list <- paste0("./outfiles/acic_out_", (1:5)[2:4], ".txt") 
file_list <- paste0("outfiles/acic/", dir("outfiles/acic/"))
rr_list   <- lapply(file_list, read.table, header = TRUE)

rr <- do.call(rbind, rr_list)

# rr <- read.table("../outfiles/sim_out.txt", header = TRUE)
rr <- as_tibble(rr)
rr$varying <- as.character(rr$varying)

# ------------------------- #
# --- find missing rows --- #
# ------------------------- #
args <- expand.grid(
  scenario = 1:77,
  seed     = 1:50,
  delta    = seq(-2.25, 2.25, 0.75),
  varying  = "treatment" #c("treatment", "intercept")
)

args$varying <- as.character(args$varying)
  
args <- as_tibble(args)

setdiff(names(args), names(rr))

missing_args <- anti_join(args, rr)

# # check unique levels of simulation parameters
# nd <- rr %>% select(model.trt, root.trt, overlap.trt,
#   model.rsp, alignment, te.hetero)

# boxplots ----

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
    rmse   = sqrt(mse),
    var    = mse - bias ^ 2)

# estimators <- unique(dd$estimator)
table(dd$estimator)


# plots at delta == 0 ----


# delta = 0 plots ----

ests <- c("nbbart", "bart", "bartlogp", "bartp2", "blmp")

acic_sub <- acic_summary %>%
  filter(estimator %in% ests, delta == 0)

mlist <- split(acic_sub, acic_sub$estimator)
mlist <- mlist[match(estimators, names(mlist), 0)]

lbias <- lapply(mlist, "[[", "bias")
lmse  <- lapply(mlist, "[[", "mse")
lrmse <- lapply(mlist, "[[", "rmse")



cs  <- acic_cols[match(ests, estimators)]
es  <- elabs[match(ests, estimators)]

temp_axis_size <- 1
jpeg("plots/ACIC-boxplots/acic-bias-mse-at-zero.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2) 
par(mfrow = c(1, 2))
boxplot(lbias,
  col  = cs,
  axes = FALSE,
  ylab = list("Bias", cex = axis_size),
  main = list("Estimated Bias", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
axis(1, at = seq_along(ests), labels = es, cex.axis = temp_axis_size)
abline(h = 0, lty = 2)

boxplot(lmse,
  col  = cs,
  axes = FALSE,
  ylab = list("MSE", cex = axis_size),
  main = list("Estimated MSE", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
axis(1, at = seq_along(ests), labels = es, cex.axis = temp_axis_size)
dev.off()

jpeg("plots/ACIC-boxplots/acic-bias-rmse-at-zero.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2) 
par(mfrow = c(1, 2))
boxplot(lbias,
  col  = cs,
  axes = FALSE,
  ylab = list("Bias", cex = axis_size),
  main = list("Estimated Bias", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
axis(1, at = seq_along(ests), labels = es, cex.axis = temp_axis_size)
abline(h = 0, lty = 2)

boxplot(lrmse,
  col  = cs,
  axes = FALSE,
  ylab = list("Root MSE", cex = axis_size),
  main = list("Estimated Root MSE", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
axis(1, at = seq_along(ests), labels = es, cex.axis = temp_axis_size)
dev.off()

# all values of delta ----
ests <- setdiff(ests, "blmp")
acic_sub <- acic_summary %>%
  filter(estimator %in% ests, abs(delta) < 2.25)


mlist <- split(acic_sub, acic_sub$estimator)
mlist <- mlist[match(estimators, names(mlist), 0)]
mlist <- lapply(mlist, function(x) split(x, x$delta))
mlist <- do.call(c, mlist)

lbias <- lapply(mlist, "[[", "bias")
lmse  <- lapply(mlist, "[[", "mse")
lrmse <- lapply(mlist, "[[", "rmse")

sq <- seq(-1.5, 1.5, 3/4) * 8
vg <- (seq_along(ests) - mean(seq_along(ests)))
at <- c(outer(sq, vg, '+'))

cs  <- acic_cols[match(ests, estimators)]
csr <- rep(cs, each = length(sq))
es  <- elabs[match(ests, estimators)]


jpeg("plots/ACIC-boxplots/acic-bias-all-delta.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(lbias,
  at  = at,
  col  = csr,
  axes = FALSE,
  # xlab = list(expression(delta), cex = axis_size),
  xlab = "Standardized Difference in Treatment Effect (Supplemental - Primary)",
  ylab = list("Bias", cex = axis_size),
  main = list("Estimated Bias", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
# axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
axis(1, at = sq, labels = seq(-1.5, 1.5, .75), cex.axis = axis_size)
abline(h = 0, lty = 2)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()

jpeg("plots/ACIC-boxplots/acic-mse-all-delta.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(lmse,
  at  = at,
  col  = csr,
  axes = FALSE,
  # xlab = list(expression(delta), cex = axis_size),
  xlab = "Standardized Difference in Treatment Effect (Supplemental - Primary)",
  ylab = list("MSE", cex = axis_size),
  main = list("Estimated MSE", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
# axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
axis(1, at = sq, labels = seq(-1.5, 1.5, .75), cex.axis = axis_size)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()

jpeg("plots/ACIC-boxplots/acic-rmse-all-delta.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(lrmse,
  at  = at,
  col  = csr,
  axes = FALSE,
  # xlab = list(expression(delta), cex = axis_size),
  xlab = "Standardized Difference in Treatment Effect (Supplemental - Primary)",
  ylab = list("Root MSE", cex = axis_size),
  main = list("Estimated Root MSE", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
# axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
axis(1, at = sq, labels = seq(-1.5, 1.5, .75), cex.axis = axis_size)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()

# ~ BART & BLM ----
# # ~ estimators to NOT plot
# ests <- names(table(acic_summary$estimator))
# ests <- ests[match(estimators, ests, 0)]
# 
# dont_plot <- c("blmp", "bartp", "nbblm", "blmpvs", "nbblmpvs")
# ests <- setdiff(ests, dont_plot)
ests <- c("nbbart", "bart", "bartlogp", "bartp2", "blmp")
acic_sub <- acic_summary %>%
  filter(estimator %in% ests, abs(delta) < 2.25)


mlist <- split(acic_sub, acic_sub$estimator)
mlist <- mlist[match(estimators, names(mlist), 0)]
mlist <- lapply(mlist, function(x) split(x, x$delta))
mlist <- do.call(c, mlist)

lbias <- lapply(mlist, "[[", "bias")
lmse  <- lapply(mlist, "[[", "mse")

sq <- seq(-1.5, 1.5, 3/4) * 10
vg <- (seq_along(ests) - mean(seq_along(ests)))
at <- c(outer(sq, vg, '+'))

cs  <- acic_cols[match(ests, estimators)]
csr <- rep(cs, each = length(sq))
es  <- elabs[match(ests, estimators)]


jpeg("plots/ACIC-boxplots/bias-2.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(lbias,
  at  = at,
  col  = csr,
  axes = FALSE,
  # xlab = list(expression(delta), cex = axis_size),
  xlab = "Difference in Treatment Effect (Supplemental - Primary)",
  ylab = list("Bias", cex = axis_size),
  main = list("Estimated Bias", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
# axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
axis(1, at = sq, labels = seq(-1.5, 1.5, .75), cex.axis = axis_size)
abline(h = 0, lty = 2)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()

jpeg("plots/ACIC-boxplots/mse-2.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(lmse,
  at  = at,
  col  = csr,
  axes = FALSE,
  # xlab = list(expression(delta), cex = axis_size),
  xlab = "Difference in Treatment Effect (Supplemental - Primary)",
  ylab = list("MSE", cex = axis_size),
  main = list("Estimated MSE", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
# axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
axis(1, at = sq, labels = seq(-1.5, 1.5, .75), cex.axis = axis_size)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()

# ~ variance ratios ----
sq <- seq(-2.25, 2.25, 3/4) * 9
# at <- c(
#   sq - 1,
#   sq + 0,
#   sq + 1)
td <- 0.5
at <- c(
  sq - td,
  sq + 0,
  sq + td)
cs  <- acic_cols[match(c("bart", "bartp", "bylmpow"), estimators)]
csr <- rep(cs, each = length(sq))
es  <- elabs[match(c("bart", "bartp", "bylmpow"), estimators)]
# at <- c(t(outer(at, c(-1, 0, 1), '+')))

jpeg("plots/ACIC-boxplots/var-ratio.jpeg",       
  width  = 10,       
  height = 10 / 2, 
  units = "in", 
  quality = 100, 
  res = 72 * 2)      
boxplot(vr,
  at  = at,
  col  = csr,
  axes = FALSE,
  xlab = list(expression(delta), cex = axis_size),
  ylab = list("Variance Ratio", cex = axis_size),
  main = list("Estimated Variance Ratio vs. No Borrowing", cex = title_size))
axis(2, cex.axis = axis_size, las = 1)
axis(1, at = sq, labels = seq(-2.25, 2.25, .75), cex.axis = axis_size)
abline(h = 1 / 2, lty = 2)
legend("topleft",
  lwd    = 4,
  col    = cs,
  legend = es,
  bty    = "n")
dev.off()



b0 <- with(filter(acic_sub, delta == 0), 
  tapply(var, estimator, identity))
b0 <- b0[c("nb", "bart", "bylmpow")]
ms <- match(names(b0), estimators)
bbox <- boxplot(b0,
  names = elabs[ms],
  col   = acic_cols[ms])

# look at high-error scenarios
arrange(dd, desc(abs(error))) %>%
  select(scenario, seed, delta, varying, estimator, error)


arrange(dd, abs(error)) %>%
  select(scenario, seed, delta, varying, estimator, error)


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


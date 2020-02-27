

# to install R package:
# setenv R_LIBS /home/merganser/jeffreyb/R
# R CMD INSTALL borrowr_0.1.1.9999.tar.gz

n_cores <- 8

# library(borrowr)
library(parallel)

source("functions.R")

# designed to run on 5 servers
host       <- system2("hostname", stdout = TRUE)
servers    <- c("carbon", "cesium", "chromium", "potassium", "silicon")
# servers    <- c("carbon", "chromium", "potassium", "silicon")
hosts      <- paste0(servers, ".ccbr.umn.edu")
n_serv     <- length(hosts)
which_serv <- match(host, hosts)

# main simulation ---- 


# specify file for the output files to track function args.
# args_file <- paste0("./args/args_", which_serv, ".txt")

# for debugging:
# which_serv <- 1

args <- expand.grid(
  seed              = 1:1000,
  n_main            = 100,
  sigma             = 1,
  scenario          = 1:6,
  offset            = seq(-2.5, 2.5, 1 / 2),
  ndpost            = 100
)
# for debugging:
# library(borrowr)
# al <- as.list(args[1, ])
# al$n_supp <- al$n_main
# do.call(main_sim, al)
# for (i in 1:6) {
#   al$scenario <- i
#   print(do.call(main_sim, al))
# }

# assign args to server
which_serv_args <- rep(seq_len(n_serv), nrow(args) / n_serv)

args <- args[which_serv_args == which_serv, ]

# ouput files hould have nrow(args) output lines

seed              <- args$seed
n_main            <- args$n_main
n_supp            <- args$n_main # for now, sample sizes are the same!
sigma             <- args$sigma
scenario          <- args$scenario
offset            <- args$offset
ndpost            <- args$ndpost

log_file <- paste0("./logfiles/main_log_", which_serv, ".txt")

run <- function() {

  # cl <- makeCluster(n_cores, outfile = args_file)
  cl <- makeCluster(n_cores, outfile = log_file)

  on.exit(stopCluster(cl))

  # toExport <- ls()
  # clusterExport(cl, toExport)
  clusterSetRNGStream(cl, iseed = which_serv)
  clusterExport(cl, "sample_posterior")
  clusterEvalQ(cl, {
    library(borrowr, lib.loc = "/home/merganser/jeffreyb/R")
  })
  # clusterExport(cl, list(failwith, main_sim))

  sim_out <- clusterMap(cl,
    # fun               = failwith(NULL, main_sim),
    fun               = main_sim,
    seed              = seed,
    n_main            = n_main,
    n_supp            = n_supp,
    sigma             = sigma,
    scenario          = scenario,
    offset            = offset,
    ndpost            = ndpost,
    SIMPLIFY          = FALSE)

  sim_out

}

# comment out if running only ACIC ----
# sim_out <- run()
# rr <- as.data.frame(do.call(rbind, sim_out))
# write.table(rr,
#   file      = paste0("../outfiles/main/main_out_", which_serv, ".txt"),
#   quote     = FALSE,
#   row.names = FALSE)

###

# acic simulation ----

# R CMD INSTALL aciccomp2016_0.1-0.tar.gz

# specify file for the output files to track function args.
log_file <- paste0("./logfiles/acic_log_", which_serv, ".txt")

# for debugging:
# which_serv <- 1

args <- expand.grid(
  scenario = 1:77,
  seed     = 1:50,
  delta    = seq(-2.25, 2.25, 0.75),
  varying  = "treatment" #c("treatment", "intercept")
)

args$varying <- as.character(args$varying)

# read outfiles to determine missing args
try({
  
  out_path  <- "../outfiles/acic/"
  out_files <- paste0(out_path, dir(out_path))

  res <- lapply(out_files, read.table, header = TRUE, as.is = TRUE)
  res <- do.call(rbind, res)

  res  <- res[, intersect(colnames(res), colnames(args))]
  args <- dplyr::anti_join(args, res)
  
}, silent = TRUE)



# for debugging:
# library(borrowr)
# library(aciccomp2016)
# library(glmnet)
# al <- as.list(args[1, ])
# debugonce(acic_sim)
# do.call(acic_sim, al)
# for (i in 1:5) {
#   al$seed <- i
#   print(do.call(acic_sim, al))
# }

# assign args to server
which_serv_args <- rep(seq_len(n_serv), ceiling(nrow(args) / n_serv))
which_serv_args <- which_serv_args[seq_len(nrow(args))]

args <- args[which_serv_args == which_serv, ]

# ouput files hould have nrow(args) output lines

scenario <- args$scenario
seed     <- args$seed
delta    <- args$delta
varying  <- args$varying



run <- function() {

  cl <- makeCluster(n_cores, outfile = log_file)
  # cl <- makeCluster(n_cores)

  on.exit(stopCluster(cl))

  # toExport <- ls()
  # clusterExport(cl, toExport)
  clusterSetRNGStream(cl, iseed = which_serv)
  clusterExport(cl, "sample_posterior")
  clusterEvalQ(cl, {
    library(borrowr, lib.loc = "/home/merganser/jeffreyb/R")
    library(aciccomp2016, lib.loc = "/home/merganser/jeffreyb/R")
    library(glmnet)
  })
  # clusterExport(cl, list(failwith, main_sim))

  sim_out <- clusterMap(cl,
    # fun               = failwith(NULL, main_sim),
    fun      = acic_sim,
    seed     = seed,
    scenario = scenario,
    delta    = delta,
    varying  = varying,
    SIMPLIFY = FALSE)

  sim_out

}

# Comment out to run main sim only
acic_start <- Sys.time()
sim_out <- run()
acic_end <- Sys.time()
# 
# rr <- as.data.frame(do.call(rbind, sim_out))
# 
# 
# write.table(rr,
#   file      = paste0("../outfiles/acic_out_", which_serv, ".txt"),
#   quote     = FALSE,
#   row.names = FALSE)
# 
# acic_end - acic_start


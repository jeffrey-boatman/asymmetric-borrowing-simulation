
n_cores <- 12


# library(borrowr)
library(parallel)

source("main_sim.R")

# designed to run on 5 servers
host       <- system2("hostname", stdout = TRUE)
servers    <- c("carbon", "cesium", "chromium", "potassium", "silicon")
hosts      <- paste0(servers, ".ccbr.umn.edu")
n_serv     <- length(hosts)
which_serv <- match(host, hosts)

# specify file for the output files to track function args.
args_file <- paste0("./args/args_", which_serv, ".txt")

# for debugging:
# which_serv <- 1

args <- expand.grid(
  seed              = 1:1000,
  n_main            = 100,
  sigma             = 1,
  scenario          = c(1, 2, 3, 4, 5, 6),
  offset            = seq(-4, 4, 1 / 2),
  ndpost            = 100
)
# for debugging:
# library(borrowr)
# al <- as.list(args[1, ])
# al$n_supp <- al$n_main
# do.call(main_sim, al)

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


run <- function() {

  cl <- makeCluster(n_cores, outfile = args_file)

  on.exit(stopCluster(cl))

  # toExport <- ls()
  # clusterExport(cl, toExport)
  clusterSetRNGStream(cl, iseed = which_serv)
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

sim_out <- run()

rr <- as.data.frame(do.call(rbind, sim_out))

# save(sim_out, file = "./RData/sim_out.RData")

write.table(rr,
  file      = paste0("../outfiles/out_", which_serv, ".txt"),
  quote     = FALSE,
  row.names = FALSE)

###


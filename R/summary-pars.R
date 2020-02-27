# This file includes paramters governing
# the creation of plots for both summary-main.R
# and summary-ACIC.R

#library(colortools)
estimators <- c(
    "nbbart"  
  , "bart"
  , "bartlogp"
  , "bartp"
  , "bartp2"
  , "nbblm"
  , "blm"
  , "blmlogp"
  , "blmp"
  , "blmpvs"
  , "nbblmpvs"
  , "mm")
# ~ elabs ----
elabs <- c(
    expression(BART[NB])
  , "BART"
  , expression(BART[log[2]][r])
  , expression(BART[r])
  , expression(BART[r2])
  , expression(BLM[NB])
  , "BLM"
  , expression(BLM[log[2]][r])
  , expression(BLM[r])
  , expression(BLMVS[r])
  , expression(BLMVS[NB]
  , "MM")
)
# ~ colors ----
# for bart maybe try analogous ("salmon3")
blmcols  <- c(colortools::analogous("steelblue3"), "grey")
bartcols <- colortools::analogous("salmon2")
# bartcols <- colortools::analogous("#94CD4F")
# cols <- c(
#   bartcols,
#   blmcols
# )
# cols <- viridis::viridis(length(estimators))
# cols <- c("red", "orange", "yellow", "green", "blue", "brown", "steelblue2", "seagreen3", "grey")
# cols <- viridis::viridis(length(estimators))
base_col <- "steelblue"
tetcol   <- colortools::tetradic(base_col)
blmcols  <-c(colortools::analogous(base_col), tetcol[2])
bartcols <- c(colortools::analogous(tetcol[4]), tetcol[3])
# colortools::analogous("#9E5CEE")
acic_cols <- c(
    gray(0.5)
  , bartcols[1] # "bart"
  , bartcols[2] # "bartlogp"
  , bartcols[3] # "bartp"
  , bartcols[4] # "bartp2"
  , gray(0.75) # "nbblm"
  , blmcols[1] # "blm"
  , blmcols[2] # "blmlogp"
  , blmcols[3] # "blmp"
  , blmcols[4] # "blmpvs"
  , "red" # "nbblmpvs"
  , "green" #"mm"
  )

# main cols were selected by using:
# colortools::tetradic("steelblue")
main_cols <- c(
    gray(0.5)
  , "#94CD4F" #"#82B446" # "bart"
  , "#94CD4F" # "#82B446" # "bartlogp"
  , "#94CD4F" # "#82B446" # "bartp"
  , "#94CD4F" # "#82B446" # "bartp2"
  , gray(0.75) # "nbblm"
  , "#4F94CD" # "#4682B4" # "blm"
  , "#4F94CD" # "#4682B4" # "blmlogp"
  , "#4F94CD" # "#4682B4" # "blmp"
  , "#4F94CD" # "#4682B4" # "blmpvs"
  , "#4F94CD" #"red" # "nbblmpvs"
  , "green" #"mm"
  )

# cols <- c(
#     gray(0.5)
#   , "gold"# "bart"
#   , bartcols[2] # "bartlogp"
#   , "gold" # "bartp"
#   , bartcols[4] # "bartp2"
#   , "seagreen3" # gray(0.75) # "nbblm"
#   , "steelblue2" # blm
#   , blmcols[2] # "blmlog
#   , "steelblue2" # "blmp"
#   , blmcols[4] # "blmpvs"
#   , "red" # "nbblmpvs"
#   , "green" #"mm"
#   )
axis_size <- 1.2
title_size <- 1.5
line_size <- 2
# ~~ choose font size 
# font_size <- 20




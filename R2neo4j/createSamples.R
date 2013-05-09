# This function simply serves to take sample of N rows from NodesDT
#  and a sample from RelsDT with corresponding rels, with max rows, R

createSamples <- function(N=5e2, R=2.2*N, NodesDT, RelsDT, spree=10, envir=parent.frame())  {
#  R is a maximum, a cap;  it is not a guaranteed minimum. 

  if (missing(NodesDT))
    NodesDT <- NODES.DT
  if (missing(RelsDT))
    RelsDT <- RELS.DT

  if (nrow(NodesDT) < 2*spree) {
    warning("Very few rows in NodesDT relative to `spree`. Results might be weird.")
  }

  ## expanding the sample size to allow for more rels
  expandFactor <- 10
  samples <- sample((nrow(NodesDT)/spree-1), expandFactor * N/spree, FALSE) * spree

  n.indexes <- c(sapply(samples, `+`, seq(spree)))

  NodesSample <- copy(NodesDT[n.indexes])
  nodes       <- NodesSample$node

  RelsSample <- copy(RelsDT[start %in% nodes & end %in% nodes])


  ## PRUNE IT DOWN

  # sample rels first, take nodes from there, then sample from nodes to the correct ammount
  relsNodes <- RelsSample[, c(start, end)]

  NodesSample <- NodesSample[node %in% relsNodes]
  NodesSample <- NodesSample[sample(nrow(NodesSample), N, FALSE)]
  nodes       <- NodesSample$node

  RelsSample <- RelsSample[start %in% nodes & end %in% nodes]

  if (nrow(RelsSample) > R) 
    RelsSample <- RelsSample[sample(nrow(RelsSample), R, FALSE)]

  assign("NodesSample", NodesSample, envir=envir)
  assign("RelsSample",  RelsSample,  envir=envir)

  cat("Created the following tables: \n\n\tNodesSample \n\tRelsSample")
  rm(NodesDT, RelsDT)

}



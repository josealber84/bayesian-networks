library(magrittr)
library(tidyverse)

create_bnfactor <- function(variable_names, variable_cardinality, values){
  
  result <- list(var = variable_names,
                 card = as.integer(variable_cardinality),
                 val = values)
  class(result) <- "bnfactor"
  result
  
}

unfold_bnfactor <- function(bnfactor){
  nrows <- length(bnfactor$val)
  ncols <- length(bnfactor$var) + 1
  numbers <- lapply(bnfactor$card, function(x) 1:x)
  numbers <- expand.grid(numbers)
  res <- matrix(data = 0, nrow = nrows, ncol = ncols)
  res[, 1:(ncols-1)] <- numbers
  res[, ncols] <- bnfactor$val
  colnames(res) <- c(bnfactor$var, "prob")
  res
}

print.bn_factor <- function(bnfactor){
  
  unfolded <- matrix(nrow = bn_factor$val %>% length())
  
}
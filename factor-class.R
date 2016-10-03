library(magrittr)
library(tidyverse)

create_bnfactor <- function(variable_names, variable_cardinality, values){
  
    # Attributes
    if(length(values) != prod(variable_cardinality))
        stop("Bad dimensions in values")
    if(length(variable_names) != length(variable_cardinality))
        stop("Bad dimensions in cardinality")
    
    result <- list(var = variable_names,
                   card = as.integer(variable_cardinality),
                   val = values)
    class(result) <- "bnfactor"
    
    # Methods
    warning("Pendiente meter aquí los métodos de get y set, para hacerlo más usable")
    
    
    result
    
}

unfold_bnfactor <- function(bnfactor){
    
    if(class(bnfactor) != "bnfactor") 
        stop("Argument must be a bnfactor")
    
    nrows <- length(bnfactor$val)
    ncols <- length(bnfactor$var) + 1
    numbers <- lapply(bnfactor$card, function(x) 1:x)
    numbers <- expand.grid(numbers) %>% as.matrix()
    res <- matrix(data = 0, nrow = nrows, ncol = ncols)
    res[, 1:(ncols-1)] <- numbers
    res[, ncols] <- bnfactor$val
    colnames(res) <- c(bnfactor$var, "prob")
    res
}

print.bnfactor <- function(bnfactor){
  unfold_bnfactor(bnfactor)
}

get_value_index <- function(bnfactor, variable_assignment){
    
    if(length(variable_assignment) != length(bnfactor$var)){
        stop("Bad dimensions in variable assignment")
    }
    
    unfolded <- unfold_bnfactor(bnfactor)
    logical_result <- rep(TRUE, nrow(unfolded))
    for(col in 1:(ncol(unfolded) - 1)){
        logical_result <- 
            logical_result & (unfolded[,col] == variable_assignment[col])
    }
    
    which(logical_result)
    
}

get_value <- function(bnfactor, variable_assignment){
    bnfactor$val[get_value_index(bnfactor, variable_assignment)]
}

set_value <- function(bnfactor, variable_assignment, value){
    bnfactor$val[get_value_index(bnfactor, variable_assignment)] <- value
    bnfactor
}
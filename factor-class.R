library(magrittr)
library(tidyverse)

create_bnfactor <- function(variable_names, variable_cardinality, values){
    
    # Check input
    if(length(values) != prod(variable_cardinality))
        stop("Bad dimensions in values")
    if(length(variable_names) != length(variable_cardinality))
        stop("Bad dimensions in cardinality")
    
    # Return functions
    objeto <- list(get_variable_names = function() variable_names,
                   get_variable_cardinality = function() variable_cardinality,
                   get_all_values = function() values,
                   print = function() 
                       unfold_bnfactor(variable_names, variable_cardinality, values),
                   get_value = function(variable_assignment)
                       values[get_value_index(variable_names, variable_cardinality, values, variable_assignment)],
                   set_value = function(variable_assignment, value)
                       values[get_value_index(variable_names, variable_cardinality, values, variable_assignment)] <<- value)
    class(objeto) <- "bnfactor"
    objeto
        
}


unfold_bnfactor <- function(variable_names, variable_cardinality, values){
    
    nrows <- length(values)
    ncols <- length(variable_names) + 1
    numbers <- lapply(variable_cardinality, function(x) 1:x)
    numbers <- expand.grid(numbers) %>% as.matrix()
    res <- matrix(data = 0, nrow = nrows, ncol = ncols)
    res[, 1:(ncols-1)] <- numbers
    res[, ncols] <- values
    colnames(res) <- c(variable_names, "prob")
    res
}

print.bnfactor <- function(bnfactor){
  bnfactor$print()
}

get_value_index <- function(variable_names, variable_cardinality, values, variable_assignment){
    
    if(length(variable_assignment) != length(variable_names)){
        stop("Bad dimensions in variable assignment")
    }
    
    unfolded <- unfold_bnfactor(variable_names, variable_cardinality, values)
    logical_result <- rep(TRUE, nrow(unfolded))
    for(col in 1:(ncol(unfolded) - 1)){
        logical_result <- 
            logical_result & (unfolded[,col] == variable_assignment[col])
    }
    
    which(logical_result)
    
}

`[<-.bnfactor` <- function(...){
    arguments <- list(...)
    bnfactor <- arguments[[1]]
    variable_assignment <- arguments[1 + (1:length(bnfactor$get_variable_names()))] %>% unlist()
    value <- arguments[[length(arguments)]]
    bnfactor$set_value(variable_assignment, value)
    bnfactor
}
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

bnfactor_product <- function(bnfactor1, bnfactor2){
    
    if(!(class(bnfactor1) == "bnfactor" && class(bnfactor2) == "bnfactor")){
        stop("Arguments must be bnfactors")
    }
    
    # Check that factors have at least one common variable
    common_variable_index <- 
        bnfactor1$get_variable_names() %in% bnfactor2$get_variable_names()
    if(sum(common_variable_index) == 0)
        stop("Factors do not have variables in common")
    
    # Check that common variables have the same cardinality
    common_variables <- bnfactor1$get_variable_names()[common_variable_index]
    cardinality1 <- bnfactor1$get_variable_cardinality()[common_variable_index]
    index2 <- bnfactor2$get_variable_names() %in% common_variables
    cardinality2 <- bnfactor2$get_variable_cardinality()[index2]
    if(any(cardinality1 != cardinality2))
        stop("Common variables have different cardinality")
    
    # Create result structure
    result_var_names <- c(bnfactor1$get_variable_names(), 
                          bnfactor2$get_variable_names())
    result_var_card <- c(bnfactor1$get_variable_cardinality(),
                         bnfactor2$get_variable_cardinality())
    dupl <- duplicated(result_var_names)
    result_var_names <- result_var_names[-dupl]
    result_var_card <- result_var_card[-dupl]
    result_values <- rep(0, prod(result_var_card))
    
    result <- create_bnfactor(variable_names = result_var_names,
                              variable_cardinality = result_var_card,
                              values = result_values)
    
    # Calculate values...
    
}

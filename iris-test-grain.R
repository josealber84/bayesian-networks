library(genalg)
library(gRain)
library(graph)
library(magrittr)
library(stringr)
library(tidyverse)

# Creo la estructura de la red
red <- dag(~Species + Shadow + Sepal.Length:Species:Shadow +
               Sepal.Width:Species:Shadow + 
               Petal.Length:Species:Shadow +
               Petal.Width:Species:Shadow)
red <- dag(~Species + Sepal.Length:Species +
               Sepal.Width:Species + 
               Petal.Length:Species +
               Petal.Width:Species)
plot(red)

red_entrenada <- compile( grain(red, iris) )

# Creo una función para crear individuos.
# Cada individuo es un set de parámetros de la red bayesiana.
crear_individuo_random <- function(estructura_red){
    
    map(estructura_red, generar_parametros)
    
}

generar_parametros_random <- function(estructura_nodo){
    
    if("prob" %in% names(estructura_nodo)){
        
        probabilidades <- estructura_nodo$prob
        nuevos_valores <- runif(n = length(probabilidades),
                                min = 0, max = 1)
        return(nuevos_valores / sum(nuevos_valores))
        
    }
    
    if("coefficients" %in% names(estructura_nodo)){
        
        coeficientes <- estructura_nodo$coefficients
        min_value <- min(coeficientes) - 1
        max_value <- max(coeficientes) + 1
        return(runif(n = length(coeficientes), 
                     min = min_value,
                     max = max_value))
    }
    
}


# Creo funciones para vectorizar y des-vectorizar individuos
vectorizar_individuo <- function(individuo){
    unlist(individuo)
}

desvectorizar_individuo <- function(individuo_vectorizado){
    stop("TODO")
}


# Creo una función para generar una red con los parámetros
# de un individuo
ajustar_red_a_individuo <- function(estructura_red, individuo){

    for(i in 1:length(estructura_red)){
        
        if("prob" %in% names(estructura_red[[i]])){
            cpt <- estructura_red[[i]]$prob
            cpt[1:length(cpt)] <- matrix(data = individuo[[i]],
                                         nrow = 1)
            estructura_red[[names(estructura_red)[[i]]]] <- cpt  
        }
        
        if("coefficients" %in% names(estructura_red[[i]])){
            cpt <- estructura_red[[i]]$coefficients
            cpt[1:length(cpt)] <- matrix(data = individuo[[i]],
                                         nrow = 1)
            estructura_red[[names(estructura_red)[[i]]]] <- cpt
        }
    }
    
    estructura_red
}

# Creo la función de evaluación
evaluar <- function(datos, red, variable_objetivo){
    
    
    
}






`[<-.pollo` <- function(a, x, value){
    cat("objeto = ", a, fill = T)
    cat("value = ", value, fill = T)
    cat("x" = deparse(substitute(x)), fill = T)
    a
}
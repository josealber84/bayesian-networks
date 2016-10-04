library(genalg)
library(bnlearn)
library(graph)
library(magrittr)
library(stringr)
library(tidyverse)

# Creo una función para crear individuos.
# Cada individuo es un set de parámetros de la red bayesiana.
get_parametros_red <- function(red){
    map(red, get_parametros_nodo)
}

get_parametros_nodo <- function(nodo){
    
    if("prob" %in% names(nodo)){
        return(nodo$prob)
    }
    
    if("coefficients" %in% names(nodo)){
        return(nodo$coefficients)
    }
    
}


generar_parametros_red_random <- function(estructura_red){
    
    map(estructura_red, generar_parametros_nodo_random)
    
}

generar_parametros_nodo_random <- function(estructura_nodo){
    
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
    
    list(Conditions = individuo_vectorizado[1:3],
         Petal.Length = individuo_vectorizado[4:12],
         Petal.Width = individuo_vectorizado[13:21],
         Sepal.Length = individuo_vectorizado[22:30],
         Sepal.Width = individuo_vectorizado[31:39],
         Species = individuo_vectorizado[40:42])
    
}

desvectorizar_individuo_2 <- function(individuo_vectorizado){
    
    list(Petal.Length = individuo_vectorizado[1:3],
         Petal.Width = individuo_vectorizado[4:6],
         Sepal.Length = individuo_vectorizado[7:9],
         Sepal.Width = individuo_vectorizado[10:12],
         Species = individuo_vectorizado[13:15])
    
}

desvectorizar_individuo_3 <- function(individuo_vectorizado){
    
    real_params$Species <- individuo_vectorizado
    real_params
    
}

# Creo una función para generar una red con los parámetros
# de un individuo
ajustar_red_a_individuo <- function(estructura_red, individuo){
    
    # Normalizar
    individuo[13:15] %<>% forzar_prob()
    
    # Des-vectorizo
    individuo %<>% desvectorizar_individuo_2()
    
    # Meto los datos en la red bayesiana
    for(i in 1:length(estructura_red)){
        
        if("prob" %in% names(estructura_red[[i]])){
            cpt <- estructura_red[[i]]$prob
            cpt[1:length(cpt)] <- matrix(data = individuo[[i]],
                                         nrow = 1)
            estructura_red[[names(estructura_red)[[i]]]] <- cpt  
        }
        
        if("coefficients" %in% names(estructura_red[[i]])){
            cpt <- coef(estructura_red[[i]])
            cpt[1:length(cpt)] <- individuo[[i]]
            estructura_red[[names(estructura_red)[[i]]]] <-
                list(coef = cpt, sd = rep(0.5, length(cpt)))
        }
    }
    
    estructura_red
}

ajustar_red_a_individuo_3 <- function(estructura_red, individuo){
    
    # Normalizar
    individuo <- individuo / sum(individuo)
    
    # Des-vectorizo
    individuo %<>% desvectorizar_individuo_3()
    
    # Meto los datos en la red bayesiana
    for(i in 1:length(estructura_red)){
        
        if("prob" %in% names(estructura_red[[i]])){
            cpt <- estructura_red[[i]]$prob
            cpt[1:length(cpt)] <- matrix(data = individuo[[i]],
                                         nrow = 1)
            estructura_red[[names(estructura_red)[[i]]]] <- cpt  
        }
        
        if("coefficients" %in% names(estructura_red[[i]])){
            cpt <- coef(estructura_red[[i]])
            cpt[1:length(cpt)] <- individuo[[i]]
            estructura_red[[names(estructura_red)[[i]]]] <-
                list(coef = cpt, sd = rep(0.5, length(cpt)))
        }
    }
    
    estructura_red
}

# Creo la función de evaluación
evaluar <- function(individuo, datos = iris){
    
    # Normalizo
    individuo[1:3] %<>% forzar_prob()
    individuo[40:42] %<>% forzar_prob()
    
    # Des-vectorizo
    individuo %<>% desvectorizar_individuo()
    
    # Ajusto, predigo y calculo el error
    red <- ajustar_red_a_individuo(fake_red, individuo)
    prediccion <- predecir_categoricas(red, datos)
    error <- 
        sum(prediccion != (datos %>% select(Species) %>% unlist %>% as.numeric()))
    cat("error = ", error, fill = T)
    error
    
}

evaluar2 <- function(individuo, datos = iris){
    
    # Ajusto, predigo y calculo el error
    red <- ajustar_red_a_individuo(fake_red, individuo)
    prediccion <- predecir_categoricas(red, datos)
    error <- 
        sum(prediccion != (datos %>% select(Species) %>% unlist %>% as.numeric()))
    cat("error = ", error, fill = T)
    error
    
}

evaluar3 <- function(individuo){
    
    # Ajusto, predigo y calculo el error
    red <- ajustar_red_a_individuo_3(fake_red, individuo)
    prediccion <- predecir_categoricas(red, iris)
    error <- 
        sum(prediccion != (iris %>% select(Species) %>% unlist %>% as.numeric()))
    cat("error = ", error, fill = T)
    error
    
}

forzar_prob <- function(x){
    abs(x) / sum(abs(x))
}

RMSE <- function(prediccion, error){
    (prediccion - error)^2 %>% mean() %>% sqrt()
}

predecir_categoricas <- function(red, datos, margin = 0.2){
    
    prediccion <- matrix(nrow = nrow(datos), ncol = 4)
    
    for(i in 1:nrow(datos)){
        
        sw <- datos$Sepal.Width[i]
        pw <- datos$Petal.Width[i]
        sl <- datos$Sepal.Length[i]
        pl <- datos$Petal.Length[i]
        evidencia <- paste0("(Sepal.Width %>% between(", sw - margin, ",", sw + margin, ")) &",
                            "(Sepal.Length %>% between(", sl - margin, ",", sl + margin, ")) &",
                            "(Petal.Width %>% between(", pw - margin, ",", pw + margin, ")) &",
                            "(Petal.Length %>% between(", pl - margin, ",", pl + margin, "))")
        
        col <- 0
        for(clase in unique(datos$Species)){
            
            col <- col + 1
            query <- paste0("cpquery(fitted = red, n = 100000,",
                            "event = (Species == '", clase, "'), ",
                            "evidence = (", evidencia, "))")
            prediccion[i,col] <- eval(parse(text = query))
            
        }
    }
    
    prediccion[,4] <- apply(prediccion, 1, function(x) which.max(x))
    prediccion[,4]
}

predecir_categoricas_2 <- function(red, datos, margin = 0.2){
    
    prediccion <- matrix(nrow = nrow(datos), ncol = 4)
    
    for(i in 1:nrow(datos)){
        
        sw <- datos$Sepal.Width[i]
        pw <- datos$Petal.Width[i]
        sl <- datos$Sepal.Length[i]
        pl <- datos$Petal.Length[i]
        sp <- datos$Species[i] %>% as.character()
        evidencia <- paste0("(Sepal.Width %>% between(", sw - margin, ",", sw + margin, ")) &",
                            "(Sepal.Length %>% between(", sl - margin, ",", sl + margin, ")) &",
                            "(Petal.Width %>% between(", pw - margin, ",", pw + margin, ")) &",
                            "(Petal.Length %>% between(", pl - margin, ",", pl + margin, ")) &",
                            "(Species == '", sp, "')")
        
        col <- 0
        for(clase in 1:3){
            
            col <- col + 1
            query <- paste0("cpquery(fitted = red, n = 100000,",
                            "event = (Conditions == ", clase, "), ",
                            "evidence = (", evidencia, "))")
            prediccion[i,col] <- eval(parse(text = query))
            
        }
    }
    
    prediccion[,4] <- apply(prediccion, 1, function(x) which.max(x))
    prediccion[,4]
}
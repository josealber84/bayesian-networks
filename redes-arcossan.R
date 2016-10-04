# La idea es crear una función para cada nodo, que calcule el valor del nodo
# en función del valor de los padres :)
# A lo mejor es una tontería, pero la idea es sencilla y versátil.
# Pruebo con el data set de vinos (https://archive.ics.uci.edu/ml/datasets/Wine)

library(tidyverse)
library(caret)

wines <- read_csv("wine-data.csv")

create_class_given_color <- function(hue, color_intensity, wine_class){
    
    # Create data frame
    data <- data_frame(hue, color_intensity, wine_class = as.factor(wine_class))
    
    # Create linear model
    model <- train(wine_class ~ ., data = data, method = "nnet", maxit = 1000, linout = 1)
    
    # Plot model
    fake_hue <- runif(n = 10000, min = min(hue), max = max(hue))
    fake_color_intensity <- runif(n = 10000, min = min(color_intensity), max = max(color_intensity))
    predictions <- data_frame(hue = fake_hue, 
                              color_intensity = fake_color_intensity, 
                              wine_class = NA)
    predictions$wine_class <- predict(model, predictions) 
    ggplot(data) + 
        geom_point(mapping = aes(x = hue, y = color_intensity, color = wine_class)) +
        geom_point(data = predictions, 
                   mapping = aes(x = hue, y = color_intensity, color = wine_class),
                   alpha = 0.05, size = 3)
    
    # Return function
    function(hue, color_intensity){
        d <- data_frame(hue, color_intensity)
        return(predict(model, d))
    }
    
}
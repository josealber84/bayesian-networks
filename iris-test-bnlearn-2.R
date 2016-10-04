source("bayesian-evolution-functions.R")

# Creo la estructura de la red
red <- model2network(paste0("[Species]",
                            "[Sepal.Length|Species]",
                            "[Sepal.Width|Species]",
                            "[Petal.Length|Species]",
                            "[Petal.Width|Species]"))
plot(red)


# Hago el cálculo de manera estándar y veo el error de predicción
red_estandar <- bn.fit(red, iris)
real_params <- get_parametros_red(red_estandar)
error <- evaluar2(individuo = vectorizar_individuo(real_params), datos = iris)
# error = 11


# Trato de mejorar el modelo cambiando sólo los prior de Species
string_min <- c(0,0,0)
string_max <- c(1,1,1)
adan <- matrix(data = c(0.333, 0.333, 0.333), nrow = 1)
resultado <- rbga(stringMin = string_min, 
                  stringMax = string_max,
                  popSize = 10, iters = 30,
                  suggestions = adan,
                  mutationChance = NA,
                  elitism = NA,
                  monitorFunc = NULL, evalFunc = evaluar3,
                  showSettings = T, verbose = T)

# Creo la red con las prior mejoradas
better_params <- real_params
better_params$Species <- resultado$population[1,] / sum(resultado$population[1,])
red_mejorada <- ajustar_red_a_individuo(red_estandar, vectorizar_individuo(better_params))



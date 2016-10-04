source("bayesian-evolution-functions.R")

# Creo la estructura de la red
red <- model2network(paste0("[Species][Conditions]",
                            "[Sepal.Length|Species:Conditions]",
                            "[Sepal.Width|Species:Conditions]",
                            "[Petal.Length|Species:Conditions]",
                            "[Petal.Width|Species:Conditions]"))
plot(red)


# Creo una red falsa para tener la estructura 
# (me invento los datos de Conditions)
fake_iris <- iris
fake_iris$Conditions <-
    rbinom(n = 150, size = 2, prob = 0.5) %>% as.factor()
fake_red <- bn.fit(red, fake_iris)


# Algortimo genético
adan <- get_parametros_red(fake_red) %>% unlist() %>% matrix(nrow = 1)
resultado <- rbga(stringMin = rep(-10, 42), stringMax = rep(10, 42),
                  suggestions = adan,
                  popSize = 10, iters = 100,
                  mutationChance = NA,
                  elitism = NA,
                  monitorFunc = NULL, evalFunc = evaluar,
                  showSettings = T, verbose = T)

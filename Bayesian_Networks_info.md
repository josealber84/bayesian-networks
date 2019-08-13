# Bayesian Network info

Based on [this PyData DC 2016 conference](https://www.youtube.com/watch?v=6pl3m-UbUV4).


## Real world applications

 * Find lost planes (Air France AF 447, June 2009)
 * Predict US elections (Nate Silver, 2012)
 * Medicine
 * Microsoft
 * Science


## Father

Judea Perl


## Discretization of numeric variables

It is often necesary to discretize numeric variables to use bayesian network implementations. 

* We can do it by using fixed lapses, using quantiles or using any other technique that makes sense in our use case.
* The number of categories we use for each variable is very important in order to avoid underfitting (if do not split enough) and overfitting (if we split too much).


## Types of bayesian networks

![](images/types_of_bn.png)



## Algorithms

![](images/algorithms.png)


## Python and R echosystem

![](images/echosystem.png)
 
* Take a look at 'Bayes Block' (Python)
* Her favourite package is 'bnlearn' (R). She shows an interesting example that includes architecture learning and inference:

![](images/bnlearn_example_learning.png)

![](images/bnlearn_example_inference.png)

* She also recommends 'BayesianNetwork Shiny app' (R) and 'Stan' (Python and R)
* Look for existing code examples! :)







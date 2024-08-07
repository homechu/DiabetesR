###################################################################
# abc_optim(par, fn, D=length(par), ..., NP=40, FoodNumber = NP/2,
#           lb=-Inf, ub=+Inf, limit=100, maxCycle=1000, optiinteger=FALSE,
#           criter=50)
# Arguments:
# par: Initial values for the parameters to be optimized over
# fn:  A function to be minimized, with first argument of the vector of parameters over
#      which minimization is to take place. It should return a scalar result.
# D:   Number of parameters to be optimized.
# ...  Further arguments to be passed to ¡¦fn¡¦.
# NP:  Number of bees.
# FoodNumber: Number of food sources to exploit.
# lb: Lower bound of the parameters to be optimized.
# ub: Upper bound of the parameters to be optimized.
# limit: Limit of a food source.
# maxCycle: Maximum number of iterations.
# optiinteger: Whether to optimize binary parameters or not.
# criter: Stop criteria (numer of unchanged results) until stopping

install.packages("GA")
library("GA")

install.packages("ABCoptim")
library(ABCoptim)

# EXAMPLE: The minimum is at (pi,pi)
fun <- function(x) {
  -cos(x[1])*cos(x[2])*exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}

abc_optim(rep(0,2), fun, lb=-20, ub=20, criter=1000)



f <- function(x)  25-x*x
min <- -5
max <- 5
curve(f, min, max)

fitness <- function(x) f(x)
GA <- ga(type="real-valued",
fitness=fitness, 
min=min, 
max=max, 
popSize = 50,
pcrossover = 0.8,
pmutation = 0.1,
elitism = 10,
monitor = gaMonitor,
maxiter = 100)

plot(GA)


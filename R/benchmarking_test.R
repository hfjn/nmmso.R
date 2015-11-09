source("./R/nmmso_iterative.R")
source("./R/nmmso.R")
source("./R/cec_2015_problem_data.R")

set.seed = 20151102

# using the CEC test problems
#problem_used = niching_func

# test-function
fit <- function(x) sin(5 * pi * x)^6

eval_list = list()
c_list = list()
pop_size = list()
initial_flag = 0

acc = c(0.1, 0.01, 0.001, 0.0001, 0.00001)

count = matrix(0, 5, 1)

state = list()

evals = 0
index = 2

result = NMMSO(swarm_size = as.numeric(10*length(mx[2])), problem_function = fit, max_evaluations = gens[2], mn = as.numeric(mn[2]), mx = as.numeric(mx[2]))
source("./R/nmmso_iterative.R")
source("./R/nmmso.R")
source("./R/cec_2015_problem_data.R")

set.seed = 20151102

# still missing
cec_2015_problem_data

# using the CEC test problems
problem_used = niching_func

eval_list = list()
c_list = list()
pop_size = list()
initial_flag = 0

acc = c(0.1, 0.01, 0.001, 0.0001, 0.00001)

count = matrix(0, 5, 1)

state = list()

evals = 0

while(evals < gens(index) %% cound)
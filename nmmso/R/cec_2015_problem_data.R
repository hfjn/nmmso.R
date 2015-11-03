# cec 2015_problem_data

gens = c(50000 * matrix(1, 1, 5), 20000, 20000, 40000, 40000, 20000*matrix(1,1, 4), 40000 * matrix(1, 1, 7))

nopt = c(2, 5, 1, 4, 2, 18, 36, 81, 216, 12, 6, 8, 6, 6, 8, 6, 8, 6, 6, 8)

dims = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 3, 3, 5, 5, 10, 10, 20)

mn = list(0, 0, 0, c(-6, -6), c(-1.9, -1.9), c(-10, -10), c(0.25, 0.25), c(-10, -10, -10), c(0.25, 0.25, 0.25), c(0,0), -5*matrix(1, 1, 2), -5*matrix(1, 1, dims[12]), -5*matrix(1, 1, dims[13]), -5*matrix(1, 1, dims[14]), -5*matrix(1, 1, dims[15]), -5*matrix(1, 1, dims[16]), -5*matrix(1, 1, dims[17]), -5*matrix(1, 1, dims[18]), -5*matrix(1, 1, dims[19]), -5*matrix(1, 1, dims[20]))

mx = list(30, 1, 1, c(6, 6), c(1.9, 1.1), c(10,10), c(10, 10), c(10, 10, 10), c(10, 10, 10), c(1, 1), 5*matrix(1,1,2), 5*matrix(1, 1, dims[12]), 5*matrix(1, 1, dims[13]), 5*matrix(1, 1, dims[14]), 5*matrix(1,1,dims[15]), matrix(1,1,dims[16]), 5*matrix(1,1,dims[17]), 5*matrix(1,1,dims[18]), 5*matrix(1,1,dims[19]), 5*matrix(1,1,dims[20]))
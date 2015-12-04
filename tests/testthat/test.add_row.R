context("Add Row")

test_that("add_row is working fine for rows mit n values", {
	a <- 1:3
	b <- 3:1
	c <- rbind(a, NA, b)
	d <- add_row(rbind(a), 3, rbind(b))
	expect_true(c[1,] == d[1,] && c[3,] == d[3,])
})

test_that("add_row is working for rows with one value", {
	a <- 1
	b <- 3
	c <- rbind(a, NA, b)
	d <- add_row(rbind(a), 3, rbind(b))
	expect_true(c[1,] == d[1,] && c[3,] == d[3,])
})

test_that("add_row is working when it replaces one row", {
	a <- 1
	b <- 3
	d <- add_row(rbind(a), 1, rbind(b))
	expect_true(d == b)
})

test_that("add_row is working with multiple rows", {
	a <- matrix(1:3, 1)
	b <- matrix(1:6, 2)
	c <- rbind(a, b)
	d <- add_row(rbind(a), 2, rbind(b))
	expect_true(all.equal(c, d))
})

test_that("add_row is working when multiple rows are included with a gap", {
	a <- matrix(1:3, 1)
	b <- matrix(1:6, 2)
	c <- rbind(a, NA, NA, b)
	d <- add_row(rbind(a), 4, rbind(b))
	expect_true(all.equal(c, d))
})






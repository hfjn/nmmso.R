# context("get initial locations")
# test_that("get_initial_locations",{
#   nmmso_state = get_initial_locations(nmmso_state, as.numeric(mn[2]), as.numeric(mx[2]))
#   str(nmmso_state)
#   expect_true(nmmso_state$swarms[[1]]$new_location[1,1] > 0)
#   expect_true(nmmso_state$swarms_changed == 1)
# })
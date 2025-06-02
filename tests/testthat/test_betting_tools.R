

test_that("poisson_goal_grid returns a valid matrix", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_true(is.matrix(grid))
  expect_equal(dim(grid), c(27, 27))
  expect_equal(sum(grid), 1, tolerance = 1e-6)
})

test_that("asian_handicap_odds handles 0.5 lines correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_handicap_odds(grid, -0.5)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
  expect_true(abs(sum(odds) - 1) < 1e-6)
})

test_that("asian_handicap_odds handles 0.25 lines correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_handicap_odds(grid, -0.25)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
})

test_that("asian_total_odds handles 2.5 goals correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_total_odds(grid, 2.5)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
  expect_true(abs(sum(odds) - 1) < 1e-6)
})

test_that("asian_total_odds throws error for invalid lines", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_error(asian_handicap_odds(grid, 0.28))
})


test_that("asian_total_odds throws error for invalid lines", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_error(asian_total_odds(grid, 0.25))
})

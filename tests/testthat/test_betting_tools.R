

test_that("poisson_goal_grid returns a valid matrix", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_true(is.matrix(grid))
  expect_equal(sum(grid), 1, tolerance = 1e-6)
})

test_that("asian_handicap_odds handles 0.5 lines correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_handicap_odds(grid, -0.5, TRUE)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
  expect_true(abs(sum(odds) - 1) < 1e-6)
})

test_that("asian_handicap_odds handles 0.25 lines correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_handicap_odds(grid, -0.25, TRUE)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
})

test_that("asian_total_odds handles 2.5 goals correctly", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  odds <- asian_total_odds(grid, 2.5, TRUE)
  expect_true(is.numeric(odds))
  expect_equal(length(odds), 2)
  expect_true(abs(sum(odds) - 1) < 1e-6)
})

test_that("asian_total_odds throws error for invalid lines", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_error(asian_handicap_odds(grid, 0.28, TRUE))
})


test_that("asian_total_odds throws error for invalid lines", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_error(asian_total_odds(grid, 0.25, TRUE))
})




test_that("EM method removes equal margin correctly", {
  odds <- c(2.88, 3.00, 2.62)
  true_odds <- remove_vig("EM", odds)
  expect_equal(length(true_odds), 3)
  expect_true(all(true_odds > odds))  #
  implied_probs <- 1 / true_odds
  expect_equal(sum(implied_probs), 1)
})

test_that("MPTO method returns valid odds", {
  odds <- c(2.88, 3.00, 2.62)
  true_odds <- remove_vig("MPTO", odds)
  expect_equal(length(true_odds), 3)
  expect_true(all(true_odds > odds))
  implied_probs <- 1 / true_odds
  expect_equal(sum(implied_probs), 1)
})

test_that("OR method works for 2-way and 3-way", {
  odds_2way <- c(1.90, 1.90)
  odds_3way <- c(1.42, 5.29, 7.75)

  true_2way <- remove_vig("OR", odds_2way)
  true_3way <- remove_vig("OR", odds_3way)

  expect_equal(length(true_2way), 2)
  expect_equal(length(true_3way), 3)

  expect_true(all(true_2way > odds_2way))
  expect_true(all(true_3way > odds_3way))

  expect_equal(sum(1 / true_2way), 1)
  expect_equal(sum(1 / true_3way), 1)
})

test_that("LOG method returns true odds", {
  odds <- c(2.88, 3.00, 2.62)
  true_odds <- remove_vig("LOG", odds)
  expect_equal(length(true_odds), 3)
  expect_true(all(true_odds > odds))
  expect_equal(sum(1 / true_odds), 1)
})

test_that("Invalid method throws an error", {
  expect_error(remove_vig("INVALID", c(2, 2)))
})

test_that("Non-numeric odds throws an error", {
  expect_error(remove_vig("EM", c("a", "b")))
})

test_that("Too short odds vector throws an error", {
  expect_error(remove_vig("EM", c(2)))
})

test_that("OR method fails for more than 3-way", {
  expect_error(remove_vig("OR", c(2.0, 3.0, 4.0, 5.0)))
})




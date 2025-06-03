

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


test_that("asian_total_odds throws error for invalid lines", {
  grid <- poisson_goal_grid(exp_home = 2.6, exp_away = 1.6)
  expect_true(abs(sum(match_outcome_odds(grid, TRUE)) - 1) < 1e-6)
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


test_that("Decimal input is converted correctly", {
  res <- convert_odds(2.5, "decimal")
  expect_equal(res$american, 150)
  expect_equal(res$fractional, "3/2")
  expect_equal(res$prob, 0.4, tolerance = 1e-4)
})

test_that("American input (positive) is converted correctly", {
  res <- convert_odds(150, "american")
  expect_equal(res$decimal, 2.5)
  expect_equal(res$fractional, "3/2")
  expect_equal(res$prob, 0.4, tolerance = 1e-4)
})

test_that("American input (negative) is converted correctly", {
  res <- convert_odds(-200, "american")
  expect_equal(res$decimal, 1.5)
  expect_equal(res$fractional, "1/2")
  expect_equal(res$prob, 2/3, tolerance = 1e-4)
})

test_that("Fractional input is converted correctly", {
  res <- convert_odds("5/2", "fractional")
  expect_equal(res$decimal, 3.5)
  expect_equal(res$american, 250)
  expect_equal(res$prob, 1 / 3.5, tolerance = 1e-4)
})

test_that("Implied probability input is converted correctly", {
  res <- convert_odds(0.4, "prob")
  expect_equal(res$decimal, 2.5)
  expect_equal(res$american, 150)
  expect_equal(res$fractional, "3/2")
})

test_that("Invalid fractional format throws error", {
  expect_error(convert_odds("5:2", "fractional"))
  expect_error(convert_odds("abc", "fractional"))
})

test_that("Decimal <= 1 throws error", {
  expect_error(convert_odds(1.00, "decimal"))
  expect_error(convert_odds(0.5, "decimal"))
})

test_that("Negative or zero probability throws error", {
  expect_error(convert_odds(0, "prob"))
  expect_error(convert_odds(-0.1, "prob"))
})






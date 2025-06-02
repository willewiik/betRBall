
#' Generate Poisson goal grid
#'
#' Creates a matrix of probabilities for different match scorelines based on Poisson-distributed expected goals.
#'
#' @param exp_home Expected goals for the home team.
#' @param exp_away Expected goals for the away team.
#' @param max_goals Maximum number of goals to consider per team.
#'
#' @return A matrix of probabilities for each possible scoreline.
#' @export
poisson_goal_grid <- function(exp_home, exp_away, max_goals = 30) {
  grid <- matrix(NA, max_goals, max_goals)
  colnames(grid) <- 0:(max_goals - 1)
  rownames(grid) <- 0:(max_goals - 1)

  prob_home <- stats::dpois(0:(max_goals - 1), exp_home)
  prob_away <- stats::dpois(0:(max_goals - 1), exp_away)

  for (i in 1:max_goals) {
    for (j in 1:max_goals) {
      grid[i, j] <- prob_home[j] * prob_away[i]
    }
  }
  return(grid)
}



#' Calculate Asian Handicap Odds Spread
#'
#' Calculates the probabilities or odds for Asian handicap betting lines.
#'
#' @param grid A matrix of scoreline probabilities.
#' @param line The Asian handicap line (e.g., -0.5, -1.25).
#' @param probs Logical; if TRUE returns probabilities, if FALSE returns odds (default FALSE).
#'
#' @return A numeric vector with two elements: home team and away team probabilities or odds.
#' @export
asian_handicap_odds <- function(grid, line, probs = FALSE) {
  reverse <- FALSE
  if (line > 0) {
    line <- -line
    reverse <- TRUE
    grid <- t(grid)
  }

  if (abs(line) %% 1 == 0) {
    push <- grid[row(grid) - col(grid) == line]
    upper <- grid[row(grid) < col(grid) - (-line)]
    home_prob <- sum(upper) / (1 - sum(push))
    away_prob <- 1 - home_prob

  } else if (abs(line) %% 1 == 0.5) {
    offset <- (-line) - 0.5
    upper <- grid[row(grid) < col(grid) - offset]
    home_prob <- sum(upper)
    away_prob <- 1 - home_prob

  } else if (abs(line) %% 1 == 0.25) {
    upper_half <- grid[row(grid) < col(grid) - ((-line) - 0.25)]
    push <- grid[row(grid) - col(grid) == (line + 0.25)]
    home_whole_ball <- sum(upper_half) / (1 - sum(push))
    home_prob <- (sum(upper_half) * 0.5 + home_whole_ball * 0.5)
    away_prob <- 1 - home_prob

  } else if (abs(line) %% 1 == 0.75) {
    upper_half <- grid[row(grid) < col(grid) - ((-line) - 0.75)]
    push <- grid[row(grid) - col(grid) == (line - 0.25)]
    upper_temp <- grid[row(grid) < col(grid) - ((-line) + 0.25)]
    home_whole_ball <- sum(upper_temp) / (1 - sum(push))
    home_prob <- (sum(upper_half) * 0.5 + home_whole_ball * 0.5)
    away_prob <- 1 - home_prob

  } else {
    stop("Invalid line")
  }

  res <- if (reverse) c(away_prob, home_prob) else c(home_prob, away_prob)

  if (!probs) {
    res <- 1 / res
  }
  return(res)
}


#' Calculate Asian Total Goal Odds
#'
#' Calculates the probabilities or odds for Asian total betting lines (over/under).
#'
#' @param grid A matrix of scoreline probabilities.
#' @param line The total goals line (e.g., 2.5, 3.25).
#' @param probs Logical; if TRUE returns probabilities, if FALSE returns odds (default FALSE).
#'
#' @return A numeric vector with two elements: over and under probabilities or odds.
#' @export
asian_total_odds <- function(grid, line, probs = FALSE) {
  if (line <= 0.49) stop("Wrong line")

  sum_goals <- (row(grid) - 1) + (col(grid) - 1)

  if (abs(line) %% 1 == 0) {
    push <- grid[sum_goals == line]
    over <- sum(grid[sum_goals > (line + 0.5)])
    over <- over / (1 - sum(push))
    under <- 1 - over

  } else if (abs(line) %% 1 == 0.5) {
    over <- sum(grid[sum_goals > line])
    under <- 1 - over

  } else if (abs(line) %% 1 == 0.25) {
    upper_half <- sum(grid[sum_goals > (line + 0.25)])
    push <- grid[sum_goals == (line - 0.25)]
    over_whole_ball <- upper_half / (1 - sum(push))
    over <- (upper_half * 0.5 + over_whole_ball * 0.5)
    under <- 1 - over

  } else if (abs(line) %% 1 == 0.75) {
    upper_half <- sum(grid[sum_goals > (line - 0.25)])
    push <- grid[sum_goals == (line + 0.25)]
    upper_temp <- sum(grid[sum_goals > (line + 0.75)])
    over_whole_ball <- upper_temp / (1 - sum(push))
    over <- (upper_half * 0.5 + over_whole_ball * 0.5)
    under <- 1 - over

  } else {
    stop("Invalid line")
  }

  res <- c(over, under)

  if (!probs) {
    res <- 1 / res
  }
  return(res)
}







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





#' Remove the bookmaker's margin from quoted odds
#'
#' This function estimates the 'true' odds (i.e., without the bookmaker's margin or vig)
#' based on one of four methods described in betting literature:
#' \itemize{
#'   \item \code{"EM"} — Equal margin across all outcomes.
#'   \item \code{"MPTO"} — Margin proportional to odds.
#'   \item \code{"OR"} — Odds Ratio method (supports 2- or 3-way markets).
#'   \item \code{"LOG"} — Logarithmic function method.
#' }
#'
#' The function is suitable for both 2-outcome (e.g. tennis) and 3-outcome (e.g. football) betting markets.
#'
#' @param method Character. The method used to remove the margin. One of "EM", "MPTO", "OR", "LOG".
#' @param odds Numeric vector. A vector of odds (decimal format).
#'
#' @return A numeric vector of 'true' odds with the bookmaker's margin removed.
#'
#' @examples
#' #' # Football match with odds for Home, Draw, Away
#' remove_vig("MPTO", c(2.88, 3.00, 2.62))
#'
#' # Tennis match with equal odds
#' remove_vig("OR", c(1.90, 1.90))
#'
#' # Logarithmic method
#' remove_vig("LOG", c(2.88, 3.00, 2.62))
#'
#' @references
#' Buchdahl, J. (2015). *The Wisdom of the Crowd to Find Value in a Football Match Betting Market*.
#' Retrieved from \url{http://www.football-data.co.uk/wisdom_of_crowd_bets}
#'
#' @export
remove_vig <- function(method = "EM", odds) {
  if (!(method %in% c("MPTO", "OR", "LOG", "EM"))) stop("No valid method")
  if (!is.numeric(odds)) stop("Odds needs to be numeric")
  if (length(odds) < 2) stop("Odds needs to be at least of length 2")

  implied_probs <- 1 / odds
  overround <- sum(implied_probs)
  margin <- overround - 1

  if (method == "EM") {
    true_probs <- implied_probs / overround
    return(1 / true_probs)

  } else if (method == "MPTO") {
    true_odds <- numeric(length(odds))
    n <- length(odds)
    for (i in seq_along(odds)) {
      true_odds[i] <- (n * odds[i]) / (n - margin * odds[i])
    }
    return(true_odds)

  } else if (method == "OR") {
    if (length(odds) == 2) {
      x <- implied_probs[1]
      y <- implied_probs[2]

      f <- function(c) {
        p <- x / (c + x - c * x)
        q <- y / (c + y - c * y)
        return(p + q - 1)
      }
      c_est <- stats::uniroot(f, c(0.001, 100))$root
      true_probs <- c(
        x / (c_est + x - c_est * x),
        y / (c_est + y - c_est * y)
      )
      true_probs <- true_probs / sum(true_probs)  # Normalize in case of numerical errors
      return(1 / true_probs)
    }

    if (length(odds) == 3) {
      # 3-outcome OR model (football)
      f <- function(c) {
        sum(implied_probs / (c + implied_probs - c * implied_probs)) - 1
      }
      c_est <- stats::uniroot(f, c(0.001, 100), tol = 1e-10)$root
      true_probs <- implied_probs / (c_est + implied_probs - c_est * implied_probs)
      true_probs <- true_probs / sum(true_probs)  # Normalize in case of numerical errors
      return(1 / true_probs)
    }

    stop("OR method currently only supports 2- or 3-way odds")

  } else if (method == "LOG") {
    # Solve for n such that sum((implied_probs)^(1/n)) = 1
    f <- function(n) sum(implied_probs^(1/n)) - 1
    n_est <- uniroot(f, c(0.001, 10))$root
    true_probs <- implied_probs^(1 / n_est)
    true_probs <- true_probs / sum(true_probs)  # Normalize in case of numerical errors
    return(1 / true_probs)
  }
}











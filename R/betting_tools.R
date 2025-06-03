
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



#' Calculate Match Outcome Probabilities
#'
#' @param grid A matrix of scoreline probabilities.
#' @param probs Logical; if TRUE returns probabilities, if FALSE returns odds (default FALSE).
#' @return Numeric vector: c(home_win, draw, away_win)
#' @export
match_outcome_odds <- function(grid, probs = FALSE) {
  home_win <- sum(grid[row(grid) > col(grid)])
  draw <- sum(diag(grid))
  away_win <- sum(grid[row(grid) < col(grid)])
  if (probs) {
    return(c(home_win, draw, away_win))
  } else {
    return(c(1/home_win, 1/draw, 1/away_win))
  }
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





#' Convert Between Odds Formats
#'
#' Converts odds between decimal, American, fractional, and implied probability formats.
#'
#' @param x The input value (numeric or character). For fractional, use string format like "5/2".
#' @param from The input format. One of: "decimal", "american", "fractional", "prob".
#'
#' @return A named list with all formats: decimal, american, fractional, and prob.
#'
#' @examples
#' convert_odds(2.50, from = "decimal")
#' convert_odds(-200, from = "american")
#' convert_odds("5/2", from = "fractional")
#' convert_odds(0.40, from = "prob")
#' @export
convert_odds <- function(x, from = c("decimal", "american", "fractional", "prob")) {
  from <- match.arg(from)

  # Convert to decimal
  decimal <- switch(from,
                    decimal = as.numeric(x),
                    american = ifelse(x >= 100, (x / 100) + 1, ifelse(x <= -100, (100 / abs(x)) + 1, NA)),
                    fractional = {
                      parts <- strsplit(x, "/")[[1]]
                      if (length(parts) != 2) stop("Invalid fractional format. Use '5/2', '3/1', etc.")
                      (as.numeric(parts[1]) / as.numeric(parts[2])) + 1
                    },
                    prob = 1 / as.numeric(x)
  )

  prob <- switch(from,
                 prob = {
                   x <- as.numeric(x)
                   if (x <= 0 || x >= 1) stop("Probability must be between 0 and 1 (exclusive).")
                   x
                 },
                 decimal = 1 / as.numeric(x),
                 american = NA,
                 fractional = NA
  )

  if (is.na(decimal) || decimal <= 1) stop("Invalid odds or format.")

  # Compute all formats
  prob <- 1 / decimal
  american <- if (decimal >= 2) {
    round((decimal - 1) * 100)
  } else {
    round(-100 / (decimal - 1))
  }

  frac <- {
    # Reduce fractional representation
    rational <- MASS::fractions(decimal - 1)
    as.character(rational)
  }

  list(
    decimal = round(decimal, 4),
    american = american,
    fractional = frac,
    prob = round(prob, 4)
  )
}










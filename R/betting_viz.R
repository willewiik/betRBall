if(getRversion() >= "2.15.1")  utils::globalVariables(c("bet_number", "simulation", "..density.."))


#' Simulate Betting Outcomes
#'
#' This function simulates the cumulative profit over a specified number of bets and simulations,
#' allowing for the calculation of expected profit based on given odds and stake.
#'
#' @param n_bets An integer specifying the number of bets to simulate.
#' @param n_sim An integer specifying the number of simulations to perform.
#' @param edge A numeric value representing the edge you have (e.g., 1.10 for 10 percent edge).
#' @param mean_stake A numeric value for the mean stake amount per bet, default is 100.
#' @param same_odds A numeric value or vector to set the same odds for all bets, default is NULL.
#'
#' @return A list with:
#' \item{profit_plot}{A ggplot object of cumulative profit across bets.}
#' \item{odds_plot}{A ggplot object of the odds distribution.}
#'
#' @examples
#' sim_bets(n_bets = 1000, n_sim = 20, edge = 1.05)
#' @export
sim_bets <- function(n_bets, n_sim, edge, mean_stake = 100, same_odds = NULL) {
  if (n_sim > 10000) stop("Too many simulations; maximum is 10,000.")
  if (n_bets > 100000) stop("Too many bets; maximum is 100,000.")

  shape_param <- 1
  scale_param <- 1
  offset <- 1.25

  odds <- if (is.null(same_odds)) {
    stats::rgamma(n_bets, shape = shape_param, scale = scale_param) + offset
  } else {
    rep(same_odds, n_bets)
  }

  true_prob <- edge * (1 / odds)

  sim_mat <- matrix(NA, nrow = n_bets, ncol = n_sim)

  for (sim in seq_len(n_sim)) {
    profit <- numeric(n_bets)
    for (bet in seq_len(n_bets)) {
      result <- sample(
        c((odds[bet] * mean_stake) - mean_stake, -mean_stake),
        size = 1,
        prob = c(true_prob[bet], 1 - true_prob[bet])
      )
      profit[bet] <- result + ifelse(bet == 1, 0, profit[bet - 1])
    }
    sim_mat[, sim] <- profit
    if (sim %% 50 == 0) cat(sprintf("Completed %d of %d simulations\n", sim, n_sim))
  }

  odds_df <- data.frame(odds = odds)
  odds_plot <- ggplot2::ggplot(odds_df, ggplot2::aes(x = odds)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), binwidth = 0.3, fill = "skyblue", color = "black", alpha = 0.8) +
    ggplot2::geom_density(color = "red", alpha = 0.5) +
    ggplot2::labs(
      title = "Histogram of Odds Played",
      subtitle = sprintf("Mean: %.2f, Min: %.2f, Max: %.2f", mean(odds), min(odds), max(odds)),
      x = "Odds", y = "Density"
    ) +
    ggplot2::theme_bw()

  profit_df <- as.data.frame(sim_mat)
  profit_df$bet_number <- seq_len(n_bets)

  profit_long <- tidyr::pivot_longer(profit_df, cols = -bet_number,
                                     names_to = "simulation", values_to = "profit")

  profit_plot <- ggplot2::ggplot(profit_long, ggplot2::aes(x = bet_number, y = profit, group = simulation)) +
    ggplot2::geom_line(color = "#7CB9E8", alpha = 0.4) +
    ggplot2::labs(
      title = sprintf("Profit over %d Bets", n_bets),
      subtitle = sprintf("%d simulations | Edge = %.2f | Mean stake = %d EUR | Mean odds = %.2f",
                         n_sim, edge, mean_stake, mean(odds)),
      x = "Number of Bets", y = "Cumulative Profit"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(suffix = " EUR")) +
    ggplot2::theme_bw()

  mean_profit <- rowMeans(sim_mat)
  ucl <- apply(sim_mat, 1, function(x) stats::quantile(x, 0.95))
  lcl <- apply(sim_mat, 1, function(x) stats::quantile(x, 0.05))

  line_df <- data.frame(
    bet_number = seq_len(n_bets),
    mean_profit = mean_profit,
    ucl = ucl,
    lcl = lcl
  )

  profit_plot <- profit_plot +
    ggplot2::geom_line(ggplot2::aes(x = bet_number, y = mean_profit), data = line_df, inherit.aes = FALSE, color = "black", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(x = bet_number, y = ucl), data = line_df, inherit.aes = FALSE, color = "red", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(x = bet_number, y = lcl), data = line_df, inherit.aes = FALSE, color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "darkgreen", linetype = "dotted")

  final_profits <- sim_mat[n_bets, ]
  cat("\nSimulation Summary:\n")
  cat(sprintf("- Profit > 0 EUR: %.2f%% of simulations\n", mean(final_profits > 0) * 100))
  cat(sprintf("- Max profit: %.2f EUR\n", max(final_profits)))
  cat(sprintf("- Min profit: %.2f EUR\n", min(final_profits)))

  list(profit_plot = profit_plot, odds_plot = odds_plot)
}

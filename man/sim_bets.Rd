% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betting_viz.R
\name{sim_bets}
\alias{sim_bets}
\title{Simulate Betting Outcomes}
\usage{
sim_bets(
  n_bets,
  n_sim,
  edge,
  mean_stake = 100,
  same_odds = NULL,
  flat = FALSE,
  min_odds = 1,
  max_odds = Inf
)
}
\arguments{
\item{n_bets}{Integer. Number of bets to simulate (max 100,000).}

\item{n_sim}{Integer. Number of simulations to perform (max 10,000).}

\item{edge}{Numeric. Your edge on each bet (e.g., 1.10 for 10 percent edge).}

\item{mean_stake}{Numeric. Mean stake amount per bet. If \code{flat = FALSE}, this represents the average potential *return* from each bet. Default is 100.}

\item{same_odds}{Optional numeric. If provided, all bets use the same odds. Otherwise, odds are sampled from a gamma distribution.}

\item{flat}{Logical. If \code{TRUE}, each bet uses a flat stake equal to \code{mean_stake}. If \code{FALSE}, the stake is calculated so the potential win equals \code{mean_stake}.}

\item{min_odds}{Numeric. Minimum allowed odds (default is 1).}

\item{max_odds}{Numeric. Maximum allowed odds (default is \code{Inf}).}
}
\value{
A list with:
\item{profit_plot}{A ggplot object of cumulative profit across bets.}
\item{odds_plot}{A ggplot object of the odds distribution.}
}
\description{
This function simulates the cumulative profit over a specified number of bets and simulations,
allowing for analysis of the impact of betting edges, stake sizing (flat or proportional),
and random or fixed odds. It returns plots of profit and odds distribution
}
\examples{
sim_bets(n_bets = 1000, n_sim = 20, edge = 1.05)
}

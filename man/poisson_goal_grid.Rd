% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betting_tools.R
\name{poisson_goal_grid}
\alias{poisson_goal_grid}
\title{Generate Poisson goal grid}
\usage{
poisson_goal_grid(exp_home, exp_away, max_goals = 30)
}
\arguments{
\item{exp_home}{Expected goals for the home team.}

\item{exp_away}{Expected goals for the away team.}

\item{max_goals}{Maximum number of goals to consider per team.}
}
\value{
A matrix of probabilities for each possible scoreline.
}
\description{
Creates a matrix of probabilities for different match scorelines based on Poisson-distributed expected goals.
}

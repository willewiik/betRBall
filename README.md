
<!-- badges: start -->
[![R-CMD-check](https://github.com/willewiik/betRBall/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/willewiik/betRBall/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# betRBall

**betRBall** is an R package for football betting calculations and visualizations.  

## 📦 Included Functions

Currently, the package contains the following core functions:

- `poisson_goal_grid()`: Generates a matrix of goal outcome probabilities for two teams using Poisson distribution.
- `asian_handicap_odds()`: Calculates Asian handicap odds (e.g., -0.75, +1.5) using a matrix of goal outcome probabilities such as from `poisson_goal_grid()`.
- `asian_total_odds()`: Calculates odds for Asian total goals lines (e.g., over/under 2.5, 3.25) using a matrix of goal outcome probabilities.
- `remove_vig()`: Estimates the 'true' odds (i.e., without the bookmaker's margin or vig)
- `sim_bets()`: Simulates betting outcomes to estimate expected profit and variance.



## 🔧 Installation

You can install the development version of `betRBall` from GitHub with:

``` r
devtools::install_github("willewiik/betRBall")
```

## 📊 Betting calculations

```{r example}
library(betRBall)

# Generate a Poisson goal probability grid with expected goals for home and away teams
grid <- poisson_goal_grid(exp_home = 1.5, exp_away = 1.2)

# Calculate Asian handicap odds for line -0.25
asian_handicap_odds(grid = grid, line = -0.25)
# [1] 1.934441 2.070158

# Calculate Asian total goals odds for line 3.25
asian_total_odds(grid = grid, line = 3.25)
[1] 3.064306 1.484424
```

### 📊 Remove margin

Below are examples of how to use the `remove_vig()` function to estimate "true" odds by removing the bookmaker's margin.

More info about the methods can be found in Joseph Buchdahl's excellent paper:
**[The Wisdom of the Crowd – Removing the Margin](http://www.football-data.co.uk/wisdom_of_crowd_bets)**

```{r example}

# Equal Margin
remove_vig("EM", c(1.28, 4.02))
[1] 1.318408 4.140625

# Margin Proportional to Odds 
remove_vig("MPTO", c(1.28, 4.02))
[1] 1.305062 4.278018

# Odds Ratio
remove_vig("OR", c(1.28, 4.02))
[1] 1.304493 4.284152

# Logarithmic Method
remove_vig("LOG", c(1.28, 4.02))
[1] 1.298212 4.353315

# Equal Margin 3-way
remove_vig("EM", c(2.88, 3.00, 2.62))
[1] 3.059237 3.186705 2.783056


```



## 📊 Simulate betting outcomes

```{r example}
library(betRBall)

# Run a simulation with:
# - 1000 bets
# - 250 simulations
# - 5% edge 
# - max odds of 4 
# - proportional stake size to the odds
result <- sim_bets(n_bets = 1000, n_sim = 250, edge = 1.05, flat = FALSE, max_odds = 4)

# Plot the cumulative profit across all simulations
result[["profit_plot"]]

# Plot the distribution of odds used in the simulations
result[["odds_plot"]]

```

## 📈 Plot outputs

Plot outputs for the function `sim_bets()`

### Profit Plot
![Profit Plot](man/plots/profit_plot.png)

### Odds Distribution Plot
![Odds Plot](man/plots/odds_plot.png)
















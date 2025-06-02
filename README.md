
<!-- badges: start -->
[![R-CMD-check](https://github.com/willewiik/betRBall/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/willewiik/betRBall/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# betRBall

An R package for football betting calculations.


## Installation

You can install the development version of `betRBall` from GitHub with:

``` r
devtools::install_github("willewiik/betRBall")
```

## Example

```{r example}
library(betRBall)

# Generate a Poisson goal probability grid with expected goals for home and away teams
grid <- poisson_goal_grid(exp_home = 1.5, exp_away = 1.2)

# Calculate Asian handicap odds for line -0.25
handicap_probs <- asian_handicap_odds(grid = grid, line = -0.25)
print(handicap_probs)

# Calculate Asian total goals odds for line 2.5
total_probs <- asian_total_odds(grid = grid, line = 2.5)
print(total_probs)
```


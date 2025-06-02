

## Installation

You can install the development version of `betRBall` like so:

``` r
devtools::install_github("ditt-användarnamn/betRBall")
```

## Example

```{r example}
library(betRBall)

grid <- poisson_goal_grid(exp_home = 1.5, exp_away = 1.2)
asian_handicap_odds(grid = grid, line = -0.25)
asian_total_odds(grid = grid, line = 2.5)

```


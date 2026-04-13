# APMA Math Arcade

A [Shiny](https://shiny.posit.co/) app that collects interactive math and statistics demos all in one place: features a carousel style home screen with eight small game demos with short explanations.

## Demos

| Demo | Topic |
|------|--------|
| Monty Hall | Probability — switch or stay? |
| Fourier Drawing | Draw a curve; animated epicycles / coefficients |
| Dating Number | Optimal stopping (secretary problem) |
| Plinko | Central limit theorem / normal distribution |
| Buffon's Needle | Monte Carlo estimate of π |
| Poker Hands | Empirical hand frequencies vs theory |
| Blackjack | Play 21 with chips and basic outcomes |
| European Roulette | Wheel + number / outside bets |

Client-side animation and games live in `www/games.js`; the Fourier coefficient table is computed in R (`app.R`) from points sent from the browser.

## Project layout

| Path | Purpose |
|------|---------|
| `app.R` | UI, server, CSS strings, game metadata, Fourier R logic |
| `www/games.js` | Navigation and in-browser game implementations |
| `www/img/games/*.png` | Carousel thumbnails (replace with your own art as needed) |
| `renv.lock` | Locked package versions — **commit this** for reproducibility |
| `renv/` | renv bootstrap (library is usually not committed — see `.gitignore`) |

## Requirements

- **R** (version recorded in `renv.lock`; use the same or compatible major release when possible)
- Packages are managed with **[renv](https://rstudio.github.io/renv/)**; see `renv.lock`

Core app dependencies include **shiny** and **jsonlite**.

## Setup

Clone or download this repository, then in R:

```r
setwd("/path/to/my_shiny_app")
# renv activates automatically if you open the folder as an RStudio Project,
# or run:
source("renv/activate.R")
renv::restore()
```

If `restore()` prompts for a mirror, use e.g. `repos = "https://cloud.r-project.org"` when installing packages manually.

## Run locally

From the project root:

```r
shiny::runApp()
```

Or in a shell:

```bash
Rscript -e "shiny::runApp('/path/to/my_shiny_app')"
```

The app serves static files from `www/` (e.g. `www/games.js`, carousel images under `www/img/games/`).

## License

This project is licensed under the [MIT License](LICENSE).

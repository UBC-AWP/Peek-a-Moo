<!-- badges: start -->
[![deploy](https://github.com/UBC-AWP/Peek-a-Moo/actions/workflows/deploy.yml/badge.svg?branch=main)](https://github.com/UBC-AWP/Peek-a-Moo/actions/workflows/deploy.yml) [![prod-release](https://github.com/UBC-AWP/Peek-a-Moo/actions/workflows/prod-release.yml/badge.svg?branch=main)](https://github.com/UBC-AWP/Peek-a-Moo/actions/workflows/prod-release.yml) 
<!-- badges: end -->

# Peek-a-Moo
Dairy Cow Dashboard for UBC Faculty of Land and Food Systems - Animal Welfare Program

# Dependenices
Run the following command in R to install the dependencies:

`install.packages(
c("tidyverse",
"shiny",
"shinydashboard",
"shinyWidgets",
"shinycssloaders",
"plotly",
"visNetwork",
"lubridate",
"DT",
"here",
"tidygraph",
"memoise",
"png"))`

# Launching the app
From the root directory, run: 

`shiny::runApp("dashboard")`

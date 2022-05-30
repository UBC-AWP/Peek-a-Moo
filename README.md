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
"tidygraph",
"memoise",
"png",
"googleCloudStorageR"))`

# Preparing the codebase to run locally
In `dashboard/server.R` comment lines 3, 19 and 20 and uncomment lines 5-10 and 18.

The code should look like this when completed:
```{r}
library(shinymanager)

# passphrase <- Sys.getenv("PASSPHRASE")

credentials <- data.frame(
  user = c("guest", "user", "admin"), # mandatory
  password = c("guest", "shiny", "shinymanager"), # mandatory
  admin = c(FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Set up shiny server
server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      credentials
      # "../auth/database.sqlite",
      # passphrase = passphrase
    )
  )
```

Comment out lines 40-60 in `dashboard/global.R`.
```{r, attr.source='.numberLines startFrom="39"'}
# download data from GCP
# gcs_auth(json_file = '../auth/peek-a-moo.json')
# 
# gcs_global_bucket("peek-a-moo-data")
# 
# objects <- gcs_list_objects()
# download_list <- grep("*.Rda", objects$name, value = TRUE)
# 
# if (!dir.exists("../data/")) {
#   dir.create("../data/")
#   map(download_list, function(x) gcs_get_object(x,
#     saveToDisk = paste('../data/', gsub(".*/","",x), sep = ""),
#     overwrite = TRUE))
# }
# 
# check_files = list.files('../data/')
# 
# if (!length(check_files) > 0) {
#   map(download_list, function(x) gcs_get_object(x,
#     saveToDisk = paste('../data/', gsub(".*/","",x), sep = ""),
#     overwrite = TRUE))
# }
```

Load your data into the `data` folder. The minimum files and structure needed to run the application are outlined below.
```
Peek-a-Moo/
├── dashboard/
│   ├── global.R
│   ├── run.R
│   ├── server.R
│   ├── ui.R
│   └── www/
├── data/
│   ├── Wali_trial_summarized_THI.Rda
│   ├── Feeding_and_drinking_analysis.Rda
│   ├── Insentec_warning.Rda
│   ├── duration_for_each_bout.Rda
│   ├── lying_standing_summary_by_date.Rda
│   ├── synchronized_lying_total_time.Rda
│   ├── Cleaned_drinking_original_data.Rda
│   ├── Cleaned_feeding_original_data.Rda
│   ├── non_nutritive_visits.Rda
│   ├── feed_replacement_10mon_CD.Rda
│   ├── bin_empty_total_time_summary.Rda
│   ├── Feeding_drinking_at_the_same_time_total_time.Rda
│   ├── Feeding_drinking_neighbour_total.Rda
│   ├── Replacement_behaviour_by_date.Rda
│   └── _10-mon__elo_all_replacements_long_noNA.Rda
└── R/
    ├── activites.R
    ├── bins.R
    ├── bully_analysis.R
    ├── daily_behavior.R
    ├── elo.R
    ├── network.R
    ├── notifications.R
    └── THI_analysis.R
```

# Launching the app
From the root directory `Peek-a-Moo/`, run: 

`shiny::runApp("dashboard")`

Use the following log-in credentials to access the dashboard.

| User Name |   Password   |
|:---------:|:------------:|
|   guest   |     guest    |
|    user   |     shiny    |
|   admin   | shinymanager |

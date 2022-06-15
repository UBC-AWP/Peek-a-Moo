library(Rcpp)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(igraph)
library(lubridate)
library(png)
library(visNetwork)
library(googleCloudStorageR)
library(reshape2)
library(shinyBS)
library(rmarkdown)
library(rstan)
library(rstantools)
library(shinyalert)
library(tibble)
library(stats)
library(graphics)
library(ggplot2)
library(akima)



# load in plot/table creation scripts
source("../R/notifications.R")
source("../R/behaviour.R")
source("../R/daily_behavior.R")
source("../R/network.R")
source("../R/elo.R")
source("../R/bully_analysis.R")
#source("../R/bins.R")
source("../R/THI_analysis.R")
source("../R/FAQ.R")

#' Helper function for converting dataframes to having dates in a single column
#'
#' @param df The dataframe to convert
#'
#' @return Dataframe with dates in a single date column
convert_date_col <- function(df) {
  tibble::enframe(df, name = "date") %>%
    mutate(date = as.Date(date)) %>%
    unnest(value)
}

# download data from GCP
gcs_auth(json_file = '../auth/peek-a-moo.json')

gcs_global_bucket("peek-a-moo-data")

objects <- gcs_list_objects()
download_list <- grep("*.Rda", objects$name, value = TRUE)

if (!dir.exists("../data/")) {
  dir.create("../data/")
  map(download_list, function(x) gcs_get_object(x,
    saveToDisk = paste('../data/', gsub(".*/","",x), sep = ""),
    overwrite = TRUE))
}

check_files = list.files('../data/')

if (!length(check_files) > 0) {
  map(download_list, function(x) gcs_get_object(x,
    saveToDisk = paste('../data/', gsub(".*/","",x), sep = ""),
    overwrite = TRUE))
}

# load data if not already in memory
if (!exists("THI")) {
  load("../data/Wali_trial_summarized_THI.Rda")
  load("../data/Feeding_and_drinking_analysis.Rda")
  load("../data/Insentec_warning.Rda")
  load("../data/duration_for_each_bout.Rda")
  load("../data/lying_standing_summary_by_date.Rda")
  load("../data/synchronized_lying_total_time.Rda")
  load("../data/Cleaned_drinking_original_data.Rda")
  load("../data/Cleaned_feeding_original_data.Rda")
  load("../data/non_nutritive_visits.Rda")
  load("../data/feed_replacement_10mon_CD.Rda")
  load("../data/bin_empty_total_time_summary.Rda")
  load("../data/Feeding_drinking_at_the_same_time_total_time.Rda")
  load("../data/Feeding_drinking_neighbour_total.Rda")
  load("../data/Feeding_drinking_neighbour_bout.Rda")
  load("../data/Replacement_behaviour_by_date.Rda")
  load("../data/_10-mon__elo_all_replacements_long_noNA.Rda")

  THI <- master_summary

  rm(master_summary)
}

# create dataframes for plots and tables
standing_bout_df <- lying_standing_summary_by_date
feed_drink_df <- Feeding_and_drinking_analysis
non_nutritive_df <- convert_date_col(non_nutritive_visits)
feeding_intake_df <- Feeding_and_drinking_analysis
feed_df <- convert_date_col(Cleaned_feeding_original_data)
replacement_df <- master_feed_replacement_all
dominance_df <- elo_24h_na_filled

max_date <- max(feed_drink_df[["date"]])
min_date <- min(feed_drink_df[["date"]])

#' Helper function to filter by user input date range
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param date_obj A list containing start and end date or a single date
#'
#' @return Filtered dataframe with only dates within selected range
filter_dates <- function(df, col, date_obj) {
  if (is.null(date_obj)) date_obj <- list(min_date, max_date)

  if (length(date_obj) > 1) {
    df %>%
      filter({{ col }} >= date_obj[[1]]) %>%
      filter({{ col }} <= date_obj[[2]])
  } else {
    df %>%
      filter({{ col }} == date_obj)
  }
}

filter_cd <- function(df, col, cd_range) {
  df %>%
    filter({{ col }} >= cd_range[[1]]) %>%
    filter({{ col }} <= cd_range[[2]])
}

#' Helper function to filter by user input cow selection
#'
#' @param df The dataframe to filter
#' @param col The cow column
#' @param cow_selection A list of cows Ids to display
#'
#' @return Filtered dataframe with only selected cows
filter_cows <- function(df, col, cow_selection) {
  df %>%
    filter({{ col }} %in% cow_selection)
}

#' Helper function for creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#' @param width The width of the box, defaults to 6
#' @param output_fun Function for producing the plot output, defaults to plotlyOutput
#'
#' @return tabBox
default_tabBox <- function(title, var_name, width = 6, height = "500px", output_fun = plotlyOutput, popover = NULL) {
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    height = height,
    popover,
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow_table.gif",
      DT::dataTableOutput(paste0(var_name, "_table"))
    )),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = paste0("loading_cow6.gif"),
      output_fun(paste0(var_name, "_plot"))
    ))
  )
}

report_tabBox <- function(title, var_name, width = 6, height = "500px", output_fun = plotlyOutput, popover = NULL) {
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    height = height,
    popover,
    tabPanel("Analysis", helpText("Select a cow of interest to generate a Bayesian analysis report on \"Feeding Neighbours\". Note: generating a report will take more than 4 minutes, as a Markov chain Monte Carlo simulation is running under the hood."),
             cow_selection_widget("analysis_cow_id", multiple = FALSE, label = "Cow of Interest"),
             download_format_widget("analysis_format"),
             downloadButton('downloadReport')),
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow_table.gif",
      DT::dataTableOutput(paste0(var_name, "_table"))
    )),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = paste0("loading_cow6.gif"),
      output_fun(paste0(var_name, "_plot"))
    ))
  )
}



#' Helper function to format tables with export option
#'
#' @param df The dataframe to convert
#' @param page_length Number of pages to show, defaults to 5
#'
#' @return DT datatable
format_dt_table <- function(df, page_length = 5, data_config) {
  if ((length(data_config) == 3 && data_config[[3]] == "t") || is.null(data_config)) {
    . <- c("Data access is limited to Admin and user only.")
    df <- data.frame(.)

    DT::renderDataTable(
      df,
      server = FALSE,
      options = data_config
    )
  } else if ((length(data_config) == 3 && data_config[[3]] == "Bftip")) {
    DT::renderDataTable(
      df,
      server = FALSE,
      options = data_config
    )
  } else {
    DT::renderDataTable(
      df,
      server = FALSE,
      extensions = "Buttons",
      options = data_config
    )
  }
}
# widget helper functions:
aggregation_widget <- function(inputId) {
  radioButtons(
    inputId = inputId,
    label = "Aggregate",
    selected = "day",
    choiceNames = c("By Day", "By Month"),
    choiceValues = c("day", "month"),
  )
}

download_format_widget <- function(inputId) {
  radioButtons(
    inputId = inputId,
    label = "Document format",
    selected = "PDF",
    choiceNames = c("PDF", "HTML"),
    choiceValues = c("PDF", "HTML"),
  )
}

date_range_widget <- function(inputId) {
  dateRangeInput(
    inputId = inputId,
    label = "Date Range",
    start = lubridate::as_date("2020-8-1"),
    end = lubridate::as_date("2020-8-14"),
    min = lubridate::as_date(min_date),
    max = lubridate::as_date(max_date)
  )
}

cow_selection_widget <- function(inputId, multiple = TRUE, label = "Cows") {
  pickerInput(
    inputId = inputId,
    label = label,
    choices = list(),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select cows"
    )
  )
}

network_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = "Network",
    choices = c("Neighbour", "Synchronicity", "Displacement", "Displacement Star*", "Displacement Paired"),
    selected = NULL,
    multiple = multiple,
    options = pickerOptions(maxOptions = 1,
                            noneSelectedText = "Select network")
  )
}

threshold_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = paste0("Threshold"),
    choices = c("5%", "10%", "25%", "All"),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select % top connected cows"
    )
  )
}

layout_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = paste0("Layout type"),
    choices = c("Circle", "Force-directed"),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select layout type"
    )
  )
}

date_widget <- function(inputId) {
  dateInput(
    inputId = inputId,
    label = "Date",
    value = lubridate::as_date("2020-8-1"),
    max = lubridate::as_date("2020-8-14")
  )
}


#' Helper function for updating cow selection picker input widgets
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
update_cow_selection <- function(date_obj, inputId, session, select_all = FALSE) {

  # find cows that exist in date range
  cow_choices <- filter_dates(feed_drink_df, date, date_obj) %>%
    select(Cow) %>%
    unique() %>%
    arrange(desc(Cow))
  colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = cow_choices,
    selected = NULL
  )
}

update_cow_selection_neighbour <- function(date_obj, inputId, session, select_all = FALSE) {
    
  # find cows that exist in date range
  cow_choices <- filter_dates(feed_drink_df, date, date_obj) %>%
    select(Cow) %>%
    unique() %>%
    arrange(desc(Cow))
  colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = cow_choices,
    selected = NULL,
    options = pickerOptions(maxOptions = 1)
  )
}

update_cow_selection_displacement <- function(relationship_type = "Displacement Star*", date_obj, inputId, session) {
  if (relationship_type != "Displacement Star*") {
    update_cow_selection(date_obj, inputId, session)
  } else {
    # find cows that exist in date range
    cow_choices <- filter_dates(replacement_df, date, date_obj) %>%
      # filter_cd(occupied_bins_with_feed_percent, cd_range) %>%
      select(Actor_cow) %>%
      unique() %>%
      arrange(Actor_cow)
    colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date and cd range")

    # update widget
    updatePickerInput(
      session = session,
      inputId = inputId,
      choices = cow_choices,
      selected = NULL
    )
  }
}

update_2nd_cow_selection_displacement <- function(date_obj,
                                                  inputId,
                                                  session,
                                                  cow_id_1 = NULL,
                                                  CD_min = NULL,
                                                  CD_max = NULL) {
  # find cows that exist in date range
  edges <- combine_replace_edges_star(
    replacement_df, date_obj[1], date_obj[2],
    cow_id_1, CD_min, CD_max
  )

  cow_choices <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    filter(id != cow_id_1)

  colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " paired cows correlated to ", cow_id_1)

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = cow_choices,
    selected = NULL
  )
}
      
#' #' Widget for Bin Weight Selection
#' #'
#' #' @param inputId The id of the picker input widget to update
#' bin_wt_widget <- function(inputId) {
#'   selectInput(
#'     inputId = inputId,
#'     label = "Full Bin Weight (KG)",
#'     choices = rep(1:100),
#'     selected = as.integer(75)
#'   )
#' }

#' #' Helper function for updating bin selection picker input widgets
#' #'
#' #' @param date_obj The date or date range to filter by
#' #' @param inputId The id of the picker input widget to update
#' #' @param session The current server session
#' update_bin_selection <- function(date_obj, inputId, session) {
#' 
#'   # find bins that exist in date range
#'   bin_choices <- filter_dates(feed_df, date, date_obj) %>%
#'     select(Bin) %>%
#'     unique() %>%
#'     arrange(Bin)
#'   colnames(bin_choices) <- paste0(length(bin_choices[[1]]), " bins with data in date range")
#' 
#'   # update widget
#'   updatePickerInput(
#'     session = session,
#'     inputId = inputId,
#'     choices = bin_choices,
#'     selected = bin_choices[[1]]
#'   )
#' }
#' 
#' #' Widget for Bin Selection
#' #'
#' #' @param inputId The id of the picker input widget to update
#' bin_selection_widget <- function(inputId) {
#'   pickerInput(
#'     inputId = inputId,
#'     label = "Bins",
#'     choices = list(),
#'     multiple = TRUE,
#'     options = list(
#'       "actions-box" = TRUE,
#'       "none-selected-text" = "Select bins"
#'     )
#'   )
#' }

#' Custom theme setting function for colors
#'
#'
custom_theme <- function() {
  theme <- tags$head(tags$style(HTML(
    ".headerStyling { 
        font-size: 28px;
        line-height: 50px;
        text-align: left;
        padding: 0 10px;
        overflow: hidden;
        color: white;
      }
      .skin-blue .main-header .logo {
        background-color: #6b96c7;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #6b96c7;
      }
      .skin-blue .main-header .navbar {
        background-color: #6b96c7;
      }
        .skin-blue .main-sidebar {
        background-color: #013551;
        }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #012a41;
      }
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #012a41;
         }
    .content-wrapper, .right-side {
      background-color: #f0fbff;
    }
    .box.box-solid.box-primary>.box-header {
    color:#fff;
     background:#6b96c7
                    }
    .box.box-solid.box-primary  {
    border-bottom-color:#6b96c7;
    border-left-color:#6b96c7;
    border-right-color:#6b96c7;
    border-top-color:#6b96c7;
    } .nav-tabs-custom>.nav-tabs>li.active {
    border-top-color: #6b96c7;
} .skin-blue .sidebar-menu>li.active>a {
    border-left-color: #6b96c7;
} .skin-blue .sidebar-menu>li:hover>a {
    border-left-color: #6b96c7;}"
  )))

  return(theme)
}


library(Rcpp)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(igraph)
library(lubridate)
library(png)
library(visNetwork)
library(googleCloudStorageR)

#' Helper function for converting dataframes to having dates in a single column
#'
#' @param df The dataframe to convert
#'
#' @return Dataframe with dates in a single date column
convert_date_col <- function(df) {
  enframe(df, name = "date") %>%
    mutate(date = as.Date(date)) %>%
    unnest(value)
}

#' Helper function to filter by user input date range
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param date_obj A list containing start and end date or a single date
#'
#' @return Filtered dataframe with only dates within selected range
filter_dates <- function(df, col, date_obj) {
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

# load in plot/table creation scripts
source(here::here("R/notifications.R"))
source(here::here("R/activities.R"))
source(here::here("R/daily_behavior.R"))
source(here::here("R/network.R"))
source(here::here("R/elo.R"))
source(here::here("R/bully_analysis.R"))
source(here::here("R/bins.R"))
source(here::here("R/THI_analysis.R"))

# download data from GCP
# gcs_auth(json_file = here::here('auth/peek-a-moo.json'))
# 
# gcs_global_bucket("peek-a-moo-data")
# 
# objects <- gcs_list_objects()
# download_list <- grep("*.Rda", objects$name, value = TRUE)
# 
# if(!dir.exists(here::here("data/"))) {
#   dir.create(here::here("data/"))
# }
# 
# map(download_list, function(x) gcs_get_object(x,
#                                               saveToDisk = here::here(paste('data/', gsub(".*/","",x), sep = "")),
#                                               overwrite = TRUE))

# load data if not already in memory
# uncomment if running locally and data is already downloaded
# if (!exists("THI")) {
#   load(here::here("data/Wali_trial_summarized_THI.Rda"))
#   load(here::here("data/Feeding_and_drinking_analysis.Rda"))
#   load(here::here("data/Insentec_warning.Rda"))
#   load(here::here("data/duration_for_each_bout.Rda"))
#   load(here::here("data/lying_standing_summary_by_date.Rda"))
#   load(here::here("data/synchronized_lying_total_time.Rda"))
#   load(here::here("data/Cleaned_drinking_original_data.Rda"))
#   load(here::here("data/Cleaned_feeding_original_data.Rda"))
#   load(here::here("data/non_nutritive_visits.Rda"))
#   load(here::here("data/feed_replacement_10mon_CD.Rda"))
#   load(here::here("data/bin_empty_total_time_summary.Rda"))
#   load(here::here("data/Feeding_drinking_at_the_same_time_total_time.Rda"))
#   load(here::here("data/Feeding_drinking_neighbour_total.Rda"))
#   load(here::here("data/Replacement_behaviour_by_date.Rda"))
#   
#   THI <- master_summary
#   
#   rm(master_summary)
# }
# 
# create dataframes for plots and tables
# uncomment if running locally and data is already downloaded
# standing_bout_df <- lying_standing_summary_by_date
# feed_drink_df <- Feeding_and_drinking_analysis
# non_nutritive_df <- convert_date_col(non_nutritive_visits)
# feeding_intake_df <- Feeding_and_drinking_analysis
# feed_df <- convert_date_col(Cleaned_feeding_original_data)
# max_date <- max(feed_drink_df[["date"]])
# replacement_df <- master_feed_replacement_all

#' Helper function for creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#' @param width The width of the box, defaults to 6
#' @param output_fun Function for producing the plot output, defaults to plotlyOutput
#'
#' @return tabBox
default_tabBox <- function(title, var_name, width = 6, height = "500px", output_fun = plotlyOutput) {
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    height = height,
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow_table.gif",
      DT::dataTableOutput(paste0(var_name, "_table"))
    )),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = paste0("loading_cow", as.character(sample(0:7, 1)), ".gif"),
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
format_dt_table <- function(df, page_length = 5) {
  DT::renderDataTable(
    df,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      pageLength = page_length,
      dom = "Bftip",
      buttons = c("csv")
    )
  )
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

date_range_widget <- function(inputId) {
  dateRangeInput(
    inputId = inputId,
    label = "Date Range",
    start = lubridate::as_date("2020-8-1"),
    end = lubridate::as_date("2020-8-14"),
    min = lubridate::as_date("2020-8-1"),
    max = lubridate::as_date("2020-8-14")
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
    choices = list(),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select network"
    )
  )
}

threshold_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = paste0("Threshold (top % of connected cows)"),
    choices = list(),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select % top connected cows"
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

update_cow_selection_displacement <- function(relationship_type = "Displacement Star*", date_obj, inputId, session) {
  if (relationship_type != "Displacement Star*") {
    update_cow_selection(date_obj, inputId, session)
  } else {
    # find cows that exist in date range
    cow_choices <- filter_dates(replacement_df, date, date_obj) %>%
      # filter_cd(occupied_bins_with_feed_percent, cd_range) %>%
      select(Actor_cow) %>%
      unique() %>%
      arrange(desc(Actor_cow))
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

#' Helper function for updating network selection picker input widgets
#'
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
update_network_selection <- function(date_obj, inputId, session, select_all = FALSE) {
  network <- c("Feeding Sychronicity", "Lying Synchronicity", "Feeding Neighbours", "Displacement")
  network_choices <- as.data.frame(network)
  colnames(network_choices) <- paste0(length(network_choices[[1]]), " network choices")

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = network_choices,
    selected = NULL
  )
}

#' Helper function for updating threshold selection picker input widgets
#'
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
update_threshold_selection <- function(date_obj, inputId, session, select_all = FALSE) {
  threshold <- c("5%", "10%", "25%")
  threshold_choices <- as.data.frame(threshold)
  colnames(threshold_choices) <- paste0(length(threshold_choices[[1]]), " thresholds")
  
  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = threshold_choices,
    selected = NULL
  )
}
      
#' Widget for Bin Weight Selection
#'
#' @param inputId The id of the picker input widget to update
bin_wt_widget <- function(inputId) {
  selectInput(
    inputId = inputId,
    label = "Full Bin Weight (KG)",
    choices = rep(1:100),
    selected = as.integer(75)
  )
}

#' Helper function for updating bin selection picker input widgets
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
update_bin_selection <- function(date_obj, inputId, session) {

  # find bins that exist in date range
  bin_choices <- filter_dates(feed_df, date, date_obj) %>%
    select(Bin) %>%
    unique() %>%
    arrange(Bin)
  colnames(bin_choices) <- paste0(length(bin_choices[[1]]), " bins with data in date range")

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = bin_choices,
    selected = bin_choices[[1]]
  )
}

#' Widget for Bin Selection
#'
#' @param inputId The id of the picker input widget to update
bin_selection_widget <- function(inputId) {
  pickerInput(
    inputId = inputId,
    label = "Bins",
    choices = list(),
    multiple = TRUE,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select bins"
    )
  )
}

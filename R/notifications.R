#' Generate water or feed intake warnings based on cuttoff
#'
#' @param df Dataframe containing water or feed intake
#' @param cuttoff The threshold to qualify for a warning if intake is below
#'
#' @return Dataframe containing a row for each date with warning messages
intake_warnings <- function(df, cuttoff){
  if (is.null(cuttoff)) cuttoff = 0
  
  convert_date_col(df) %>%
    mutate(date = as.Date(date)) %>%
    group_by(Cow, date) %>%
    summarise(intake = sum(Intake)) %>%
    filter(intake < cuttoff) %>%
    mutate(intake_message = paste0(Cow, ": ", intake)) %>%
    group_by(date) %>%
    summarise(intake_message = paste(intake_message, collapse = ", "))
}

bin_warnings <- function(df, cuttoff){
  if (is.null(cuttoff)) cuttoff = 0
    
  convert_date_col(df) %>%
    mutate(date = as.Date(date)) %>%
    group_by(Bin, date) %>%
    filter(End == max(End)) %>%
    filter(Endweight < cuttoff) %>%
    mutate(message = paste0(Bin, ": ", Endweight, "(", End, ")")) %>%
    group_by(date) %>%
    summarise(bin_warning = paste(message, collapse = ", "))
}

#' Combine additional warnings with original warnings
#'
#' @param food_cuttoff Cuttoff for feed to trigger warning
#' @param water_cuttoff Cuttoff for water to trigger warning
#'
#' @return named list containing error names and messages
combine_warnings <- function(food_cuttoff = 0, water_cuttoff = 0, bin_cuttoff = 0){
  
  food_warnings <- intake_warnings(Cleaned_feeding_original_data, food_cuttoff) 
  drink_warnings <- intake_warnings(Cleaned_drinking_original_data, water_cuttoff) 
  bin_warnings <- bin_warnings(Cleaned_drinking_original_data, bin_cuttoff)
  
  # get the latest warnings
  all_warnings <- Insentec_warning %>%
    mutate(date = as.Date(date)) %>%
    arrange(desc(date))
  
  all_warnings %>%
    left_join(food_warnings[c("date", "intake_message")], by = "date") %>%
    rename(food_intake = intake_message) %>%
    left_join(drink_warnings[c("date", "intake_message")], by = "date") %>%
    rename(water_intake = intake_message) %>%
    left_join(bin_warnings[c("date", "bin_warning")], by = "date") 
}

#' Generate warning message list
#'
#' @param warning_df Dataframe containing all warnings
#'
#' @return named list containing error names and messages
parse_warnings <- function(warning_df){
  
  # get the latest warnings
  latest_warning_df <- warning_df %>%
    filter(date == max(date))
  
  # warning types to notify
  warning_types <- list(
    "No feed/water after 6pm: " = latest_warning_df[["no_show_after_6pm_cows"]],
    "No feed visit Cows: " = latest_warning_df[["cows_no_visit_to_feed_bin"]],
    "Double detection bin: " = latest_warning_df[["double_bin_detection_bin"]],
    "High intake bin: " = latest_warning_df[["double_cow_detection_bin"]],
    "Negative intake bin: " = latest_warning_df[["negative_intake_bin"]]
  )
  
  # filter out empty warnings
  warning_types[warning_types != ""]
}

#' Generate warning notifications and dropdown
#'
#' @param raw_df Dataframe containing all warnings
#' @param food_cuttoff Cuttoff for feed to trigger warning
#' @param water_cuttoff Cuttoff for water to trigger warning
#'
#' @return dropdownMenu with warning messages
get_warning_dropdown <- function(warning_df){
  
  warning_types <- parse_warnings(warning_df)
  
  # format warning messages
  warning_names <- lapply(
    seq_along(warning_types), 
    function(x){
      paste(names(warning_types[x]), ": ", warning_types[x])
    }
  ) 
  
  get_noti <- function(x){
    notification <- notificationItem(
         text = x,
         icon = icon("exclamation-triangle"), 
         status = "warning"
         )
    notification$children[[1]] <- a(href = "#shiny-tab-warnings_tab",
                                    "onclick" = paste0(
                                      "clickFunction('",paste0(substr(as.character(runif(1, 0, 1)),1,6),"noti"),"'); return false;"),
                                    list(notification$children[[1]]$children))
    return(notification)
  }
  
  # create notification items list
  warnings_list <- warning_names %>%
    lapply(get_noti)
  
  
  # return dropdown menu with notification items
  dropdownMenu(
    type = "notifications", badgeStatus = "warning",
    .list = warnings_list
  )
}
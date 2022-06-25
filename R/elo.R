#' Filtered elo data. 
#'
#' @param x The dataframe with new column names 
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id_1 The interested cow
#' @param cow_id_2 Second interested cow (optional)
#'
#' @return A filtered data. 
elo_df <- function(x, start_date, end_date, cow_id_1 = NULL, cow_id_2 = NULL) {
  df <- x %>%
    select(-present) %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
}

#' Filtered elo data. 
#'
#' @param x The dataframe
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id_1 The interested cow
#' @param cow_id_2 Second interested cow (optional)
#'
#' @return A filtered data. 
elo_df_cow <- function(x, start_date, end_date, cow_id_1 = NULL, cow_id_2 = NULL) {
  df <- elo_df(x, start_date, end_date) %>%
    filter(Cow %in% c(cow_id_1, cow_id_2))
}

#' Make an interactive plot of ELO data and return both plot and filtered data. 
#'
#' @param x The dataframe 
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id The interested cow
#'
#' @return An interactive plotly plot. 
plot_elo <- function(x, start_date, end_date) {
  df <- elo_df(x, start_date, end_date) %>%
    group_by(Cow) %>%
    mutate(Elo_mean = mean(Elo)) %>%
    ungroup()
  
  df <- bind_rows(
    df %>% slice_max(Elo_mean, n = 1),
    df %>% slice_min(Elo_mean, n = 1),
  ) %>%
    mutate(Color = case_when(
      Elo_mean == max(Elo_mean) ~ "#F7766D",
      Elo_mean == min(Elo_mean) ~ "#6fa8dc"
      # ), 
      #   Elo_Group = case_when(
      #   Elo_mean == max(Elo_mean) ~ "Highest Mean",
      #   Elo_mean == min(Elo_mean) ~ "Lowest Mean"
    ))
  
  colors <- unique(df %>%
                     select(Cow, Color))
  
  plot <- ggplot(df, aes(x = Date, y = Elo, color = Cow)) +
    geom_line() +
    scale_x_date(date_labels = "%d-%B-%y") +
    scale_color_manual(name = "Cow", values = colors$Color, labels = unique(df$Cow)) +
    labs(x = 'Date', 
         y = 'Elo Rating',
         title = paste0('Most & least dominant Cow in the network:')) +
    ylim(min(x$Elo), max(x$Elo)) +
    theme_classic() + theme(legend.position = "bottom")
  
  ggplotly(plot)
}

#' Make an interactive plot of ELO data and return both plot and filtered data. 
#'
#' @param x The dataframe
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id The interested cow
#'
#' @return An interactive plotly plot. 
plot_elo_star <- function(x, start_date, end_date, cow_id = NULL) {
  df <- elo_df_cow(x, start_date, end_date, cow_id)
  
  plot <- ggplot(df, aes(x = Date, y = Elo)) +
    geom_line(color = "#F7766D") +
    scale_x_date(date_labels = "%d-%B-%y") +
    labs(x = 'Date', 
         y = 'Elo Rating',
         title = paste0('Daily Dominance Score of Cow ', cow_id)) +
    ylim(min(x$Elo), max(x$Elo)) +
    theme_classic() + theme(legend.position = "none")
  
  ggplotly(plot)
}

#' Make an interactive plot of ELO data and return both plot and filtered data. 
#'
#' @param x The dataframe
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id_1 The interested cow
#' @param cow_id_2 Second interested cow (optional)
#'
#' @return An interactive plotly plot. 
plot_elo_paired <- function(x, start_date, end_date, cow_id_1 = NULL, cow_id_2 = NULL) {
  df <- elo_df_cow(x, start_date, end_date, cow_id_1, cow_id_2) %>%
    group_by(Cow) %>%
    mutate(Elo_mean = mean(Elo)) %>%
    arrange(desc(Elo_mean)) %>%
    ungroup() %>%
    mutate(Color = case_when(
      Elo_mean == max(Elo_mean) ~ "#F7766D",
      Elo_mean == min(Elo_mean) ~ "#6fa8dc"
    )) %>%
    arrange(Cow)
  
  plot <- ggplot(df, aes(x = Date, y = Elo, colour = Cow)) +
    geom_line() +
    scale_x_date(date_labels = "%d-%B-%y") +
    scale_color_manual(values = unique(df$Color)) +
    labs(x = 'Date', 
         y = 'Elo Rating',
         title = paste0('Daily Dominance Score of Cow ', cow_id_1, ' vs. ', cow_id_2)) +
    ylim(min(x$Elo), max(x$Elo)) +
    theme_classic() + theme(legend.position = "bottom")
  
  ggplotly(plot)
}

#' Helper function for catching if there are missing dates in plotly plots
#'
#' @param df The data frame for the selected network
#' @param date_range The input date range from the date range widget
#'
#' @return error_message if there is a date input issue that needs to stop the graph generation
missing_date_range_check_plotly <- function(date_range, df = NULL) {
  `%!in%` <- Negate(`%in%`)
  df_dates <- sort(unique(df$Date))
  
if (date_range[[1]] == date_range[[2]]) {
    error_messagep3 <- renderPlotly({
      validate(
        need(
          date_range[[1]] != date_range[[2]],
          paste0(
            "Plot cannot generate for a single day. Please select a timeline with more than a single date."
          )
        )
      )
    })
    return(error_messagep3)
  } else if (date_range[[2]] %!in% df_dates) {
    error_messagep2 <- visNetwork::renderVisNetwork({
      validate(
        need(
          date_range[[2]] %in% df_dates,
          paste0(
            "There is no data for the selected date ",
            date_range[[2]],
            ". Plot will crash if ending date is missing. Please select a different ending date."
          )
        )
      )
    })
    return(error_messagep2)
  }
  else {
    if (date_range[[1]] %!in% df_dates) {
      showNotification(
        type = "warning",
        paste0("Date range contains days with missing data: Dominance Plot.")
      )
    }
    if (date_range[[1]] %in% df_dates && date_range[[2]] %in% df_dates) {
      range_of_df <- df_dates[which(df_dates == date_range[[1]]):which(df_dates == date_range[[2]])]
      
      range_days <- seq(as.Date(date_range[[1]]),
                        as.Date(date_range[[2]]),
                        by = "days"
      )
      
      if (all(range_days %in% range_of_df) == FALSE) {
        showNotification(
          type = "warning",
          paste0("Date range contains days with missing data: Dominance Plot.")
        )
      }
    }
    return(NULL)
  }
}
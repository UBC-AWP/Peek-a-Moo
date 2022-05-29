#' Filter data by user input and add summary rows
#'
#' @param df Dataframe to filter and adjust
#' @param agg_type A string representing aggregation type, day/month/year
#' @param cow_selection A list of cow Ids to display
#' @param date_range A list containing two dates - start and end date
#'
#' @return Filtered dataframe with additional stats
process_range_data <- function(df, agg_type, cow_selection, date_range){
  if (is.null(agg_type)) agg_type = 'day'
  
  # convert Cow col to character to add summary stats later and filter by date
  df <- df %>%
    mutate(Cow = as.character(Cow)) %>%
    filter_dates(date, date_range)
  
  # calculate summary stats
  herd_average <- df %>%
    mutate(date = lubridate::floor_date(date, agg_type)) %>%
    group_by(date) %>%
    summarise(.,
              across(where(is.character), ~"Herd Average"),
              across(where(is.numeric), mean, na.rm=TRUE),
              across(where(is.numeric), round, 0))
  
  # filter by cow and add summary rows
  df <- df %>%
    filter_cows(Cow, cow_selection) %>%
    group_by(Cow, date) %>%
    summarise(.,
              across(where(is.character), ~"Herd Average"),
              across(where(is.numeric), mean, na.rm=TRUE),
              across(where(is.numeric), round, 0)) %>%
    bind_rows(herd_average)
  
  # convert Cow to factor
  df$Cow <- factor(df$Cow) %>%
    forcats::fct_rev()
  
  df
}

#' Format column names for display
#' 
#' @param col_name The column name to format
#' 
#' return the formatted column name
format_col_name <- function(col_name){
  str_replace_all(col_name, "_", " ") %>%
    str_to_title() %>%
    str_replace("Seconds", "s") %>%
    str_replace("\\((?i)s\\)", " (s)") %>%
    str_replace("\\((?i)kg\\)", " (kg)")
}

#' Generate the plot and data tabs for time range plots
#'
#' @param df The dataframe containing data to be displayed
#' @param y_col The column of interest
#' @param show_average Boolean whether to show average line per cow
#'
#' @return NULL
cow_date_range_plot <- function(df, y_col, show_average){
  
  # throw error if no data available for date range
  if(nrow(df) == 0) stop('No data available for this date range')
  
  # extract y_label from unquoted col and format
  y_label <- quo_name(enquo(y_col)) %>%
    format_col_name()
  
  plt <- df %>%
    ggplot(aes(x = date, y = {{y_col}}, colour = `Cow`)) +
    geom_line() +
    theme(legend.position = "bottom") + 
    xlab("Date") +
    ylab(y_label) +
    scale_x_date(date_labels = "%b-%Y") + 
    theme_classic() + theme(legend.position = "none")
  
  if(show_average){
    plt <- plt + 
      stat_smooth(
        method = "lm", 
        formula = y~1,
        se = FALSE,
        fullrange = TRUE,
        size = 0.5,
        linetype = "dashed"
      )
  }
  
  plt %>%
    ggplotly() %>%
    layout(legend = list(orientation = "h", y = -0.2))
}
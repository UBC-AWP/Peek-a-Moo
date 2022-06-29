#' Filter data by user input and add summary rows
#'
#' @param df Dataframe to filter and adjust
#' @param agg_type A string representing aggregation type, day/month/year
#' @param cow_selection A list of cow Ids to display
#' @param date_range A list containing two dates - start and end date
#'
#' @return Filtered dataframe with additional stats
process_range_data <- function(df, agg_type, cow_selection, date_range) {
  if (is.null(agg_type)) agg_type <- "day"

  # convert Cow col to character to add summary stats later and filter by date
  df <- df %>%
    mutate(Cow = as.character(Cow)) %>%
    filter_dates(date, date_range)

  # calculate summary stats
  herd_average <- df %>%
    mutate(date = lubridate::floor_date(date, agg_type)) %>%
    group_by(date) %>%
    summarise(
      .,
      across(where(is.character), ~"Herd Average"),
      across(where(is.numeric), mean, na.rm = TRUE),
      across(where(is.numeric), round, 0)
    )

  # filter by cow and add summary rows
  df <- df %>%
    filter_cows(Cow, cow_selection) %>%
    group_by(Cow, date) %>%
    summarise(
      .,
      across(where(is.character), ~"Herd Average"),
      across(where(is.numeric), mean, na.rm = TRUE),
      across(where(is.numeric), round, 0)
    ) %>%
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
format_col_name <- function(col_name) {
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
cow_date_range_plot <- function(df, y_col, show_average, date_range, agg_type) {

  # throw error if no data available for date range
  if (nrow(df) == 0) stop("No data available for this date range")
  Date_1 = format(as.POSIXct(as.character(date_range[1])),"%m")
  Date_2 = format(as.POSIXct(as.character(date_range[2])),"%m")
  if(Date_1 == Date_2 && agg_type == 'month') stop('Date range must span over 2 months when data is aggregated by month, please adjust date input selection.')
  if (length(unique(df$Cow)) > 61) stop("The maximum cows to plot is 61. Please adjust selection.")

  # custom color palette
  custom_palette <- c(
    "#F7766D",
    "#325803",
    "#C5D1A5",
    "#7C9454",
    "#D4DDB5",
    "#8AA164",
    "#4F7023",
    "#99AD74",
    "#5E7C34",
    "#A8B984",
    "#D1DAE0",
    "#BECBD3",
    "#ABBCC6",
    "#98ADB9",
    "#859EAC",
    "#728F9F",
    "#608092",
    "#4D7185",
    "#3A6278",
    "#27536B",
    "#14445E",
    "#013551",
    "#416413",
    "#F7766D",
    "#325803",
    "#C5D1A5",
    "#7C9454",
    "#D4DDB5",
    "#8AA164",
    "#4F7023",
    "#99AD74",
    "#5E7C34",
    "#A8B984",
    "#D1DAE0",
    "#BECBD3",
    "#ABBCC6",
    "#98ADB9",
    "#859EAC",
    "#728F9F",
    "#608092",
    "#4D7185",
    "#3A6278",
    "#27536B",
    "#14445E",
    "#013551",
    "#416413",
    "#F7766D",
    "#325803",
    "#C5D1A5",
    "#7C9454",
    "#D4DDB5",
    "#8AA164",
    "#4F7023",
    "#99AD74",
    "#5E7C34",
    "#A8B984",
    "#D1DAE0",
    "#BECBD3",
    "#ABBCC6",
    "#98ADB9",
    "#859EAC",
    "#728F9F",
    "#608092",
    "#4D7185",
    "#3A6278",
    "#27536B",
    "#14445E",
    "#013551",
    "#416413"
  )

  # extract y_label from unquoted col and format
  y_label <- quo_name(enquo(y_col)) %>%
    format_col_name()

  plt <- df %>%
    ggplot(aes(x = date, y = {{ y_col }}, color = Cow)) +
    geom_line() +
    theme(legend.position = "bottom") +
    xlab("Date") +
    ylab(y_label) +
    scale_x_date(date_labels = "%b-%Y") +
    scale_colour_manual(values = custom_palette) +
    theme_classic() +
    theme(legend.position = "bottom")

  if (show_average) {
    plt <- plt +
      stat_smooth(
        method = "lm",
        formula = y ~ 1,
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
#' Carry out the analysis and create new columns for the desired THI
#' @param df The combined dataframe i.e the output of make_analysis_df function
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @return A dataframe with new columns names
THI_analysis <- function(df, start_date, end_date) {

  names(df)[names(df) == "temperature(C)_mean"] <- "Average_Temp_C"
  names(df)[names(df) == "temperature(C)_standard_deviation"] <- "SD_Temp_C"
  names(df)[names(df) == "temperature(C)_min"] <- "Min_Temp_C"
  names(df)[names(df) == "temperature(C)_max"] <- "Max_Temp_C"
  
  names(df)[names(df) == "relative_humidity(%)_mean"] <- "Average_Humidity_p"
  names(df)[names(df) == "relative_humidity(%)_standard_deviation"] <- "SD_Humidity_p"
  names(df)[names(df) == "relative_humidity(%)_max"] <- "Max_Humidity_p"
  names(df)[names(df) == "relative_humidity(%)_min"] <- "Min_Humidity_p"

  df <- df %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date))
  
  return(df)
    
}

#' Make an interactive plot of THI data. 
#'
#' @param df The dataframe with new column names i.e the output of THI_analysis function
#'
#' @return An interactive plotly plot. 
plot_THI_analysis <- function(df) {
  
  
  # Make plot
  analysis_plot_thi <- ggplot(df, aes(x = date)) +
    geom_line(aes(y = THI_mean,  color="red")) +
    geom_ribbon(aes(ymin = THI_min, ymax = THI_max, fill = "red"), alpha = 0.2) +
    scale_x_date(date_labels = "%d-%B-%y") +
    labs(x = 'Date', y = 'Mean Daily THI') +
    geom_line(linetype="dashed", color="gray", size=1, alpha=.5, y = 68) + 
    theme_classic() + theme(legend.position = "none")
  
  ggplotly(analysis_plot_thi)
}
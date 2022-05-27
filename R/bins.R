#' Generate Feed Bin Data for a Particular Date
#'
#' @param df The data frame containing feed data
#' @param feed_date The date of interest
#' @param bin_selection Bins to include in data frame
#' @return Grouped data frame for particular date chosen and summarized values.
select_feed_bin_data <- function(df, feed_date, bin_selection) {
  # if (is.null(feed_date)) feed_date = lubridate::as_date("2020-8-1")
  
  ## feed bin plot section
  df <- df %>%
    mutate(
      Startweight = case_when(Startweight < 0 ~ 0, # set to zero where weight is negative
                              TRUE ~ Startweight),
      Endweight = case_when(Endweight < 0 ~ 0,
                            TRUE ~ Endweight)
    )
  
  df <- df %>% filter(date == feed_date,
                      Bin %in% bin_selection)  # filter by a single date, selected bins
  # choose date
  one_bin_df <- df %>%
    mutate(Hour = lubridate::hour(Start)) %>%
    group_by(Bin, Hour) %>%
    # mutate(count_visits = n()) %>%
    summarise(hrly_avg_wt = mean(Startweight),
              hrly_visits = n())
}

#' Generate Feed Bin Plot for a Particular Date
#'
#' @param hourly_df The data frame containing feed data
#' @param hr The hour of interest
#' @param max_wt Maximum weight of a bin to represent 100% capacity, default 75kg.
#' @return NULL
plot_feed_bin_data <- function(hourly_df, hr, max_wt) {
  if (dim(hourly_df)[1] == 0) {
    stop('No data available for this date.')
  }
  # Adapted plot from
  # https://stackoverflow.com/questions/48522350/create-an-image-filled-chart-in-r-using-ggplot
  # Load png file from imgur as binary
  container_img <-
    readPNG(here::here("dashboard/www/container.png"))
  h <- dim(container_img)[1]
  w <- dim(container_img)[2]
  
  # Find the rows where image starts & ends
  pos1 <- which(apply(container_img[, , 1], 1, function(y)
    any(y == 1)))
  mn1 <- min(pos1)
  mx1 <- max(pos1)
  
  # manual colouring for plot
  cols <- c(
    rgb(255, 255, 255, maxColorValue = 255),
    # bg
    rgb(237, 237, 237, maxColorValue = 255),
    # empty
    rgb(42, 128, 183, maxColorValue = 255)
  )   # full
  full_df <- data.frame(
    Var1 = integer(),
    Var2 = integer(),
    value = integer(),
    Bin = integer()
  )
  hourly_df <- hourly_df %>% filter(Hour == hr)
  for (n in hourly_df$Bin) {
    imgmtx <- container_img[h:1, , 1]
    whitemtx <- (imgmtx == 1)
    
    
    pospct <- hourly_df %>% filter(Bin == n) %>% .$hrly_avg_wt
    denom <- max(as.integer(max_wt), pospct)
    if (pospct > 0) {
      pospct <- pospct %>% magrittr::divide_by(denom) %>%
        magrittr::multiply_by(mx1 - mn1) %>%
        magrittr::add(mn1) %>% round()
    }
    
    colmtx <- matrix(rep(FALSE, h * w), nrow = h)
    colmtx[mx1:pospct, 1:w] <- TRUE
    imgmtx[whitemtx & colmtx] <- 0.5
    # Need to melt the matrix into a data.frame that ggplot can understand
    df <- reshape2::melt(imgmtx)
    df <- df %>% mutate(Bin = n)
    full_df <-
      rbind(full_df, df) # append to get all bins into one df
  }
  
  full_df <-
    merge(full_df, hourly_df, by = 'Bin') %>% #filter(Bin < 4) %>%
    mutate(
      Title_label = factor(Bin),
      Label_wt = paste0(hrly_avg_wt, 'KG'),
      Label_visits = paste0(hrly_visits, ' Visits')
    )
  
  # Then use a heatmap with 3 colours for background, and percentage fills
  # Converting the fill value to a factor causes a discrete scale.
  # geom_tile takes three columns: x, y, fill corresponding to
  # x-coord, y-coord, and colour of the cell.
  
  ggplot(full_df, aes(
    x = Var2,
    y = Var1,
    fill = factor(value),
    label = Label_wt
  )) +
    geom_tile() +
    scale_fill_manual(values = cols) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#BCD3E7")
    ) +
    facet_wrap( ~ Title_label + Label_visits)
}


parse_hunger_df <- function(df, bins){
  df %>% 
    pivot_longer(-date, names_to = "Bin", values_to = "Duration") %>%
    filter(Bin %in% bins) %>%
    mutate(Bin = forcats::as_factor(Bin))
}

hunger_plot <- function(df){
  plt <- df %>%
    ggplot(aes(x = Bin, y = Duration)) +
    geom_col(fill = "lightblue") +
    ylab("Empty Duration (s)") + 
    geom_hline(yintercept = mean(df$Duration), linetype='dotted', col = "gray", alpha = 0.5) +
    theme_classic() +
    theme(legend.position="none")
  
  plt %>% ggplotly()
}
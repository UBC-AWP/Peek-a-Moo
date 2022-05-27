library(igraph)

plot_network <- function(nodes, edges, threshold_id = "10%") {
  ledges <- data.frame(color = c("#D3D3D3", "white"), 
                       label = c(paste0("Edges with top ", threshold_id, " weights"), "Hidden edges"), 
                       arrows =c("both", "both"), 
                       font.align = "bottom") 
  
  visNetwork(nodes,
    edges,
    width = "100%", height = "800px"
  ) %>%
    visNodes(
      font = list(size = 30),
      shape = "circle",
      shadow = TRUE,
      borderWidth = 1,
      color = list(
        border = "darkgray",
        highlight = list(background = "orange", border = "darkred")
      )
    ) %>%
    visEdges(
      smooth = list(enabled = TRUE, type = "horizontal"),
      color = list(color = "#D3D3D3", highlight = "orange", hover = "#2B7CE9")
    ) %>%
    visLegend(addEdges = ledges, useGroups = FALSE) %>%
    visInteraction(
      hover = TRUE,
      tooltipDelay = 100,
      tooltipStay = 300,
      dragNodes = FALSE,
      selectable = FALSE,
      selectConnectedEdges = FALSE
    ) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    visPhysics(stabilization = FALSE) %>%
    visExport(
      type = "png", name = "export-network",
      float = "left", label = "Save network", background = "white", style = ""
    )
}

plot_network_disp <- function(nodes, edges, threshold_id = "10%") {
  plot_network(nodes, edges, threshold_id = threshold_id) %>%
    visNodes(
      shape = "dot"
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.8))
    ) %>%
    visOptions(
      nodesIdSelection = list(enabled = TRUE, main = "Select Focused Cow"),
      highlightNearest = list(
        enabled = T,
        degree = 1,
        hideColor = "rgba(0,0,0,0)",
        labelOnly = TRUE
      )
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      multiselect = TRUE,
      selectable = TRUE,
      selectConnectedEdges = TRUE
    )
}

plot_network_disp_star <- function(nodes, edges) {
  visNetwork(nodes,
    edges,
    width = "100%", height = "800px"
  ) %>%
    visNodes(
      font = list(size = 20),
      shape = "dot",
      shadow = TRUE,
      borderWidth = 2,
      color = list(hightlight = "#D2E5FF", highlight.border = "#2B7CE9")
    ) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>%
    visInteraction(
      hover = TRUE,
      tooltipDelay = 100,
      tooltipStay = 300
    ) %>%
    visPhysics(stabilization = FALSE) %>%
    visExport(
      type = "png", name = "export-network",
      float = "left", label = "Save network", background = "white", style = ""
    )
}

combine_edges <- function(x, from_date = NULL, to_date = NULL, threshold = 0.9) {

  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf

  # combine list into one long data frame
  edgelist <- x %>%
    purrr::map_df(adjacency_to_long, .id = "date") %>%
    dplyr::mutate(dplyr::across(date, as.Date)) %>%
    rename(weight = value) %>%
    filter(
      date >= from_date,
      date <= to_date
    ) %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(weight_bins = cut(weight,
      breaks = c(
        min(weight),
        quantile(weight, threshold),
        max(weight)
      ),
      include.lowest = TRUE, ordered = TRUE
    )) %>%
    mutate(
      width = as.integer(weight_bins) - 1,
      color.opacity = case_when(
        width >= 1 ~ 1,
        width < 1 ~ 0
      )
    )

  # return the edgelist
  edgelist
}

# combine dataframe for displacement data and create edges list (displacement)
combine_replace_edges <- function(x,
                                  from_date = NULL,
                                  to_date = NULL,
                                  CD_min = NULL,
                                  CD_max = NULL,
                                  threshold = 0.9) {

  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  CD_min <- CD_min %||% 0
  CD_max <- CD_max %||% 1

  combo_df <- as.data.frame(x) %>%
    mutate(date = as.Date(date)) %>%
    rename(
      CD = occupied_bins_with_feed_percent,
      from = Actor_cow,
      to = Reactor_cow
    ) %>%
    select(date, from, to, CD) %>%
    filter(
      date >= from_date,
      date <= to_date,
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup() %>%
    mutate(weight_bins = cut(weight,
      breaks = c(
        min(weight),
        quantile(weight, threshold),
        max(weight)
      ),
      include.lowest = TRUE, ordered = TRUE
    )) %>%
    mutate(
      width = as.integer(weight_bins) - 1,
      color.opacity = case_when(
        width >= 1 ~ 1,
        width < 1 ~ 0
      ),
      title = paste0("Displacements: ", weight)
    )
}

# combine dataframe for displacement data and create edges list (star version)
combine_replace_edges_star <- function(x,
                                       from_date = NULL,
                                       to_date = NULL,
                                       cow_id = NULL,
                                       CD_min = NULL,
                                       CD_max = NULL) {

  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  CD_min <- CD_min %||% 0
  CD_max <- CD_max %||% 1

  combo_df <- as.data.frame(x) %>%
    mutate(date = as.Date(date)) %>%
    rename(
      CD = occupied_bins_with_feed_percent,
      from = Actor_cow,
      to = Reactor_cow
    ) %>%
    select(date, from, to, CD) %>%
    filter(
      date >= from_date,
      date <= to_date,
      (from == cow_id) | (to == cow_id),
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup() %>%
    mutate(
      type = case_when(
        from == cow_id ~ "actor",
        to == cow_id ~ "reactor"
      ),
      title = paste0("Actions: ", weight)
    ) %>%
    arrange(from != cow_id)
}

# create nodes list from edges list
combine_nodes <- function(edges, deg) {
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    mutate(
      size = unname(deg) / max(unname(deg)) * 40, # Node size
      label = id
    )
}

# create nodes list from edges list (displacement)
combine_replace_nodes <- function(edges, deg) {
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    mutate(
      size = unname(deg) / max(unname(deg)) * 40, # Node size
      title = paste0("Cow: ", id, "<br>Different Associations: ", unname(deg))
    )
}

# create nodes list from edges list (star version)
combine_replace_nodes_star <- function(edges, cow_id = NULL) {
  nodes_size_to <- edges %>%
    group_by(to) %>%
    summarise(deg = sum(weight)) %>%
    rename(id = to)

  nodes_size_from <- edges %>%
    group_by(from) %>%
    summarise(deg = sum(weight)) %>%
    rename(id = from)

  nodes_size <- rbind(nodes_size_to, nodes_size_from) %>%
    group_by(id) %>%
    summarise(deg = sum(deg))

  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    mutate(
      color.background = case_when(
        id == cow_id ~ "orange",
        id != cow_id ~ "#D2E5FF"
      ),
      color.border = case_when(
        id == cow_id ~ "darkred",
        id != cow_id ~ "#2B7CE9"
      ),
      label = paste(id)
    ) %>%
    left_join(nodes_size, by = "id") %>%
    mutate(
      size = case_when(
        id == cow_id ~ 20,
        id != cow_id ~ log(deg + 1) * 10
      ),
      title = case_when(
        id == cow_id ~ paste0("Center Cow"),
        id != cow_id ~ paste0("Cow: ", id, "<br>Displacements with Center: ", deg),
      )
    )
}

.make_tidygraph <- function(x, edgelist = NULL, directed = FALSE) {
  edgelist <- edgelist %||% combine_data(x)
  g <- graph_from_data_frame(edgelist, directed = directed)

  # return the graph
  g
}


make_tidygraph <- memoise::memoise(.make_tidygraph)


adjacency_to_long <- function(x, upper_only = FALSE) {
  # check inputs
  dn <- dimnames(x)
  if (!inherits(x, "matrix")) {
    stop("Input must be a matrix")
  } else if (is.null(dn) || is.null(dn[[1]]) || is.null(dn[[2]])) {
    stop("Input matrix must have named dimensions.")
  } else if (!all.equal(dn[[1]], dn[[2]])) {
    stop("Dimension names must match across both axes")
  }

  # zero-out the lower triangle if needed
  if (upper_only) {
    x[lower.tri(x)] <- 0
  }

  # pivot data to long
  x %>%
    as.data.frame() %>%
    tibble::rownames_to_column("to") %>%
    tidyr::pivot_longer(-to, "from", "time") %>%
    dplyr::filter(value > 0)
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Helper function for catching if there are missing date inputs in the networks
#'
#' @param network The input for the selected network
#' @param df The data frame for the selected network
#' @param date_range The input date range from the date range widget
#'
#' @return error_message if there is a date input issue that needs to stop the graph generation
missing_date_range_check <- function(date_range, df = NULL, network = NULL) {
  `%!in%` <- Negate(`%in%`)

  if (!(network %in% c("Displacement", "Displacement Star*"))) {
    df_dates <- names(df)
    df_dates <- as.Date(df_dates, format = "%Y-%m-%d")
  } else {
    df_dates <- unique(df$date)
  }
  if (date_range[[1]] %!in% df_dates && date_range[[2]] == date_range[[1]]) {
    error_message1 <- visNetwork::renderVisNetwork({
      validate(
        need(
          date_range[[1]] %in% df_dates,
          paste0(
            "There is no data for the selected date ",
            date_range[[1]],
            ". Please select a different date."
          )
        )
      )
    })
    return(error_message1)
  } else if (date_range[[2]] %!in% df_dates) {
    error_message2 <- visNetwork::renderVisNetwork({
      validate(
        need(
          date_range[[2]] %in% df_dates,
          paste0(
            "There is no data for the selected date ",
            date_range[[2]],
            ". The network cannot compute if the ending date is missing. Please select a different ending date."
          )
        )
      )
    })
    return(error_message2)
  } else {
    if (date_range[[1]] %!in% df_dates) {
      showNotification(
        type = "warning",
        paste0("Date range contains days with missing data.")
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
          paste0("Date range contains days with missing data.")
        )
      }
    }
    return(NULL)
  }
}

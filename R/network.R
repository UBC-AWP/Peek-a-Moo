library(igraph)

plot_network <- function(nodes, edges, layouts_type = "Circle", selected_nodes = NULL) {
  if (layouts_type == "Circle") {
    layouts = "layout_in_circle"
  } else {
    layouts = "layout_with_fr"
  }
  
  if (is.null(selected_nodes) || length(selected_nodes) > 1) {
    list_nodesIdSelection = list(enabled = TRUE)
  } else {
    list_nodesIdSelection = list(enabled = TRUE, 
                                 selected = selected_nodes)
  }
  
  visNetwork(nodes,
    edges,
    width = "100%", height = "800px"
  ) %>%
    visNodes(
      font = list(size = 30),
      shape = "dot",
      shadow = TRUE,
      borderWidth = 1,
      color = list(
        border = "darkgray"
        )
    ) %>%
    visEdges(
      smooth = list(enabled = TRUE, type = "horizontal"),
      color = list(color = "#D3D3D3", highlight = "#ffaa00", hover = "#2B7CE9")
    ) %>%
    visInteraction(
      hover = TRUE,
      tooltipDelay = 0,
      tooltipStay = 500,
      dragNodes = TRUE,
      selectable = TRUE,
      selectConnectedEdges = FALSE,
      navigationButtons = TRUE
    ) %>%
    visOptions(
      nodesIdSelection = list_nodesIdSelection,
      highlightNearest = list(
        enabled = T,
        degree = 0,
        hideColor = "rgba(0,0,0,0)",
        labelOnly = TRUE
      )
    ) %>%
    visIgraphLayout(layout = layouts) %>%
    visPhysics(stabilization = FALSE)
}

nodes_edges_list_synchronicity <- function(raw_graph_data, 
                                           date_range, 
                                           threshold_selected) {
  
  edges <- combine_edges(
    raw_graph_data,
    date_range[[1]],
    date_range[[2]],
    threshold_selected
  )
  
  g <- .make_tidygraph(raw_graph_data, edges)
  deg <- degree(g)
  size <- deg / max(deg) * 40
  
  nodes <- combine_nodes(
    raw_graph_data,
    date_range[[1]],
    date_range[[2]],
    size
  )
  
  if (mean(edges$width > 2)) {
    edges$width <- edges$width / 2
  }
  
  out <- list(nodes, edges)
  
}

plot_network_disp <- function(nodes, edges, layouts_type = "Circle") {
  
  plot_network(nodes, edges, layouts_type) %>%
    visNodes(
      shape = "dot",
      color = list(
        highlight = list(background = "#ffaa00", border = "darkred")
      )
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
      tooltipDelay = 0,
      tooltipStay = 500
    ) %>%
    visPhysics(stabilization = FALSE)
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
    ungroup() 
  
  if (threshold != 0) {
    edgelist <- edgelist %>%
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
        # color.opacity = case_when(
        #   width >= 1 ~ 1,
        #   width < 1 ~ 0
        # ),
        title = paste0(from, " and ", to, ": ", weight, " secs")
      ) %>%
      filter(width >= 1)
  } else {
    edgelist <- edgelist %>%
      mutate(
        width = weight / mean(weight),
        # color.opacity = case_when(
        #   width >= 1 ~ 1,
        #   width < 1 ~ 0
        # ),
        title = paste0(from, " and ", to, ": ", weight, " secs")
      )
  }

  # return the edgelist
  edgelist
}

# combine dataframe for displacement data and create edges list (displacement)
combine_replace_df <- function(x,
                                  from_date = NULL,
                                  to_date = NULL,
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
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup()
}

combine_replace_edges <- function(x,
                                  from_date = NULL,
                                  to_date = NULL,
                                  CD_min = NULL,
                                  CD_max = NULL,
                                  threshold = 0.9) {
  
  combo_df <- combine_replace_df(x, from_date, to_date, CD_min, CD_max)
  
  if (threshold != 0) {
    edgelist <- combo_df %>%
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
        # color.opacity = case_when(
        #   width >= 1 ~ 1,
        #   width < 1 ~ 0
        # ),
        title = paste0("Displacements: ", weight)
      ) %>%
      filter(width >= 1)
  } else {
    edgelist <- combo_df %>%
      mutate(
        width = weight / mean(weight),
        # color.opacity = case_when(
        #   width >= 1 ~ 1,
        #   width < 1 ~ 0
        # ),
        title = paste0(from, " and ", to, ": ", weight, " secs")
      )
  }
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

# combine dataframe for displacement data and create edges list (paired version)
combine_replace_edges_paired <- function(x,
                                       from_date = NULL,
                                       to_date = NULL,
                                       cow_id_1 = NULL,
                                       cow_id_2 = NULL,
                                       CD_min = NULL,
                                       CD_max = NULL) {
  
  paired_df <- combine_replace_edges_star(x, from_date, to_date, cow_id_1, CD_min, CD_max) %>% 
    filter(from == cow_id_2 | to == cow_id_2) %>%
    mutate(label = title)
}

# create nodes list from edges list
combine_nodes <- function(df,
                          from_date = NULL,
                          to_date = NULL, 
                          size) {
  df <- df %>%
    purrr::map_df(adjacency_to_long, .id = "date") %>%
    dplyr::mutate(dplyr::across(date, as.Date)) %>%
    rename(weight = value) %>%
    filter(
      date >= from_date,
      date <= to_date
    )
  
  nodes <- data.frame(id = unique(c(
    df$from,
    df$to
  ))) %>%
  mutate(
    #size = unname(deg) / max(unname(deg)) * 10, # Node size
    label = id
  ) %>%
    arrange(id)
  
  nodes$size <- size[match(nodes$id, names(size))]
  
  nodes[is.na(nodes)] <- 2
  
  return(nodes)
}

# create nodes list from edges list (displacement)
combine_replace_nodes <- function(x,
                                  from_date = NULL,
                                  to_date = NULL,
                                  cow_id = NULL,
                                  CD_min = NULL,
                                  CD_max = NULL,
                                  deg = NULL) {
  df <- combine_replace_df(x, from_date, to_date, CD_min, CD_max)
  
  nodes <- data.frame(id = unique(c(
    df$from,
    df$to
  )))
  
  nodes$degree <- deg[match(nodes$id, names(deg))]
  nodes[is.na(nodes)] <- 0
  
  size <- deg / max(deg) * 40
  nodes$size <- size[match(nodes$id, names(size))]
  nodes[is.na(nodes)] <- 2

  nodes <- nodes %>%
    mutate(
      title = paste0("Cow: ", id, "<br>Different Associations: ", degree)
    )
  
  return(nodes)
}

# create nodes list from edges list (star version)
combine_replace_nodes_star <- function(edges, cow_id = NULL,
                                       from_date = NULL,
                                       to_date = NULL) {
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
    left_join(combine_elo_star(from_date, to_date), by = "id") %>%
    mutate(
      color.background = as.character(Elo_bins),
      color.border = case_when(
        id == cow_id ~ "darkred",
        id != cow_id ~ "#2B7CE9"
      ),
      color.hover.background = case_when(
        id == cow_id ~ "#ffaa00",
        id != cow_id ~ "#D2E5FF"
      ),
      color.hover.border = case_when(
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
        id == cow_id ~ paste0("Center Cow",
                              "<br>Mean Elo: ", Elo_mean),
        id != cow_id ~ paste0("Cow: ", id, 
                              "<br>Displacements with Center: ", deg,
                              "<br>Mean Elo: ", Elo_mean)
      )
    )
}

# create nodes list from edges list (star version)
combine_replace_nodes_paired <- function(edges,
                                       from_date = NULL,
                                       to_date = NULL) {
  
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    left_join(combine_elo_star(from_date, to_date), by = "id") %>%
    #arrange(id) %>%
    arrange(desc(Elo_mean)) %>%
    mutate(
      label = paste(id)
    )
  
  if (nrow(nodes) > 0) {
    nodes$color <- c("#F7766D", "#6fa8dc")
  }
  
  return(nodes)
}

# Fiter elo score data
combine_elo_star <- function(from_date = NULL,
                             to_date = NULL) {
  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf
  
  combo_df <- dominance_df %>%
    mutate(Date = as.Date(Date),
           id = as.numeric(Cow)) %>%
    select(-c(Cow, present)) %>%
    filter(
      Date >= from_date,
      Date <= to_date
    ) %>%
    group_by(id) %>%
    summarise(Elo_mean = round(mean(Elo), 2)) %>%
    ungroup() %>%
    mutate(Elo_bins = cut(Elo_mean,
                           breaks = 5,
                           labels = c("#cfe2f3", "#9fc5e8", "#6fa8dc", "#3d85c6", "#0b5394"),
                           include.lowest = TRUE, 
                           ordered = TRUE
    ))
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

  if (!(network %in% c("Displacement", "Displacement Star*", "Displacement Paired"))) {
    df_dates <- names(df)
    df_dates <- sort(as.Date(df_dates, format = "%Y-%m-%d"))
  } else {
    df_dates <- sort(unique(df$date))
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
        paste0("Date range contains days with missing data: Social Network.")
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
          paste0("Date range contains days with missing data: Social Network.")
        )
      }
    }
    return(NULL)
  }
}

library(igraph)

#' Plots a general visNetwork.
#'
#' @param nodes The dataframe with nodes list
#' @param edges The dataframe with edges list
#' @param layouts_type A string value that represents the layout type, defaults to "Circle"
#' @param selected_nodes The selected cow used in visInteraction
#'
#' @return A general visNetwork plot
plot_network <- function(nodes, edges, layouts_type = "Circle", selected_nodes = NULL) {
  if (layouts_type == "Circle") {
    layouts <- "layout_in_circle"
  } else {
    layouts <- "layout_with_fr"
  }

  if (is.null(selected_nodes) || length(selected_nodes) > 1) {
    list_nodesIdSelection <- list(enabled = TRUE)
  } else {
    list_nodesIdSelection <- list(
      enabled = TRUE,
      selected = selected_nodes
    )
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

#' Converts a raw network dataframe to nodes and edges lists.
#'
#' @param raw_graph_data The dataframe of synchronicity or neighbours
#' @param date_range The list of start and end date range
#' @param threshold_selected A vector of selected threshold
#'
#' @return A list contains nodes and edges list
nodes_edges_list_synchronicity <- function(raw_graph_data,
                                           date_range,
                                           threshold_selected) {
  edges <- combine_edges(
    raw_graph_data,
    date_range[[1]],
    date_range[[2]],
    threshold_selected
  )

  g <- .make_tidygraph(edges)
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

#' Plots a visNetwork visualization for global displacement.
#'
#' @param nodes The dataframe with a nodes list
#' @param edges The dataframe with an edges list
#' @param layouts_type A string value that represents the layout type, defaults to "Circle"
#'
#' @return A visNetwork plot for global displacement
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

#' Plots a visNetwork visualization for displacement in the star layout.
#'
#' @param nodes The dataframe with nodes list
#' @param edges The dataframe with edges list
#'
#' @return A visNetwork plot for displacement in star layout
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

#' Gets edges list from a raw network dataframe.
#'
#' @param x The dataframe of synchronicity or neighbours data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param threshold A vector of selected threshold, defaults to 0.9
#'
#' @return A dataframe of edges list
combine_edges <- function(x, from_date = NULL, to_date = NULL, threshold = 0.9) {

  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf

  # combine list into one long data frame
  edgelist <- tbl(con, x) %>%
    filter(
      date >= from_date,
      date <= to_date
    ) %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    as.data.frame()

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
        title = paste0(from, " and ", to, ": ", weight, " secs")
      ) %>%
      filter(width >= 1)
  } else {
    edgelist <- edgelist %>%
      mutate(
        width = weight / mean(weight),
        title = paste0(from, " and ", to, ": ", weight, " secs")
      )
  }

  # return the edgelist
  edgelist
}

#' Combines displacement dataframe for creating the edges list
#'
#' @param x The dataframe of synchronicity or neighbours data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#'
#' @return A dataframe of combined data
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

  combo_df <- tbl(con, x) %>%
    filter(
      date >= from_date,
      date <= to_date,
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    as.data.frame() %>%
    group_by(from, to) %>%
    summarise(weight = n()) %>%
    ungroup()
}

#' Gets edges list from a combined displacement dataframe.
#'
#' @param x The dataframe of displacement data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#' @param threshold A vector of selected threshold, defaults to 0.9
#'
#' @return A dataframe of edges list
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
        title = paste0("Displacements: ", weight)
      ) %>%
      filter(width >= 1)
  } else {
    edgelist <- combo_df %>%
      mutate(
        width = weight / mean(weight),
        title = paste0(from, " and ", to, ": ", weight, " secs")
      )
  }
}

#' Combines displacement dataframe for creating the edges list in star layout
#'
#' @param x The dataframe of displacement data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param cow_id A vector of center cow, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#'
#' @return A dataframe of combined data
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

  combo_df <- tbl(con, x) %>%
    filter(
      date >= from_date,
      date <= to_date,
      CD <= CD_max,
      CD >= CD_min
    ) %>%
    as.data.frame() %>%
    filter(
      (from == cow_id) | (to == cow_id)
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

#' Combines displacement dataframe for creating the edges list in paired layout
#'
#' @param x The dataframe of displacement data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param cow_id_1 A vector of the 1st interested cow, defaults to NULL
#' @param cow_id_2 A vector of the 2nd interested cow, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#'
#' @return A dataframe of combined data
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

#' Gets nodes list from an edges list.
#'
#' @param df The edges list
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param size The dataframe of the degree calculated from igraph
#'
#' @return A dataframe of nodes list
combine_nodes <- function(df,
                          from_date = NULL,
                          to_date = NULL,
                          size) {
  df <- tbl(con, df) %>%
    filter(
      date >= from_date,
      date <= to_date
    ) %>%
    as.data.frame()

  nodes <- data.frame(id = unique(c(
    df$from,
    df$to
  ))) %>%
    mutate(
      label = id
    ) %>%
    arrange(id)

  nodes$size <- size[match(nodes$id, names(size))]

  nodes[is.na(nodes)] <- 2

  return(nodes)
}

#' Gets nodes list from an edges list for displacement data.
#'
#' @param x The dataframe of displacement data
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#' @param cow_id A vector of center cow, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#' @param deg The dataframe of the degree calculated from igraph
#'
#' @return A dataframe of nodes list
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

#' Gets nodes list from an edges list for displacement data in star layout.
#'
#' @param edges The edges list
#' @param cow_id A vector of center cow, defaults to NULL
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#'
#' @return A dataframe of nodes list
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
        id == cow_id ~ paste0(
          "Center Cow",
          "<br>Mean Elo: ", Elo_mean
        ),
        id != cow_id ~ paste0(
          "Cow: ", id,
          "<br>Displacements with Center: ", deg,
          "<br>Mean Elo: ", Elo_mean
        )
      )
    )
}

#' Gets nodes list from an edges list for displacement data in paired layout.
#'
#' @param edges The edges list
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#'
#' @return A dataframe of nodes list
combine_replace_nodes_paired <- function(edges,
                                         from_date = NULL,
                                         to_date = NULL) {
  nodes <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    left_join(combine_elo_star(from_date, to_date), by = "id") %>%
    arrange(desc(Elo_mean)) %>%
    mutate(
      label = paste(id)
    )

  if (nrow(nodes) > 0) {
    nodes$color <- c("#F7766D", "#6fa8dc")
  }

  return(nodes)
}

#' Combines elo score data filtered from date range and color codes the diplacement network in star layout
#'
#' @param from_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis, defaults to NULL
#' @param to_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis, defaults to NULL
#'
#' @return A dataframe
combine_elo_star <- function(from_date = NULL,
                             to_date = NULL) {
  # set defaults
  from_date <- from_date %||% -Inf
  to_date <- to_date %||% Inf

  combo_df <- dominance_df %>%
    mutate(
      Date = as.Date(Date),
      id = as.numeric(Cow)
    ) %>%
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

#' Creates an igraph graph from the edges list
#'
#' @param edgelist The edges list, defaults to NULL
#' @param directed A boolean value represents if a directed graph is needed, defaults to FALSE
#'
#' @return A igraph graph
.make_tidygraph <- function(edgelist = NULL, directed = FALSE) {
  edgelist <- edgelist
  g <- graph_from_data_frame(edgelist, directed = directed)

  # return the graph
  g
}

make_tidygraph <- memoise::memoise(.make_tidygraph)

#' Processes raw data into an edges list
#'
#' @param x The dataframe of raw data
#' @param upper_only A boolean value represents if raw data has lower triangle being all zeros, defaults to FALSE
#'
#' @return A dataframe
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

#' Catches errors if there are any missing date inputs in the networks
#'
#' @param date_range The input date range from the date range widget
#' @param df The data frame for the selected network, defaults to NULL
#' @param network The input for the selected network, defaults to NULL
#'
#' @return error_message if there is a date input issue that needs to stop the graph generation
missing_date_range_check <- function(date_range, df = NULL, network = NULL) {
  `%!in%` <- Negate(`%in%`)

  df <- tbl(con, df) %>%
    select(date) %>%
    distinct() %>%
    arrange(date) %>%
    as.data.frame()
  df_dates <- sort(unique(df$date))

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

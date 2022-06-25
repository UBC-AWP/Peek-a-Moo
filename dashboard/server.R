library(shinymanager)

passphrase <- Sys.getenv("PASSPHRASE")

# credentials <- data.frame(
#   user = c("guest", "user", "admin"), # mandatory
#   password = c("guest", "shiny", "shinymanager"), # mandatory
#   admin = c(FALSE, FALSE, TRUE),
#   stringsAsFactors = FALSE
# )

# Set up shiny server
server <- function(input, output, session) {

  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
     # credentials
      "../auth/database.sqlite",
      passphrase = passphrase
    )
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  user <- reactive({
    res_auth$user[[1]]
  })

  # Call to determine user access, and welcome modal

  config <- NULL
  data_config <- NULL

  observeEvent(user(), {
    if (is.null(user()) == TRUE) {
      config <<- c("zoomIn2d", "zoomOut2d")
      data_config <<- list(
        scrollX = TRUE,
        pageLength = 5,
        dom = "t"
      )
    } else if (user() == "guest") {
      config <<- c("toImage", "zoomIn2d", "zoomOut2d")
      data_config <<- list(
        scrollX = TRUE,
        pageLength = 1,
        dom = "t"
      )
      shinyalert("Welcome to the UBC AWP Dairy Cow Dashboard!",
        "Please select a network from the drop down menu in the \"Global Customizations\" panel to begin.",
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "Let's get mooving!",
        confirmButtonCol = "#6b96c7",
        imageUrl = "../welcome.png",
        imageWidth = 100,
        imageHeight = 100
      )
    } else if ((user() == "user")) {
      config <<- c("zoomIn2d", "zoomOut2d")
      data_config <<- list(
        scrollX = TRUE,
        pageLength = 5,
        dom = "Bftip"
      )
      shinyalert("Welcome to the UBC AWP Dairy Cow Dashboard!",
        "Please select a network from the drop down menu in the \"Global Customizations\" panel to begin.",
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "Let's get mooving!",
        confirmButtonCol = "#6b96c7",
        imageUrl = "../welcome.png",
        imageWidth = 100,
        imageHeight = 100
      )
    } else {
      config <<- c("zoomIn2d", "zoomOut2d")
      data_config <<- list(
        scrollX = TRUE,
        pageLength = 5,
        dom = "Bftip",
        buttons = list(list(extend = "csv", title = "Data_Download"))
      )
      shinyalert("Welcome to the UBC AWP Dairy Cow Dashboard!",
        "Please select a network from the drop down menu in the \"Global Customizations\" panel to begin.",
        closeOnClickOutside = TRUE,
        showConfirmButton = TRUE,
        confirmButtonText = "Let's get mooving!",
        confirmButtonCol = "#6b96c7",
        imageUrl = "../welcome.png",
        imageWidth = 100,
        imageHeight = 100
      )
    }
  })

  # Warning section
  observe({
    warning_df <- combine_warnings(
      food_cuttoff = input$food_intake,
      water_cuttoff = input$water_intake,
      bin_cuttoff = input$bin_volume
    )

    output$warning_table <- format_dt_table(warning_df, page_length = 20, data_config = data_config)

    output$warning_plot <- DT::renderDataTable(
      {
        warning_df %>%
          filter(date == max(date)) %>%
          t() %>%
          as.data.frame() %>%
          filter(V1 != "")
      },
      colnames = c("Warning Type", "Warning"),
      options = list(
        pageLength = 50,
        dom = "ft"
      )
    )
  })

  # contact menu
  output$contact <- renderMenu({
    dropdownMenu(
      type = "messages", badgeStatus = NULL, headerText = "Feedback and questions",
      messageItem(
        from = "Contact us",
        message =  "animal.welfare@ubc.ca",
        icon = icon("envelope"),
        href = "mailto:animal.welfare@ubc.ca"
      )
    )
  })

  # github issues contribute
  output$github <- renderMenu({
    dropdownMenu(
      type = "messages", badgeStatus = NULL, headerText = "See an issue or want to contribute?",
      messageItem(
        from = "Issues / Contributions",
        message =  "Visit our GitHub repository",
        icon = icon("github", lib = "font-awesome"),
        href = "https://github.com/UBC-AWP/Peek-a-Moo"
      ),
      icon = icon("github", lib = "font-awesome")
    )
  })


  # update cow selections based on selected dates
  observe({
    update_cow_selection(input$behaviour_date_range, "behaviour_cow_selection", session)
  })
  observe({
    update_cow_selection(input$daily_date, "daily_cow_selection", session)
  })

  observe({
    update_cow_selection_neighbour(
      input$relationship_date_range, 
      "analysis_cow_id", 
      session)
  })

  observe({
    req(input$relationship_date_range)

    update_cow_selection_displacement(
      date_obj = input$relationship_date_range,
      inputId = "star_cow_selection",
      session = session
    )
  })

  observe({
    req(input$relationship_date_range)

    update_cow_selection_displacement(
      date_obj = input$relationship_date_range,
      inputId = "paired_cow_selection_1",
      session = session
    )
  })

  observe({
    req(input$relationship_date_range)
    req(input$paired_cow_selection_1)

    update_2nd_cow_selection_displacement(
      date_obj = input$relationship_date_range,
      inputId = "paired_cow_selection_2",
      session = session,
      cow_id_1 = input$paired_cow_selection_1,
      CD_min = input$paired_cd_range[[1]],
      CD_max = input$paired_cd_range[[2]]
    )
  })
  
  values <- reactiveValues(nodes_feeding = NULL, 
                           edges_feeding = NULL, 
                           nodes_lying = NULL,
                           edges_lying = NULL,
                           nodes_neighbour = NULL,
                           edges_neighbour = NULL)
  
  # render Synchronicity network
  observe({
    req(input$relationship_date_range)
    req(input$relationship_network_selection)
    
    `%!in%` <- Negate(`%in%`)
    
    threshold_id <- input$relationship_threshold_selection
    threshold_df <- data.frame(threshold = c(0.95, 0.9, 0.75, 0))
    rownames(threshold_df) <- c("5%", "10%", "25%", "All")
    threshold_selected <- threshold_df[threshold_id, ]
    
    if (input$relationship_network_selection == "Synchronicity") {
      if (input$relationship_date_range[[1]] > input$relationship_date_range[[2]]) {
        output$feeding_plot <- visNetwork::renderVisNetwork({
          validate(
            need(
              input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
              paste0(
                "Ending date must come after the starting date. Please select a different starting date."
              )
            )
          )
        })
        
        output$lying_plot <- visNetwork::renderVisNetwork({
          validate(
            need(
              input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
              paste0(
                "Ending date must come after the starting date. Please select a different starting date."
              )
            )
          )
        })
      } else {
        values$nodes_feeding <- nodes_edges_list_synchronicity("Feeding_drinking_at_the_same_time_total_time",
                                                               input$relationship_date_range,
                                                               threshold_selected)[[1]]
        values$edges_feeding <- nodes_edges_list_synchronicity("Feeding_drinking_at_the_same_time_total_time",
                                                               input$relationship_date_range,
                                                               threshold_selected)[[2]]
        
        values$nodes_lying <- nodes_edges_list_synchronicity("synchronized_lying_total_time",
                                                             input$relationship_date_range,
                                                             threshold_selected)[[1]]
        values$edges_lying <- nodes_edges_list_synchronicity("synchronized_lying_total_time",
                                                             input$relationship_date_range,
                                                             threshold_selected)[[2]]
      }
    } else if (input$relationship_network_selection == "Neighbour") {
      if (input$relationship_date_range[[1]] > input$relationship_date_range[[2]]) {
        output$neighbour_plot <- visNetwork::renderVisNetwork({
          validate(
            need(
              input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
              paste0(
                "Ending date must come after the starting date. Please select a different starting date."
              )
            )
          )
        })
      } else {
        values$nodes_neighbour <- nodes_edges_list_synchronicity("Feeding_drinking_neighbour_total_time",
                                                                 input$relationship_date_range,
                                                                 threshold_selected)[[1]]
        values$edges_neighbour <- nodes_edges_list_synchronicity("Feeding_drinking_neighbour_total_time",
                                                               input$relationship_date_range,
                                                               threshold_selected)[[2]]
      }
    }
  })

  # render network
  observe({
    req(input$relationship_date_range)
    req(input$relationship_network_selection)
    

    `%!in%` <- Negate(`%in%`)

    threshold_id <- input$relationship_threshold_selection
    threshold_df <- data.frame(threshold = c(0.95, 0.9, 0.75, 0))
    rownames(threshold_df) <- c("5%", "10%", "25%", "All")
    threshold_selected <- threshold_df[threshold_id, ]
    layouts_type <- input$relationship_layout_selection

    # check for erroneous start date
    if (input$relationship_date_range[[1]] > input$relationship_date_range[[2]]) {
      output$network_plot <- visNetwork::renderVisNetwork({
        validate(
          need(
            input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
            paste0(
              "Ending date must come after the starting date. Please select a different starting date."
            )
          )
        )
      })
    } else {

      # select network to plot
      if (!(input$relationship_network_selection %in% c("Displacement", "Displacement Star*", "Displacement Paired"))) {
        if (input$relationship_network_selection == "Neighbour") {
          
          if (!(is.null(missing_date_range_check(input$relationship_date_range,
                                                 df = "Feeding_drinking_neighbour_total_time",
                                                 network = input$relationship_network_selection
          )))) {
            output$neighbour_plot <- missing_date_range_check(input$relationship_date_range,
                                                            df = "Feeding_drinking_neighbour_total_time",
                                                            network = input$relationship_network_selection
            )
          } else {
            
            if (length(input$current_neighbour) == 0) {
              output$neighbour_plot <- visNetwork::renderVisNetwork({
                plot_network(values$nodes_neighbour, values$edges_neighbour, layouts_type, selected_nodes = NULL) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_neighbour', nodes.nodes);
                ;}")
              })
              
              output$neighbour_table <- format_dt_table(values$edges_neighbour %>% select(c(from, to, weight)), data_config = data_config)
            } else if (length(input$current_neighbour) == 1) {
              output$neighbour_table <- format_dt_table(
                values$edges_neighbour %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_neighbour) | (to == input$current_neighbour)), 
                data_config = data_config)
            }
            
            ## render R markdown file
            
            # output$downloadReport <- downloadHandler(
            #   filename = function() {
            #     paste0("Cow_",
            #           input$analysis_cow_id,
            #           "_",
            #           input$relationship_date_range[[1]],
            #           "_to_",
            #           input$relationship_date_range[[2]],
            #           '_neighbor_count_report', 
            #           '.', 
            #           switch(
            #       input$analysis_format, PDF = 'pdf', HTML = 'html'
            #             )
            #           )
            #     },
            #   
            #   content = function(file) {
            #     src1 <- normalizePath('report.Rmd')
            #     src2 <- normalizePath('reference.bib')
            #     src3 <- normalizePath('neighbour_report.tex')
            #     
            #     # temporarily switch to the temp dir, in case you do not have write
            #     # permission to the current working directory
            #     owd <- setwd(tempdir())
            #     on.exit(setwd(owd))
            #     file.copy(src1, 'report.Rmd', overwrite = TRUE)
            #     file.copy(src2, 'reference.bib', overwrite = TRUE)
            #     file.copy(src3, 'neighbour_report.tex', overwrite = TRUE)
            #     
            #     from_date_neighbour <- input$relationship_date_range[[1]]
            #     to_date_neighbour <- input$relationship_date_range[[2]] 
            #     Feeding_drinking_neighbour_bout <- tbl(con,"Feeding_drinking_neighbour_bout") %>%
            #       filter(
            #         date >= !!(from_date_neighbour),
            #         date <= !!(to_date_neighbour)
            #       ) %>%
            #       as.data.frame()
            #     # Set up parameters to pass to Rmd document
            #     params <- list(
            #       data = Feeding_drinking_neighbour_bout,
            #       cow_id = input$analysis_cow_id, 
            #       date_range = input$relationship_date_range
            #     )
            #     # Knit the document, passing in the `params` list, and eval it in a
            #     # child of the global environment (this isolates the code in the document
            #     # from the code in this app).
            #     out <- rmarkdown::render(
            #       'report.Rmd', 
            #       switch(
            #         input$analysis_format,
            #         PDF = pdf_document(fig_caption = TRUE,        
            #                            includes = includes(in_header =  "neighbour_report.tex")), 
            #         HTML = html_document(toc = TRUE)
            #       ),
            #       params = params,
            #       envir = new.env(parent = globalenv())
            #     )
            #     file.rename(out, file)
            #   }
            # )
            
          }
        } else {
          
          if (!(is.null(missing_date_range_check(input$relationship_date_range,
                                                 df = "Feeding_drinking_at_the_same_time_total_time",
                                                 network = input$relationship_network_selection
          )))) {
            output$feeding_plot <- missing_date_range_check(input$relationship_date_range,
                                                            df = "Feeding_drinking_at_the_same_time_total_time",
                                                            network = input$relationship_network_selection
            )
          } else if (!(is.null(missing_date_range_check(input$relationship_date_range,
                                                        df = "synchronized_lying_total_time",
                                                        network = input$relationship_network_selection
          )))) {
            output$lying_plot <- missing_date_range_check(input$relationship_date_range,
                                                            df = "synchronized_lying_total_time",
                                                            network = input$relationship_network_selection
            )
          } else {
            if (length(input$current_feeding) == 0 && length(input$current_lying) == 0) {
              output$feeding_plot <- visNetwork::renderVisNetwork({
                plot_network(values$nodes_feeding, values$edges_feeding, layouts_type, selected_nodes = NULL) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_feeding', nodes.nodes);
                ;}")
              })
              
              output$feeding_table <- format_dt_table(values$edges_feeding %>% select(c(from, to, weight)), data_config = data_config)
              
              output$lying_plot <- visNetwork::renderVisNetwork({
                plot_network(values$nodes_lying, values$edges_lying, layouts_type, selected_nodes = NULL) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_lying', nodes.nodes);
                ;}")
              })
              
              output$lying_table <- format_dt_table(values$edges_lying %>% select(c(from, to, weight)), data_config = data_config)
            } else if (length(input$current_feeding) > 0) {
              output$lying_plot <- visNetwork::renderVisNetwork({
                plot_network(values$nodes_lying, values$edges_lying, layouts_type, selected_nodes = input$current_feeding) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_lying', nodes.nodes);
                ;}")
              })
              
              output$feeding_table <- format_dt_table(
                values$edges_feeding %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_feeding) | (to == input$current_feeding)), 
                data_config = data_config
                )
              
              output$lying_table <- format_dt_table(
                values$edges_lying %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_feeding) | (to == input$current_feeding)), 
                data_config = data_config
              )
            } else if (length(input$current_lying) > 0) {
              output$feeding_plot <- visNetwork::renderVisNetwork({
                plot_network(values$nodes_feeding, values$edges_feeding, layouts_type, selected_nodes = input$current_lying) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_feeding', nodes.nodes);
                ;}")
              })
              
              output$feeding_table <- format_dt_table(
                values$edges_feeding %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_lying) | (to == input$current_lying)), 
                data_config = data_config
              )
              
              output$lying_table <- format_dt_table(
                values$edges_lying %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_lying) | (to == input$current_lying)), 
                data_config = data_config
                )
            } 
          }
        }
      } else {
        # displacement network setup
        raw_graph_data <- "master_feed_replacement_all"

        if (!(is.null(missing_date_range_check(input$relationship_date_range,
          df = raw_graph_data,
          network = input$relationship_network_selection
        )))) {
          output$network_plot <- missing_date_range_check(input$relationship_date_range,
            df = raw_graph_data,
            network = input$relationship_network_selection
          )
        } else {
          if (input$relationship_network_selection == "Displacement") {
            # Plot Displacement network
            
            edges <- combine_replace_edges(raw_graph_data,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]],
              CD_min = input$cd_range[[1]],
              CD_max = input$cd_range[[2]],
              threshold_selected
            )

            g <- .make_tidygraph(edges, directed = TRUE)
            deg <- degree(g, mode = "all")

            nodes <- combine_replace_nodes(
              raw_graph_data,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]],
              CD_min = input$cd_range[[1]],
              CD_max = input$cd_range[[2]],
              deg = deg
            )

            if (mean(edges$width > 2)) {
              edges$width <- edges$width / 2
            }
            
            if (length(input$current_disp) == 0) {
              output$network_plot <- visNetwork::renderVisNetwork({
                plot_network_disp(nodes, edges, layouts_type) %>%
                  visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_disp', nodes.nodes);
                ;}")
              })
              
              output$network_table <- format_dt_table(edges %>% select(c(from, to, weight)), data_config = data_config)
            } else if (length(input$current_disp) == 1) {
              output$network_disp_table <- format_dt_table(
                edges %>% 
                  select(c(from, to, weight)) %>%
                  filter((from == input$current_disp) | (to == input$current_disp)), 
                data_config = data_config)
            }
          } else if (input$relationship_network_selection == "Displacement Star*") {
            cow_id <- input$star_cow_selection

            edges <- combine_replace_edges_star(raw_graph_data,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]],
              cow_id = cow_id,
              CD_min = input$star_cd_range[[1]],
              CD_max = input$star_cd_range[[2]]
            )
            edges$width <- edges$weight
            if (mean(edges$width > 2)) {
              edges$width <- edges$width / 2
            }

            nodes <- combine_replace_nodes_star(
              edges,
              cow_id,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]]
            )

            output$network_plot <- visNetwork::renderVisNetwork({
              plot_network_disp_star(nodes, edges)
            })
            output$network_table <- format_dt_table(edges %>% select(c(from, to, weight, type)), data_config = data_config)
          } else {
            cow_id_1 <- input$paired_cow_selection_1
            cow_id_2 <- input$paired_cow_selection_2

            edges <- combine_replace_edges_paired(raw_graph_data,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]],
              cow_id_1 = cow_id_1,
              cow_id_2 = cow_id_2,
              CD_min = input$paired_cd_range[[1]],
              CD_max = input$paired_cd_range[[2]]
            )

            edges$width <- edges$weight

            nodes <- combine_replace_nodes_paired(
              edges,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]]
            )

            output$network_plot <- visNetwork::renderVisNetwork({
              plot_network_disp_star(nodes, edges) %>%
                visNodes(shape = "circle") %>%
                visEdges(length = 200) %>%
                visInteraction(hover = FALSE)
            })
            output$network_table <- format_dt_table(edges %>% select(c(from, to, weight)), data_config = data_config)
          }
        }
      }
    }
  })

  # render elo plot
  observe({
    req(input$relationship_date_range)
    req(input$relationship_network_selection)

    raw_graph_data <- dominance_df

    if (input$relationship_date_range[[1]] > input$relationship_date_range[[2]]) {
      output$elo_plot <- renderPlotly({
        validate(
          need(
            input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
            paste0(
              "Ending date must come after the starting date. Please select a different starting date."
            )
          )
        )
      })
    } else if (!(is.null(missing_date_range_check_plotly(input$relationship_date_range,
                                                         df = raw_graph_data
    )))) {
      output$elo_plot <- missing_date_range_check_plotly(input$relationship_date_range,
                                                         df = raw_graph_data
      )
    } else {
      if (input$relationship_network_selection == "Displacement") {
        output$elo_plot <- renderPlotly({
          plot_elo(raw_graph_data,
                   input$relationship_date_range[[1]],
                   input$relationship_date_range[[2]]
          ) %>%
            layout(legend = list(
              orientation = "h",
              x = 0,
              y = -0.2
            )
            ) %>%
            config(modeBarButtonsToRemove = config)
        })
        
        output$elo_table <- format_dt_table(elo_df(raw_graph_data,
                                                   input$relationship_date_range[[1]],
                                                   input$relationship_date_range[[2]]
        ), data_config = data_config)
      } else if (input$relationship_network_selection == "Displacement Star*") {
        cow_id <- input$star_cow_selection

        output$elo_plot <- renderPlotly({
          plot_elo_star(raw_graph_data,
            input$relationship_date_range[[1]],
            input$relationship_date_range[[2]],
            cow_id = cow_id
          ) %>%
            config(modeBarButtonsToRemove = config)
        })

        output$elo_table <- format_dt_table(elo_df(raw_graph_data,
          input$relationship_date_range[[1]],
          input$relationship_date_range[[2]],
          cow_id_1 = cow_id
        ), data_config = data_config)
      } else if (input$relationship_network_selection == "Displacement Paired") {
        cow_id_1 <- input$paired_cow_selection_1
        cow_id_2 <- input$paired_cow_selection_2

        output$elo_plot <- renderPlotly({
          plot_elo_paired(raw_graph_data,
            input$relationship_date_range[[1]],
            input$relationship_date_range[[2]],
            cow_id_1 = cow_id_1,
            cow_id_2 = cow_id_2
          ) %>%
            layout(legend = list(
              orientation = "h",
              x = 0,
              y = -0.2
            )
            ) %>%
            config(modeBarButtonsToRemove = config)
        })

        output$elo_table <- format_dt_table(elo_df(raw_graph_data,
          input$relationship_date_range[[1]],
          input$relationship_date_range[[2]],
          cow_id_1 = cow_id_1,
          cow_id_2 = cow_id_2
        ), data_config = data_config)
      }
    }
  })

  # render behaviour plots
  observe({

    #' Generate the plot and data tabs for time range plots
    #'
    #' @param df The dataframe containing data to be displayed
    #' @param y_col The column of interest
    #' @param var_name The name of the UI output variable
    plot_cow_date_range <- function(df, y_col, var_name) {

      # filter table
      df <- process_range_data(df, input$behaviour_agg_type, input$behaviour_cow_selection, input$behaviour_date_range)

      # generate table
      output[[paste0(var_name, "_table")]] <- format_dt_table(df, data_config = data_config)

      # generate plot
      output[[paste0(var_name, "_plot")]] <- renderPlotly({
        cow_date_range_plot(df, {{ y_col }}, input$show_average) %>%
          config(modeBarButtonsToRemove = config)
      })
    }

    plot_cow_date_range(
      feed_drink_df,
      `Feeding_Duration(s)`,
      "feed"
    )

    plot_cow_date_range(
      feed_drink_df,
      `Drinking_Duration(s)`,
      "drink"
    )

    plot_cow_date_range(
      standing_bout_df,
      `standing_time(seconds)`,
      "standing_time"
    )

    plot_cow_date_range(
      standing_bout_df,
      `standing_bout`,
      "standing_bout"
    )

    plot_cow_date_range(
      non_nutritive_df,
      `number_of_non_nutritive_visits`,
      "non_nutritive"
    )

    plot_cow_date_range(
      feeding_intake_df,
      `Feeding_Intake(kg)`,
      "feed_intake"
    )
  })

  observe({
    req(input$daily_date)
    req(input$daily_cow_selection)
    
    daily_date <- input$daily_date
    daily_cow_id <- input$daily_cow_selection
    # Create feeding, drinking, and lying_standing dataframes
    feeding <- Cleaned_feeding_original_data %>%
      mutate(Behaviour = 'feeding') %>%
      filter(
        date == as.Date(!!daily_date),
        Cow %in% !!daily_cow_id
             ) %>%
      select(Cow, Behaviour, Start, End, Intake) %>%
      as.data.frame() 

    
    drinking <- tbl(con,"Cleaned_drinking_original_data") %>%
      mutate(Behaviour = 'drinking') %>%
      filter(
        date == as.Date(!!daily_date),
        Cow %in% !!daily_cow_id
        ) %>%
      select(Cow, Behaviour, Start, End, Intake) %>%
      as.data.frame() 
      
    
    lying_standing <- tbl(con,"duration_for_each_bout") %>%
      filter(
        floor_date(Start, 'day') == as.Date(!!daily_date) | floor_date(End, 'day') == as.Date(!!daily_date),
        Cow %in% !!daily_cow_id
            ) %>%
      mutate(Intake = NA) %>%
      select(Cow, Behaviour, Start, End, Intake) %>%
      as.data.frame() 

    # Render daily behavior plot
    df <- daily_schedu_moo_data(feeding, drinking, lying_standing, cow_id = input$daily_cow_selection, date = input$daily_date)
    df_1 <- df %>%
      mutate(Time = as.character(df$Time))
    output$daily_table <- format_dt_table(drop_na(df_1, Cow), data_config = data_config)
    output$daily_plot <- renderPlotly({
      daily_schedu_moo_plot(df) %>%
        config(modeBarButtonsToRemove = config)
    })
    
    totals <- daily_total_schedumoo_info(df)

    output$total_standing <- renderValueBox({
      valueBox(
        tags$p(paste0(format(round((totals[4]/3600),1), big.mark = "."), " hrs"), style = "font-size: 60%;"),
        "Average standing time",
        icon = icon("walking", lib = "font-awesome", style = "font-size: 40px;"),
        color = "red"
      )
    })
    output$total_lying <- renderValueBox({
      valueBox(
        tags$p(paste0(format(round((totals[3]/3600),1), big.mark = "."), " hrs"), style = "font-size: 60%;"),
        "Average lying time",
        icon = icon("bed", lib = "font-awesome", style = "font-size: 40px;"),
        color = "yellow"
      )
    })
    output$total_feeding <- renderValueBox({
      valueBox(
        tags$p(paste0(format(round((totals[2]/3600),1), big.mark = "."), " hrs"), style = "font-size: 60%;"),
        "Average feeding time",
        icon = icon("grain", lib = "glyphicon", style = "font-size: 40px;"),
        color = "green"
      )
    })
    output$total_drinking <- renderValueBox({
      valueBox(
        tags$p(paste0(format(round((totals[1]/60),1), big.mark = "."), " min"), style = "font-size: 60%;"),
        "Average drinking time",
        icon = icon("tint", lib = "glyphicon", style = "font-size: 40px;"),
        color = "blue"
      )
    })
  })

  # Render Relationship tab plots
  observe({
    req(input$relationship_cow_selection)
    req(input$relationship_date_range)



    df <- Replacement_behaviour_by_date
    output$bullying_table <- format_dt_table(df, data_config = data_config)
    output$bullying_plot <- renderPlotly({
      plot_bully_analysis(
        df,
        input$relationship_cow_selection,
        input$relationship_date_range[[1]],
        input$relationship_date_range[[2]]
      ) %>%
        config(modeBarButtonsToRemove = config)
    })
  })

  observe({
    req(input$relationship_date_range)

    if (input$relationship_date_range[[1]] > input$relationship_date_range[[2]]) {
      output$THI_plot <- renderPlotly({
        validate(
          need(
            input$relationship_date_range[[1]] < input$relationship_date_range[[2]],
            paste0(
              "Ending date must come after the starting date. Please select a different starting date."
            )
          )
        )
      })
      output$mean_THI <- renderValueBox({
        valueBox(
          tags$p("Error: date selection"),
          "Average THI",
          color = "light-blue"
        )
      })
      output$max_THI <- renderValueBox({
        valueBox(
          tags$p("Error: date selection"),
          "Max THI",
          color = "navy"
        )
      })
      output$min_THI <- renderValueBox({
        valueBox(
          tags$p("Error: date selection"),
          "Min THI",
          color = "purple"
        )
      })
    } else {
      df <- THI_analysis(THI, input$relationship_date_range[[1]], input$relationship_date_range[[2]])
      output$THI_table <- format_dt_table(df, data_config = data_config)
      output$THI_plot <- renderPlotly({
        plot_THI_analysis(df) %>%
          config(modeBarButtonsToRemove = config)
      })

      output$mean_THI <- renderValueBox({
        valueBox(
          tags$p(paste0(format(round(mean(df$THI_mean), 1), big.mark = ","), " THI"), style = "font-size: 60%;"),
          "Average THI",
          icon = icon("thermometer-half", lib = "font-awesome", style = "font-size: 40px;"),
          color = "light-blue"
        )
      })
      output$max_THI <- renderValueBox({
        valueBox(
          tags$p(paste0(format(round(max(df$THI_max), 1), big.mark = ","), " THI"), style = "font-size: 60%;"),
          "Max THI",
          icon = icon("thermometer-full", lib = "font-awesome", style = "font-size: 40px;"),
          color = "navy"
        )
      })
      output$min_THI <- renderValueBox({
        valueBox(
          tags$p(paste0(format(round(min(df$THI_min), 1), big.mark = ","), " THI"), style = "font-size: 60%;"),
          "Min THI",
          icon = icon("thermometer-empty", lib = "font-awesome", style = "font-size: 40px;"),
          color = "purple"
        )
      })
    }
  })


  # # Feed Bin selection
  # observe({
  #   update_bin_selection(input$bin_date, "behaviour_bin_selection", session)
  # })


  # Feed bin tab
  # observe({
  #   req(input$bin_date)
  #   req(input$behaviour_bin_selection)
  # 
  #   bin_df <- select_feed_bin_data(feed_df,
  #     feed_date = input$bin_date,
  #     bin_selection = input$behaviour_bin_selection
  #   )
  #   # plot
  #   output$feed_bin_plot <- renderPlot({
  #     plot_feed_bin_data(
  #       hourly_df = bin_df,
  #       hr = input$obs_hr,
  #       max_wt = input$bin_weight
  #     )
  #   })
  #   # CSV output
  #   output$feed_bin_table <- format_dt_table(bin_df, data_config = data_config)
  # })
  # 
  # observe({
  #   req(input$bin_date)
  #   req(input$behaviour_bin_selection)
  # 
  #   df <- filter_dates(bin_empty_total_time_summary, date, input$bin_date) %>%
  #     parse_hunger_df(input$behaviour_bin_selection)
  # 
  #   output$hunger_table <- format_dt_table(df, data_config = data_config)
  #   output$hunger_plot <- renderPlotly({
  #     hunger_plot(df) %>%
  #       config(modeBarButtonsToRemove = config)
  #   })
  # })
  
  observe({
    output$downloadReferences <- downloadHandler(
      filename = "citation-references.pdf",
      content = function(file) {
        file.copy("www/citation-references.pdf", file)
      }
    )
  })
}

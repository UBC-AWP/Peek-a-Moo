library(shinymanager)

passphrase <- Sys.getenv("PASSPHRASE")

# Set up shiny server
server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "../auth/database.sqlite",
      passphrase = passphrase
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Warning section
  observe({
    
    warning_df <- combine_warnings(
      food_cuttoff = input$food_intake,
      water_cuttoff = input$water_intake,
      bin_cuttoff = input$bin_volume
    )
    
    output$warning_table <- format_dt_table(warning_df, page_length = 20)

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

    # Warning notifications menu
    output$notifications <- renderMenu({
      get_warning_dropdown(warning_df)
    })
    observeEvent(input$linkClicked, {
      updateTabItems(session, "sidemenu", selected = "warnings")
      output$dropdown <- renderMenu({
        get_warning_dropdown(warning_df)
      })
    })
  })


  # update cow selections based on selected dates
  observe({
    update_cow_selection(input$activity_date_range, "activity_cow_selection", session)
  })
  observe({
    update_cow_selection(input$daily_date, "daily_cow_selection", session)
  })
  observe({
    update_network_selection(input$relationship_network_selection, "relationship_network_selection", session)
  })
  observe({
    update_threshold_selection(input$relationship_threshold_selection, "relationship_threshold_selection", session)
  })

  observe({
    req(input$star_date_range)

    update_cow_selection_displacement(
      date_obj = input$star_date_range,
      inputId = "star_cow_selection",
      session = session
    )
  })

  # render star network
  observe({
    req(input$star_date_range)
    req(input$star_cow_selection)

    raw_graph_data <- master_feed_replacement_all

    cow_id <- input$star_cow_selection

    edges <- combine_replace_edges_star(raw_graph_data,
      input$star_date_range[[1]],
      input$star_date_range[[2]],
      cow_id = cow_id,
      CD_min = input$star_cd_range[[1]],
      CD_max = input$star_cd_range[[2]]
    )
    edges$width <- edges$weight
    if (mean(edges$width > 2)) {
      edges$width <- edges$width / 2
    }

    nodes <- combine_replace_nodes_star(edges, cow_id)

    output$star_plot <- visNetwork::renderVisNetwork({
      plot_network_disp_star(nodes, edges)
    })
    output$star_table <- format_dt_table(edges %>% select(c(from, to, weight, type)))
  })

  # render network
  observe({
    req(input$relationship_date_range)
    req(input$relationship_network_selection)

    `%!in%` <- Negate(`%in%`)

    threshold_id <- input$relationship_threshold_selection
    threshold_df <- data.frame(threshold = c(0.95, 0.9, 0.75))
    rownames(threshold_df) <- c("5%", "10%", "25%")
    threshold_selected <- threshold_df[threshold_id, ]

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
      if (input$relationship_network_selection != "Displacement") {
        if (input$relationship_network_selection == "Lying Synchronicity") {
          raw_graph_data <- synchronized_lying_total_time
        } else if (input$relationship_network_selection == "Feeding Sychronicity") {
          raw_graph_data <- Feeding_drinking_at_the_same_time_total_time
        } else {
          raw_graph_data <- Feeding_drinking_neighbour_total_time
        }

        # check for missing data in network

        if (!(is.null(missing_date_range_check(input$relationship_date_range,
          df = raw_graph_data,
          network = input$relationship_network_selection
        )))) {
          output$network_plot <- missing_date_range_check(input$relationship_date_range,
            df = raw_graph_data,
            network = input$relationship_network_selection
          )
        } else {

          # plot the network (not displacement)
          edges <- combine_edges(
            raw_graph_data,
            input$relationship_date_range[[1]],
            input$relationship_date_range[[2]],
            threshold_selected
          )

          g <- .make_tidygraph(raw_graph_data, edges)
          deg <- degree(g)

          nodes <- combine_nodes(edges, deg)

          if (mean(edges$width > 2)) {
            edges$width <- edges$width / 2
          }

          output$network_plot <- visNetwork::renderVisNetwork({
            plot_network(nodes, edges, threshold_id)
          })

          output$network_table <- format_dt_table(edges %>% select(c(from, to, weight)))
        }
      } else {
        # displacement network setup
        raw_graph_data <- master_feed_replacement_all

        if (!(is.null(missing_date_range_check(input$relationship_date_range,
          df = raw_graph_data,
          network = input$relationship_network_selection
        )))) {
          output$network_plot <- missing_date_range_check(input$relationship_date_range,
            df = raw_graph_data,
            network = input$relationship_network_selection
          )
        } else {

          # Plot Displacement network
            
            edges <- combine_replace_edges(raw_graph_data,
              input$relationship_date_range[[1]],
              input$relationship_date_range[[2]],
              CD_min = input$cd_range[[1]],
              CD_max = input$cd_range[[2]],
              threshold_selected
            )

            g <- .make_tidygraph(raw_graph_data, edges, directed = TRUE)
            deg <- degree(g, mode = "all")

            nodes <- combine_replace_nodes(edges, deg)

            if (mean(edges$width > 2)) {
              edges$width <- edges$width / 2
            }

            output$network_plot <- visNetwork::renderVisNetwork({
              plot_network_disp(nodes, edges, threshold_id)
            })
            output$network_table <- format_dt_table(edges %>% select(c(from, to, weight)))
          }
        }
      }
  })

  # render elo plot
  observe({
    req(input$star_date_range)

    cow_id <- input$star_cow_selection

    raw_graph_data <- dominance_df

    output$elo_plot <- renderPlotly({
      plot_elo(raw_graph_data,
        input$star_date_range[[1]],
        input$star_date_range[[2]],
        cow_id = cow_id
      )
    })

    output$elo_table <- format_dt_table(elo_df(raw_graph_data,
      input$relationship_date_range[[1]],
      input$relationship_date_range[[2]],
      cow_id = cow_id
    ))
  })

  # render activity plots
  observe({

    #' Generate the plot and data tabs for time range plots
    #'
    #' @param df The dataframe containing data to be displayed
    #' @param y_col The column of interest
    #' @param var_name The name of the UI output variable
    plot_cow_date_range <- function(df, y_col, var_name) {
    
      # filter table
      df <- process_range_data(df, input$activity_agg_type, input$activity_cow_selection, input$activity_date_range)
      
      # generate table
      output[[paste0(var_name, "_table")]] <- format_dt_table(df)

      # generate plot
      output[[paste0(var_name, "_plot")]] <- renderPlotly({
        cow_date_range_plot(df, {{ y_col }}, input$show_average)
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
    
    # Create feeding, drinking, and lying_standing dataframes
    feeding <- Cleaned_feeding_original_data
    drinking <- Cleaned_drinking_original_data
    lying_standing <- duration_for_each_bout
    
    # Render daily behavior plot
    df <- daily_schedu_moo_data(feeding, drinking, lying_standing, cow_id = input$daily_cow_selection, date = input$daily_date)
    df_1 <- df %>%
      mutate(Time = as.character(df$Time))
    output$daily_table <- format_dt_table(drop_na(df_1, Cow))
    output$daily_plot <- renderPlotly(daily_schedu_moo_plot(df))
  })
  
  observe({
    req(input$daily_date)
    req(input$daily_cow_selection)
    
    # Create feeding, drinking, and lying_standing dataframes
    feeding <- Cleaned_feeding_original_data
    drinking <- Cleaned_drinking_original_data
    lying_standing <- duration_for_each_bout
    
    # Render daily total behavior plot
    df <- daily_schedu_moo_data(feeding, drinking, lying_standing, cow_id = input$daily_cow_selection, date = input$daily_date)
    df_1 <- df %>%
      mutate(time_for_total = as.integer(df$Time - lag(df$Time))) %>%
      select(c("Cow", "Behaviour", "time_for_total")) %>%
      drop_na() %>%
      group_by(Behaviour) %>%
      summarise(Total_Time = sum(time_for_total))
    output$daily_total_table <- format_dt_table(df_1)
    output$daily_total_plot <- renderPlotly(daily_total_schedumoo_plot(df))
  })

  observe({
    req(input$relationship_cow_selection)
    req(input$relationship_date_range)


    df <- actor_reactor_analysis(make_analysis_df(Replacement_behaviour_by_date))
    output$bullying_table <- format_dt_table(df)
    output$bullying_plot <- renderPlotly({
      plot_bully_analysis(df, input$relationship_cow_selection, input$relationship_date_range[[1]], input$relationship_date_range[[2]])
    })
  })

  observe({
    req(input$relationship_date_range)

    df <- THI_analysis(THI, input$relationship_date_range[[1]], input$relationship_date_range[[2]])
    output$THI_table <- format_dt_table(df)
    output$THI_plot <- renderPlotly({
      plot_THI_analysis(df)
    })
  })


  # Feed Bin selection
  observe({
    update_bin_selection(input$bin_date, "activity_bin_selection", session)
  })


  # feed bin tab
  observe({
    req(input$bin_date)
    req(input$activity_bin_selection)

    bin_df <- select_feed_bin_data(feed_df,
      feed_date = input$bin_date,
      bin_selection = input$activity_bin_selection
    )
    # plot
    output$feed_bin_plot <- renderPlot({
      plot_feed_bin_data(
        hourly_df = bin_df,
        hr = input$obs_hr,
        max_wt = input$bin_weight
      )
    })
    # CSV output
    output$feed_bin_table <- format_dt_table(bin_df)
  })

  observe({
    req(input$bin_date)
    req(input$activity_bin_selection)

    df <- filter_dates(bin_empty_total_time_summary, date, input$bin_date) %>%
      parse_hunger_df(input$activity_bin_selection)

    output$hunger_table <- format_dt_table(df)
    output$hunger_plot <- renderPlotly({
      hunger_plot(df)
    })
  })
}
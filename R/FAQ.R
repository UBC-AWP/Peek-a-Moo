#' A helper function for calling the methodologies and data
#' written section into ui.R
#' 
methodologies_data <- function(){
  fluidRow(
    h2(paste0('Methodologies by Tab:')),
    tags$u(tags$b( "Relationships")), 
    tags$li(tags$b("Networks:"),
            "Created with the ",
            tags$a("visNetwork package.",
                   href = "https://datastorm-open.github.io/visNetwork/"),
            "The network is calcualted with nodes representing individual cows, and the edges representing total synchronized lying/standing time, or count of bouts/interactions between cows."),
    tags$li(tags$b("THI:"),
            "Created with the ",
            tags$a("Plotly package.",
                   href = "https://plotly.com/r/"),
    "The plot shows the daily max, min and average THI values over a given timeline, with the grey dashed line representing the threshold for when cows go into \"heat stress\". The value boxes above show the total max, min, and average THI for the given period."),
    tags$u(tags$b( "Activity Patterns")),
    tags$li(tags$b("All plots:"),
            "Created with the ",
            tags$a("Plotly package.",
                   href = "https://plotly.com/r/"),
            "The plots show daily values for various behaviours over a given timeline. The herd average is defaulted and shown in red."),
    
  )
}
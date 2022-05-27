library(shinymanager)

header <- dashboardHeader(
  title = "Dairy Cow Dashboard",
  dropdownMenuOutput("notifications")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidemenu",
    menuItem("Activity Patterns", icon = icon("chart-line"), tabName = "activities"),
    menuItem("Daily Behavior", icon = icon("calendar"), tabName = "daily_behavior"),
    menuItem("Relationships", icon = icon("connectdevelop"), tabName = "relationships"),
    menuItem("Bins", icon = icon("chart-bar"), tabName = "bins"),
    menuItem("Warnings", icon = icon("exclamation-triangle"), tabName = "warnings"),
    menuItem("Source code",
      icon = icon("file-code"),
      href = "https://github.com/UBC-AWP/Peek-a-Moo"
    )
  )
)

activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2, aggregation_widget("activity_agg_type")),
      column(
        2,
        checkboxInput(
          inputId = "show_average",
          label = "Show Dashed Average Lines",
          value = FALSE
        )
      ),
      column(4, date_range_widget("activity_date_range")),
      column(4, cow_selection_widget("activity_cow_selection"))
    )
  ),
  fluidRow(
    default_tabBox("Feeding Duration", "feed"),
    default_tabBox("Drinking Duration", "drink")
  ),
  fluidRow(
    default_tabBox("Standing Duration", "standing_time"),
    default_tabBox("Standing Bout Duration", "standing_bout")
  ),
  fluidRow(
    default_tabBox("Non-nutritive Visits", "non_nutritive"),
    default_tabBox("Feed Intake", "feed_intake")
  )
)

daily_tab <- tabItem(
  "daily_behavior",
  fluidRow(
    box(
      title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(4, date_widget("daily_date")),
      column(4, cow_selection_widget("daily_cow_selection")),
      column(4, h5(br(), "Please select a valid date and cow(s) to generate the plots below"))
    )
  ),
  fluidRow(
    default_tabBox("Daily Behavior", "daily", width = 12)
  ),
  fluidRow(
    default_tabBox("Daily Behaviour Totals", "daily_total", width = 12)
  ),
)

relationships_tab <- tabItem(
  "relationships",
  fluidRow(
    box(
      title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(4, date_range_widget("relationship_date_range")),
      column(4, network_selection_widget("relationship_network_selection", multiple = FALSE)),
      column(4, threshold_selection_widget("relationship_threshold_selection", multiple = FALSE)),
      column(12, sliderInput("cd_range", "Competition Density (Displacement network only.)", min = 0, max = 1, value = c(0.2, 0.5)))
    )
  ),
  fluidRow(
    default_tabBox("Social Network", "network", 
                   width = 12, 
                   output_fun = visNetworkOutput)
  ),
  fluidRow(
    default_tabBox("THI", "THI", width = 12)
  )
)

star_tab <- tabItem(
  "star",
  fluidRow(
    box(
      title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(6, date_range_widget("star_date_range")),
      column(6, cow_selection_widget("star_cow_selection", multiple = FALSE, label = "Cow of Interest")),
      column(12, sliderInput("star_cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5)))
    )
  ),
  fluidRow(
    default_tabBox("Star Network", "star", 
                   width = 12, 
                   output_fun = visNetworkOutput)
  ),
  fluidRow(
    default_tabBox("Dominance", "elo", width = 12)
  )
)

bins_tab <- tabItem(
  "bins",
  fluidRow(
    box(
      title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      fluidRow(
        column(4, date_widget("bin_date"), ),
        column(8, bin_selection_widget("activity_bin_selection")),
      )
    )
  ),
  fluidRow(
    default_tabBox("Hunger Plot", "hunger", width = 12)
  ),
  fluidRow(
    box(
      title = "Hourly Feed Bin Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      fluidRow(
        column(8, sliderInput("obs_hr", "Hour", min = 0, max = 23, value = 12)),
        column(4, bin_wt_widget("bin_weight"))
      )
    )
  ),
  fluidRow(
    default_tabBox("Hourly Feed Bin Data", "feed_bin", width = 12, output_fun = plotOutput)
  )
)

warnings_tab <- tabItem(
  "warnings",
  box(
    title = "Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
    column(
      4,
      numericInput(
        inputId = "food_intake",
        label = "Food Intake Cuttoff",
        value = 0,
        min = 0
      )
    ),
    column(
      4,
      numericInput(
        inputId = "water_intake",
        label = "Water Intake Cuttoff",
        value = 0,
        min = 0
      )
    ),
    column(
      4,
      numericInput(
        inputId = "bin_volume",
        label = "Bin Volume Cuttoff",
        value = 0,
        min = 0
      )
    )
  ),
  default_tabBox("Warnings", "warning", width = 12, output_fun = DT::dataTableOutput)
)

body <- dashboardBody(
  tags$script(
    HTML(
      "function clickFunction(link){
        Shiny.onInputChange('linkClicked',link);
      }"
    )
  ),
  tabItems(
    activities_tab,
    daily_tab,
    relationships_tab,
    star_tab,
    bins_tab,
    warnings_tab
  )
)

ui <- fluidPage(dashboardPage(header, sidebar, body, skin = "blue"))

ui <- secure_app(ui,
                   # add image on top ?
                   tags_top = 
                     tags$div(
                       tags$h2("Peek-a-Moo\nDashboard", style = "align:center"),
                       tags$img(
                         src = "../loading_cow1.gif", width = 100),
                       tags$p(
                         "General visitors can contact Borbala for user name and password."
                       )),
                   # add information on bottom ?
                   tags_bottom = tags$div(
                     tags$p(
                       "For any question, please  contact ",
                       tags$a(
                         href = "mailto:someone@example.com?Subject=Shiny%20aManager",
                         target="_top", "administrator"
                       ))),
                 enable_admin = FALSE,
                 download = NULL)

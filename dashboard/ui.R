library(shinymanager)

header <- dashboardHeader(
  title = tags$a(
    href = "https://awp.landfood.ubc.ca/",
    tags$img(src = "logo_white.png", height = "50", width = "210")
  ),
  dropdownMenuOutput("contact"),
  dropdownMenuOutput("github")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidemenu",
    menuItem(HTML(paste("&nbsp; Relationships")), icon = icon("connectdevelop"), tabName = "relationships"),
    menuItem("Behaviour Patterns", icon = icon("chart-line"), tabName = "activities"),
    menuItem("Daily Behaviour", icon = icon("calendar"), tabName = "daily_behavior"),
   # menuItem("Bins", icon = icon("chart-bar"), tabName = "bins"),
    menuItem("Warnings", icon = icon("exclamation-triangle"), tabName = "warnings"),
    menuItem("FAQ", icon = icon("question-circle"), tabName = "FAQ")
  )
)

# Activities tab
activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_behaviour{border-radius: 0px;border-width: 0px}"),
        bsButton("button_behaviour", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2, aggregation_widget("behaviour_agg_type")),
      column(
        2,
        checkboxInput(
          inputId = "show_average",
          label = "Show Average for Selected Group",
          value = FALSE
        )
      ),
      column(4, date_range_widget("behaviour_date_range")),
      column(4, cow_selection_widget("behaviour_cow_selection")),
      bsPopover(
        id = "button_behaviour", title = "Behaviour Patterns Tab",
        content = paste("This tab shows six behaviour patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
          "",
          "<b>Customizations:</b>",
          "<u>Aggregate</u> - aggregates the data on the daily, or monthly level.",
          "<u>Date Range</u> - timeline for the plots.",
          "<u>Cows</u> - cow(s) to showcase in the plots.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    default_tabBox("Feeding Duration", "feed"),
    default_tabBox("Drinking Duration", "drink")
  ),
  fluidRow(
    default_tabBox("Standing Duration", "standing_time"),
    default_tabBox("Standing Bouts", "standing_bout")
  ),
  fluidRow(
    default_tabBox("Non-nutritive Visits", "non_nutritive"),
    default_tabBox("Feed Intake", "feed_intake")
  )
)

# Daily tab
daily_tab <- tabItem(
  "daily_behavior",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_daily{border-radius: 0px;border-width: 0px}"),
        bsButton("button_daily", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(4, date_widget("daily_date")),
      column(4, cow_selection_widget("daily_cow_selection")),
      column(4, h5(br(), "Please select a valid date and cow(s) to generate the plots below")),
      bsPopover(
        id = "button_daily", title = "Daily Behaviour Tab",
        content = paste("This tab depicts the feeding, lying, standing, and drinking behaviours of selected cows.",
          "",
          "<b>Customizations:</b>",
          "<u>Date</u> - the date for the plots to showcase.",
          "<u>Cows</u> - the cow(s) to showcase in the plots.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    tags$style(".small-box.bg-red { background-color: #ff9999 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("total_standing", width = 3),
    tags$style(".small-box.bg-yellow { background-color: #ffcc66 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("total_lying", width = 3),
    tags$style(".small-box.bg-green { background-color: #b2bb90 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("total_feeding", width = 3),
    tags$style(".small-box.bg-blue { background-color: #6b96c7 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("total_drinking", width = 3)
  ),
  fluidRow(
    default_tabBox(
      title = p(
        "Daily Behaviour",
        tags$style(type = "text/css", "#button_daily_plot{border-radius: 0px;border-width: 0px}"),
        bsButton("button_daily_plot",
          label = "", icon = icon("info-circle", lib = "font-awesome"),
          size = "extra-small"
        )
      ),
      "daily",
      width = 12,
      popover = bsPopover(
        id = "button_daily_plot", title = "Average time boxes:",
        content = paste(
          "<b>Daily behaviour plots:</b>",
          "Average daily value boxes are calculated based on the data presented in the daily behaviour timeline chart. Both plots use duration of bouts and only consider behaviours that are fully contained within the given date. Behaviours that extend beyond midnight or began before midnight the previous day are not considered.",
          "",
          "<u>Warning:</u> Including cows with missing behaviour data will effect the calculation of the average daily value boxes as they are still included in the total number of cows, and their available info is included in the calculation of the average.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  )
)

# Realtionships tab
relationships_tab <- tabItem(
  "relationships",
  fluidRow(
    box(
      title = p(
        "Global Customizations",
        tags$style(type = "text/css", "#button_global_network{border-radius: 0px;border-width: 0px}"),
        bsButton("button_global_network", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ), width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(6, date_range_widget("relationship_date_range")),
      column(6, network_selection_widget("relationship_network_selection", multiple = TRUE))
    ),
    box(
      title = p(
        "Network Customizations",
        tags$style(type = "text/css", "#button_network{border-radius: 0px;border-width: 0px}"),
        bsButton("button_network", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ), width = 12, solidHeader = TRUE, collapsible = TRUE,
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Neighbour' || input.relationship_network_selection == 'Synchronicity' || input.relationship_network_selection == 'Displacement'",
        column(4, threshold_selection_widget("relationship_threshold_selection", multiple = FALSE)),
        column(4, layout_selection_widget("relationship_layout_selection", multiple = FALSE))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement'",
        column(4, sliderInput("cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement Star*'",
        column(4, cow_selection_widget("star_cow_selection", multiple = FALSE, label = "Cow of Interest")),
        column(8, sliderInput("star_cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement Paired'",
        column(4, cow_selection_widget("paired_cow_selection_1", multiple = FALSE, label = "First Cow of Interest")),
        column(4, cow_selection_widget("paired_cow_selection_2", multiple = FALSE, label = "Second Cow of Interest")),
        column(4, sliderInput("paired_cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      bsPopover(
        id = "button_network_info", title = "Network Decriptions",
        content = paste(
          "<b>Neighbours</b> - Cows who feed and drink directly beside each other.",
          "<b>Synchronicity</b> - Split into two networks, this showcases networks of cows who stand and lie at the same time.",
          "<b>Displacement</b> - Displacement is an instance of a cow replacing another cow at the same feeder within a time frame of 23 seconds or less. This network showcases that type of interaction between cows.",
          "<b>Displacement Star*</b> - An indiviual cows displacement interactions.",
          "<b>Displacement Paired</b> - A pair of cows displacement interactions.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      bsPopover(
        id = "button_global_network", title = "Global Network Customizations",
        content = paste(
          "<b>Customizations:</b>",
          "<u>Date Range</u> - timeline for the given plots.",
          "<u>Network</u> - type of network to display.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      bsPopover(
        id = "button_network", title = "Network Specific Customizations",
        content = paste(
          "<b>Customizations (Feeding/Lying networks) :</b>",
          "<u>Layout Type</u> - shape of the displayed network.",
          "<u>Threshold</u> - top weighted % of connections to display in the network.",
          "",
          "<b>Customizations (Displacement networks) :</b>",
          "<u>Competition Density</u> - threshold of competition density to filter connections in the network by.",
          "<u>Threshold (default only)</u> - top weighted % of connections to display in the network.",
          "<u>Cow of Interest (star only) </u> - cow to be showcased in the network.",
          "<u>First/Second Cow of Interest (paired only) </u> - the two cows to be showcased in the network together.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "input.relationship_network_selection == 'Neighbour'",
      report_tabBox("Feeding Neighbours", "neighbour", 
                     width = 12, 
                     output_fun = visNetworkOutput)
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "input.relationship_network_selection == 'Synchronicity'",
      default_tabBox("Feeding Synchronicity", "feeding",
        width = 6,
        output_fun = visNetworkOutput
      ),
      default_tabBox("Lying Synchronicity", "lying",
        width = 6,
        output_fun = visNetworkOutput
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "input.relationship_network_selection == 'Displacement' || input.relationship_network_selection == 'Displacement Star*' || input.relationship_network_selection == 'Displacement Paired'",
      default_tabBox("Social Network", "network",
        width = 6,
        output_fun = visNetworkOutput
      ),
      default_tabBox(
        title = p(
        "Dominance",
        tags$style(type = "text/css", "#button_Dominance_plot{border-radius: 0px;border-width: 0px}"),
        bsButton("button_Dominance_plot",
                 label = "", icon = icon("info-circle", lib = "font-awesome"),
                 size = "extra-small"
        )
      ),
      "elo",
      width = 6,
      popover = bsPopover(
        id = "button_Dominance_plot", title = "Dominance",
        content = 
          paste("The plot shown for the full Displacement network, (most and least dominant cows), is calculated as the cows with the maximum and minimum average elo for the selected period.",
                "",
                "The most dominant cows are shown in red, and the least dominant in blue.",
                sep = "<br>"),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")))
    )
  ),
  fluidRow(
    tags$style(".small-box.bg-light-blue { background-color: #CA929D !important; color: #FFFFFF !important; }"),
    valueBoxOutput("mean_THI", width = 4),
    tags$style(".small-box.bg-navy { background-color: #F7766D !important; color: #FFFFFF !important; }"),
    valueBoxOutput("max_THI", width = 4),
    tags$style(".small-box.bg-purple { background-color: #6b96c7 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("min_THI", width = 4),
  ),
  fluidRow(
    default_tabBox(
      title = p(
        "THI",
      tags$style(type = "text/css", "#button_THI_plot{border-radius: 0px;border-width: 0px}"),
      bsButton("button_THI_plot",
               label = "", icon = icon("info-circle", lib = "font-awesome"),
               size = "extra-small"
      )
    ), 
    "THI",
    width = 12,
    popover = bsPopover(
      id = "button_THI_plot", title = "THI",
      content = 
      paste("THI stands for Temperature Humidity Index and is calculated as:",
            "",
            "<i>THI = 0.8*Temperature + Relative Humidity Index * (Temperature - 14.4) + 46.4 </i>",
            "",
            "THI is an important measure for dairy farms, as dairy cows are very susceptible to heat stress.",
            "",
            "The threshold for heat stress is THI = 68, pictured as the grey dashed line in the plot below.",
            sep = "<br>"),
      placement = "right",
      trigger = "hover",
      options = list(container = "body"))
)
)
)

# Bins tab
# bins_tab <- tabItem(
#   "bins",
#   fluidRow(
#     box(
#       title = p(
#         "Customizations",
#         tags$style(type = "text/css", "#button_bins{border-radius: 0px;border-width: 0px}"),
#         bsButton("button_bins", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
#       ),
#       width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
#       fluidRow(
#         column(4, date_widget("bin_date"), ),
#         column(8, bin_selection_widget("behaviour_bin_selection")),
#         bsPopover(
#           id = "button_bins", title = "Bins Tab",
#           content = paste("This tab shows weight status and interaction information for the feedbins.",
#             "",
#             "<b>Customizations:</b>",
#             "<u>Date</u> - the date for the plots to showcase.",
#             "<u>Bins</u> - feed bin(s) to showcase in the hunger plot.",
#             sep = "<br>"
#           ),
#           placement = "right",
#           trigger = "hover",
#           options = list(container = "body")
#         )
#       )
#     )
#   ),
#   fluidRow(
#     default_tabBox("Hunger Plot", "hunger", width = 12)
#   ),
#   fluidRow(
#     box(
#       title = p(
#         "Customizations",
#         tags$style(type = "text/css", "#button_other{border-radius: 0px;border-width: 0px}"),
#         bsButton("button_other", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
#       ),
#       width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
#       fluidRow(
#         column(8, sliderInput("obs_hr", "Hour", min = 0, max = 23, value = 12)),
#         column(4, bin_wt_widget("bin_weight")),
#         bsPopover(
#           id = "button_other", title = "Behaviour Patterns Tab",
#           content = paste("This tab shows six behaviour patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
#             "",
#             "<b>Customizations:</b>",
#             "<u>Hour</u> - hour of the selected date to observe in the plot",
#             "<u>Full Bin Weight</u> - TBD",
#             sep = "<br>"
#           ),
#           placement = "right",
#           trigger = "hover",
#           options = list(container = "body")
#         )
#       ),
#     )
#   ),
#   fluidRow(
#     default_tabBox("Hourly Feed Bin Data", "feed_bin", width = 12, output_fun = plotOutput)
#   )
# )

# Warnings tab
warnings_tab <- tabItem(
  "warnings",
  box(
    title = p(
      "Customizations",
      tags$style(type = "text/css", "#button_warning{border-radius: 0px;border-width: 0px}"),
      bsButton("button_warning", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
    ),
    width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
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
    ),
    bsPopover(
      id = "button_warning", title = "Warnings Tab",
      content = paste("This tab shows the detected Instatec warnings for the dataset.",
        "",
        "<b>Customizations:</b>",
        "<u>Food Intake Cuttoff</u> - a value kg of feed to set a threshold for the warnings shown below.",
        "<u>Water Intake Cuttoff</u> - a value kg of water to set a threshold for the warnings shown below.",
        "<u>Bin Volume Cuttoff</u> - a value for volume of the bins to set a threshold for the warnings shown below.",
        sep = "<br>"
      ),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
  ),
  default_tabBox("Warnings", "warning", width = 12, output_fun = DT::dataTableOutput)
)

# FAQ tab
FAQ_tab <- tabItem(
  "FAQ",
  tags$style(HTML("#shiny-tab-FAQ{ padding-left: 20px; padding-right: 20px; }")),
  about_FAQ(),
  methodologies_FAQ(),
  data_FAQ(),
  statistical_FAQ(),
  references_FAQ(),
  downloadButton('downloadReferences')
)

# Dashboard body and combining tabs
body <- dashboardBody(
  tags$script(
    HTML(
      "function clickFunction(link){
        Shiny.onInputChange('linkClicked',link);
      }"
    )
  ),
  custom_theme(),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="headerStyling"> Dairy Cow Dashboard </span>\');
      })
     ')),
  tabItems(
    activities_tab,
    daily_tab,
    relationships_tab,
   # bins_tab,
    warnings_tab,
    FAQ_tab
  )
)

ui <- fluidPage(setSliderColor(c("#6b96c7","#6b96c7","#6b96c7","#6b96c7"),
                               c(1,2,3,4)),
                dashboardPage(title="Dairy Cow Dashboard",
                              header,
                              sidebar,
                              body))

# Authentication page
ui <- secure_app(ui,
  tags_top =
    tags$div(
      tags$style(HTML(".btn-primary {
    color: #ffffff;
    background-color: #6b96c7;
    border-color: #6b96c7;
    } .btn-primary:hover {
    color: #ffffff;
    background-color: #6b96c7;
    border-color: #6b96c7;
} .btn-primary:focus {
    color: #ffffff;
    background-color: #6b96c7;
    border-color: #6b96c7;
}  .btn-primary:active:hover {
    color: #ffffff;
    background-color: #6b96c7;
    border-color: #6b96c7;
} a {color:#6b96c7; 
} a:hover {color:#6b96c7; 
} a:focus {color:#6b96c7;
} body {
    font-family:Helvetica;}")),
      tags$h2("Peek-a-Moo\nDashboard", style = "align:center"),
      tags$img(
        src = "../welcome.png", width = 100
      ),
      tags$p(
        "General visitors please contact the administrator for login in credentials."
      )
    ),
  tags_bottom =
    tags$div(
      tags$style(HTML(".btn-primary {
    color: #ffffff;
    background-color: #6b96c7;
    border-color: #6b96c7;
}")),
      tags$p(
        "For any questions, please contact the ",
        tags$a(
          href = "mailto:animalwelfare@ubc.ca?Subject=Peek-a-Moo%20aDashboard%20aAccess",
          target = "_top", "administrator"
        )
      ),
      tags$div(
        HTML("<center>"),
        tags$img(
          src = "../logo.png",
          width = 250
        ),
        HTML("</center>")
      )
    ),
  background = "center / cover no-repeat url('../cow_pasture.jpg');",
  enable_admin = FALSE,
  download = NULL
)

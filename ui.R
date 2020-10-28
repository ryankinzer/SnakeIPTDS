
# DashboardHeader ----
header <- dashboardHeader(
  title = "Snake IPTDS",
  tags$li(tags$a("DFRM Data App", href = "https://nptfisheries.shinyapps.io/kus-data/",
                 target = '_blank',
                 class='navlinks'),
          class = 'dropdown'),
  tags$li(tags$a("PITtrackR Web App", href = "https://nptfisheries.shinyapps.io/PITtrackR/",
                 target = '_blank',
                 class='navlinks'),
          class = 'dropdown'),
  tags$li(a(href = 'https://nezperce.org/government/fisheries-resources-management/',
            img(src = 'NPTlogos2.png',
                title = 'DFRM Home', height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)

# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
  #useShinyjs(), # Activate ShinyJS
  tags$script(src='javascript.js'), # include Javascript file (for custom spinner functionality)
  sidebarMenu(
    radioButtons('basin_spp', label = 'Species', choices = c('Steelhead', 'Chinook salmon'),
                 selected = 'Steelhead'),
    menuItem("Data Visualization", tabName = 'home', icon = icon("fish")),
    menuItem("Data Table", tabName = 'data', icon = icon("table")),
    div(class = 'busy',
                  img(src="kus_spinner.gif", height= 'auto', width = '100%') # Ty's B.A. custom Spinner
              )
  )
)

# Dashboard Body ----
body <- dashboardBody(
  includeCSS('./www/styles.css'),
  br(), br(), br(),
  tabItems(
    tabItem("home",
            #verbatimTextOutput("spp_text"), # debug option
            iptds_ui("stadem"),
            fluidRow(
              column(12,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                         title = 'Snake Basin Natural-Origin Population and IPTDS Site Escapement',
                         leafletOutput("map", width = '100%', height = 600)))
              )
          ),
    tabItem('data',
            downloadButton('data_export', "Export Data"),
            DT::DTOutput('raw_data'),
            style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
            )
    )
) #dashboardBody

dashboardPage(header, sidebar, body)

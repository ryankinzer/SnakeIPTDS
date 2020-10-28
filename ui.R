# Kus UI

# DashboardHeader ----
header <- dashboardHeader(
  title = "Snake IPTDS",
  tags$li(tags$a("KUS Web App", href = "https://nptfisheries.shinyapps.io/kus-data/",
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
  # div(id = "kustitle", 'IPTDS', style='float:right;'), 
  # tags$li(img(src='NPTlogos2.png', title = NULL, draggable = FALSE, height = '40px'), 
  #       class = 'dropdown', style = 'position: fixed; left:20px; padding-top:6px'),
)

# Dashboard Sidebar ----
sidebar <- dashboardSidebar(
  #useShinyjs(), # Activate ShinyJS
  tags$script(src='javascript.js'), # include Javascript file (for custom spinner functionality)
  sidebarMenu(
    menuItem("Steelhead", tabName = 'steelhead', icon = icon("fish")),
    menuItem("Spring-Summer Chinook", tabName = 'chinook', icon = icon("bar-chart")),
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
  #textOutput("text"),
  tabItems(
    tabItem("steelhead",
            iptds_ui("steelhead")
            ),
    tabItem("chinook",
            iptds_ui("chinook")
            ),
    tabItem('data',
            downloadButton('data_export', "Export Data"),
            DT::DTOutput('raw_data'),
            style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
            )
    )
) #dashboardBody

dashboardPage(title = "Nez Perce Tribe's IPTDS Web Application", header, sidebar, body)#, skin= 'blue')

# Kus Server ----
server <- function(input, output, session) {

  showModal(modalDialog(
    title = "Snake IPTDS Use Agreement",
    tags$div("The Snake River Basin IPTDS web application is currently in development. Please be patient while the organization changes and content is added. The application uses live data obtained from the Nez Perce Tribe Department of Fisheries Resources Management's Centralized Data Managment System (CDMS). More information regarding estimation methods and metadata are available in technical reports for reference at ", tags$a(href="http://kus.nptfisheries.org/kus-data/",
    "http://kus.nptfisheries.org/kus-data/"
  ))
))
  
iptds_server("steelhead", "Steelhead", chn_map)
iptds_server("chinook", "Chinook salmon", sth_map)

  output$raw_data <- DT::renderDataTable({
    #export_dat()
    site_est_spp
  })

  # function for downloading data
  output$data_export <- downloadHandler(  #output name needs to match ui object id name

    filename = function() {
      #paste0(rawdat(),"_", Sys.Date(), "_.csv")
      paste0("iptds_estimates","_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(site_est_spp, filename, row.names = FALSE)
    }
  )
  
} # close Server

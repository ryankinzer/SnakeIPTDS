iptds_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             box(width = NULL, solidHeader = TRUE, status = 'primary',
                 title = 'Lower Granite Dam Total Escapement',
                 plotOutput(ns("stadem_plot"), width = '100%', height = 600))),
      column(6,
             box(width = NULL, solidHeader = TRUE, status = 'primary',
                 title = 'Lower Granite Dam Natural-Origin Escapement',
                 plotOutput(ns("stadem_nat_plot"), width = '100%', height = 600)))
          )
        )
}
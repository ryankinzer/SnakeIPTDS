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
    ),
    fluidRow(
      column(12,
             box(width = NULL, solidHeader = TRUE, status = 'primary',
                 title = 'Snake Basin Natural-Origin Population and IPTDS Site Escapement',
                 leafletOutput(ns("map"), width = '100%', height = 600)))
    )
  )
}
iptds_server <- function(id, species_, map_) {
  
  moduleServer(
    id,
    function(input, output, session) {

      # STADEM Plot  
      output$stadem_plot <- renderPlot({
        
        ns <- session$ns # session provides the namespace
        
        stadem_data %>%
          
          filter(species == species_) %>%
          filter(origin != 'Total') %>%
          ggplot(aes(x = fct_rev(as.factor(spawn_year)), y = estimate,
                     fill = fct_rev(as.factor(origin)))) +
          geom_col(colour = 'black', alpha = .6) +
          scale_fill_viridis_d() +
          scale_y_continuous(labels = scales::comma, 
                             breaks = scales::pretty_breaks()) +
          coord_flip() +
          theme_bw() +
          theme(legend.position = 'bottom',
                axis.title.y = element_text(angle = 0, vjust = .5)) +
          c_theme() +
          labs(x = 'Spawn Year',
               y = 'Estimate',
               fill = 'Origin')
        
      })
      
      
      output$stadem_nat_plot <- renderPlot({
        
        stadem_data %>%
          filter(species == species_) %>% 
          filter(origin == 'Natural') %>%
          ggplot(aes(x = spawn_year, y = as.integer(estimate))) +
          geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = '#440154FF', alpha = .5) +
          geom_line(size = 2) +
          geom_point(colour = 'black', size = 4) +
          scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
          scale_x_continuous(breaks = scales::pretty_breaks()) +
          theme_bw() +
          theme(legend.position = 'bottom',
                axis.title.y = element_text(angle = 0, vjust = .5)) +
          c_theme() + 
          labs(x = 'Spawn Year',
               y = 'Estimate')
        
      })
      
      output$map <- renderLeaflet({
      map_
        })

      return()
    }#function
  ) #moduleServer
}
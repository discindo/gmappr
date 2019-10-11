#' @import shiny

#### Load the data
map_joined_with_data <- readRDS("data-raw/default-map.Rds")
stat_reg <- readRDS("data-raw/stat-reg.Rds")
title_csv <- readr::read_lines("data-raw/sample_data.csv", n_max=1)
example_data <- readr::read_csv("data-raw/sample_data.csv", skip = 1, col_types = "cn", col_names = c("locname", "values")) 


app_server <- function(input, output,session) {
  # List the first level callModules here
  
  what_data <- reactive({
    if (input$demoData == 1) {
      what_data <- map_joined_with_data
    } else {
      what_data <-
        map_joined_with_data %>% select(-values) %>% 
        inner_join(., loadData(), by = "locname")
    }
    return(what_data)
  })
  
  what_title <- reactive({
    if (input$demoData == 1) {
      what_title <- title_csv
    } else {
      what_title <- loadTitle()
    }
    return(what_title)
  })
  
  output$userTitle <- renderText({
    what_title()
  })  
  
  what_to_plot <- reactive({
    level_selection <- input$data
    slider_selection <-input$slider
    
    if (level_selection == "reg") {
      map_to_return <- what_data() %>% 
        group_by(locname_reg) %>% 
        mutate(geometry = st_union(geometry)) %>% 
        summarise_at("values", "sum") %>% 
        tm_shape() +  tm_borders(col = 'black', lwd = 0.3) + 
        tm_fill(col="values", title = "Број", id="locname_reg", n=slider_selection, 
                popup.vars = c("Број"="values"), popup.format = list(big.mark = ".", decimal.mark = ",")) +
        tm_view(set.view = 9,
                set.zoom.limits = c(8, 10), alpha=0.7) +
        tm_basemap("OpenStreetMap")
    }
    
    if (level_selection == "munsk") {
      map_to_return <- what_data() %>% 
        group_by(locname_sk) %>% 
        mutate(geometry = st_union(geometry)) %>% 
        summarise_at("values", "sum") %>% 
        tm_shape() +  tm_borders(col = 'black', lwd = 0.3) + 
        tm_fill(col="values", title = "Број", id="locname_sk", n=slider_selection,
                popup.vars = c("Број"="values"), popup.format = list(big.mark = ".", decimal.mark = ",")) +
        tm_view(set.view = 9,
                set.zoom.limits = c(8, 10), alpha=0.7) +
        tm_basemap("OpenStreetMap") +
        tm_shape(stat_reg) +
        tm_borders(col = 'black', lwd = 0.4)
    }
    
    if (level_selection == "mun") {
      map_to_return <- what_data() %>% 
        tm_shape() +  tm_borders(col = 'black', lwd = 0.3) + 
        tm_fill(col="values", title = "Број", id="locname", n=slider_selection, 
                popup.vars = c("Број"="values"), popup.format = list(big.mark = ".", decimal.mark = ",")) +
        tm_view(set.view = 9,
                set.zoom.limits = c(8, 10), alpha=0.7) +
        tm_basemap("OpenStreetMap") + 
        tm_shape(stat_reg) +
        tm_borders(col = 'black', lwd = 0.4)
    }
    return(map_to_return)
  })
  
  output$mkmap <- renderLeaflet({
    tmap_leaflet(what_to_plot()) %>% 
      addFullscreenControl()
  })
  
  
  output$downloadMap <- downloadHandler(
    filename = "mapa.png",
    content = function(file) {
      tm_style_png <- what_to_plot() + tm_style("gray") +
        tm_layout(frame = FALSE,
                  inner.margins = c(0,0,.05,0),
                  legend.frame = FALSE, 
                  legend.format = list(text.separator = "-", big.mark = ".", decimal.mark = ","),
                  legend.outside=FALSE,
                  legend.title.size=0.7,
                  legend.text.size = 0.5) +
        tm_credits(what_title(), size = 0.85, position=c("LEFT", "TOP"))
      tmap_save(tm_style_png, file, width = 1920, height = 1080, asp=0)
    }
  )
  
  output$downloadData<- downloadHandler(
    filename = "sample_data.csv",
    content = function(file) {
      file.copy("data/sample_data.csv", file)      
    }
  )
  
  
  getFile <- reactive({
    validate(
      need(input$file1 != "" , "Одберете датотека со нови податоци")
    )
    inFile <- input$file1  
    return(inFile)
  })
  
  loadData <- reactive({
    user_data <- read_csv(getFile()$datapath, col_types = "cn", col_names=c("locname", "values"), skip = 1)
    #Verify the user data
    validate(
      need(identical(example_data[['locname']],user_data[['locname']]),
           "Нешто не е во ред со прикачените податоци во колоната општини.")
    )
    return(user_data)
  })
  
  loadTitle <- reactive({
    user_title <- read_lines(getFile()$datapath, n_max=1)
    return(user_title)
  })
  
}

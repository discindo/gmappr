#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import leaflet.extras
#' @import tmap
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    ui <-
      dashboardPage(
        skin = "black",
        title = "Географски истражувач",
        dashboardHeader(title = "мк-гео-мапа"),
        ## Sidebar content
        dashboardSidebar(disable = TRUE,
                         sidebarMenu(
                           menuItem("Мапа", tabName = "map", icon = icon("map"))
                         )),
        ## Body content
        dashboardBody(tabItems(
          # First tab content
          tabItem(
            tabName = "map",
            column(
              width = 3,
              absolutePanel(
                top = 240,
                left = 20,
                width = 300,
                draggable = TRUE,
                style = "opacity: 0.90; z-index: 500;" ,
                wellPanel(
                  box(
                    width = NULL,
                    title = "За",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    tags$p(
                      "Оваа апликација овозможува креирање на мапи на Северна Македонија
                                           со статистички податоци, така што податоците може да се групираат
                                           по општини, општини + градот Скопје, и статистички региони."
                    ),
                    
                    tags$p(
                      "За да добиете Ваша мапа потребно е да имате податоци за сите општини
                                           во Република Северна Македонија. Демо податоците што се вчитани се таков
                                           тип на податоци, и нив може да ги преземете за да можете CSV датотеката да
                                           ја искористите како урнек."
                    ),
                    
                    tags$p(
                      "Во поставките за мапата можете да одберете на кое статистичко ниво
                                           сакате да ги гледате податоците, на пр. општини или региони. Исто така
                                           можете да одберете дали сакате помало или поголемо групирање на податоците.
                                           На крај Вашата мапа можете да ја преземете во PNG формат за понатамошна употреба."
                    )
                  ),
                  box(
                    width = NULL,
                    title = "Податоци",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    radioButtons(
                      inputId = "demoData",
                      label = "Разгледај:",
                      choices = c("Демо податоци" = 1, "Нови податоци" = 2)
                    ),
                    conditionalPanel(
                      condition = "input.demoData == '2'",
                      wellPanel(
                        downloadButton("downloadData", "Download"),
                        helpText(
                          "Преземи ги демо податоците за да ги искористиш како урнек за твои податоци."
                        ),
                        tags$hr(),
                        fileInput(
                          "file1",
                          "Прикачи CSV датотека со нови податоци",
                          multiple = FALSE,
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"
                          )
                        ),
                        tags$p(
                          "Напомена: Новите податоци треба да бидат во ист формат како демо податоците."
                        )
                      )
                    )
                    
                  ),
                  box(
                    width = NULL,
                    title = "Поставки за мапата",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    radioButtons(
                      "data",
                      "Избор на ниво:",
                      c(
                        "Општини" = "mun",
                        "Општини + Скопје" = "munsk",
                        "Региони" = "reg"
                      )
                    ),
                    sliderInput("slider", "Број на групи за боење:", 3, 15, 2)
                  ),
                  
                  box(
                    width = NULL,
                    title = "Преземање",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    downloadButton("downloadMap", "Преземи мапа")
                  )
                )
              )
            ),
            
            absolutePanel(
              top = "auto",
              left = 350,
              width = "auto",
              draggable = FALSE,
              style = "opacity: 0.90; z-index: 500;" ,
              wellPanel(h3(textOutput({
                outputId = "userTitle"
              })))
            ),
            
            #    fluidRow(
            leafletOutput(outputId = "mkmap", height = 800)
            #)
            
          )
        ))
      )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  
  addResourcePath(
    'www', system.file('app/www', package = 'gmappr')
    )
  
  tags$head(
    golem::activate_js(),
    golem::favicon()
            # Add here all the external resources
            # If you have a custom.css in the inst/app/www
            # Or for example, you can add shinyalert::useShinyalert() here
            #tags$link(rel="stylesheet", type="text/css", href="www/custom.css"))
)}

library(shiny)
library(devtools)
library(googledrive)
library(googlesheets)
library(tidyverse)
library(xlsx)
library(DT)
#library(diveRsity)
library(measurements)
#library(hierfstat)

source("functions.R")


folders <- drive_ls("Plant PopGen Project/formatted_datasets")

sheet <- gs_title("Plant PopGen datasets")

qs <- gs_read(sheet, ws="reformatting") %>%
      mutate(rownum = (1:nrow(.)))
qs$component <- paste0(qs$'dataset ID', qs$'component ID')

q <- qs %>% split(1:nrow(.))

ui <- fluidPage(
      
      titlePanel("PopGen dataset validation"),
      
      sidebarLayout(
            sidebarPanel(
                  
                  tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 50%;
               left: 50%;
               width: 20%;
               height: 5%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #97eaed;
               z-index: 105;
             }
          ")),
                  selectInput("component", "Select component IDs:", 
                              qs$component, multiple=T),
                  actionButton("run", "Click to validate"),
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$div("... processing datasets ...",id="loadmessage")),
                  
                  width=3
            ),
            
            mainPanel(
                  h5(textOutput("txt")),
                  br(),
                  DTOutput("results")
            )
      )
)

server <- function(input, output) {
      
      msg <- reactiveVal()
      tbl <- reactiveVal()
      
      observeEvent(input$run,{
            
            qi <- q[qs$rownum[qs$component %in% input$component]]
            
            v <- lapply(qi, validate, folders=folders, sheet=sheet)
            
            qi <- do.call("rbind", qi)
            #browser()
            tr <- v %>% 
                  lapply(function(x) c(x, rep("", 7))[1:7]) %>%
                  do.call("rbind", .) %>%
                  as.data.frame() %>%
                  cbind(component=qi$component, .)
            
            tbl(tr)
            msg("See test results below; these have also been posted to the Google spreadsheet.")
      })
      
      output$txt <- renderText({ msg() })
      
      output$results <- renderDT({ tbl() })
}

shinyApp(ui = ui, server = server)

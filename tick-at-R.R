# tickatlab analyzR
#tick@R

tl2 %>%
  ggplot(aes(x= Alter2, fill = ParticipatesInActiveMating))+
  geom_bar()+
  facet_wrap(Zuchtlinie~.)

tl2 %>%
  ggplot(aes(x= Alter2, fill = ParticipatesInActiveMating))+
  geom_bar()+
  facet_grid(Raum~Zuchtlinie)


## display as a table of numbers the genotypes

library(shiny)
library(tidyverse)
library(readxl)
library(DT)

## change imm-obst filter

ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 3,
               fileInput("files", "Choose Exported Tick@Lab File (.xls)", accept = ".xls", multiple = F),
               
               hr(),
               
               checkboxGroupInput("line",choices = character(0),label = "Zuchtlinie")
               
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Past Experiments", 
                      
                         plotOutput("plotzl")
                        
                         
                ),
                tabPanel("Future Experiments", 
                         verbatimTextOutput("summary")
                )
    )
    
    
  )
  
  
)

server <- function(input, output, session) {
  
  tltable <- reactiveVal()
  
  tltable <- eventReactive(input$files,{
    
    print(input$files)
    tl <- read_excel(path = input$files[1,4])
    
    tl <- tl %>% 
      replace(is.na(.), "NA") %>%
      filter(Team == "Immunologie-Obst") %>% ## filter for team
      select(ParticipatesInActiveMating,
             IsLitter,
             IsWeaned,
             IsSick,
             HasTask,
             NotesExist,
             IsPreviouslyUsed,
             Tags,
             `Käfig-ID`,
             `Auto-ID`,
             G,
             `Tier-ID1`,
             Geb.,
             Anzahl,
             Zuchtlinie,
             Genotyp,
             Stamm,
             Alter,
             Raum,
             Team,
             `Verantwortliche Person`,
             Aktenzeichen,
             Status,
      )
    
    
    tl <- tl %>%
      mutate(Alter2 = case_when(
        str_detect(Alter, "\\d(?=y)") & str_detect(Alter, "\\d(?=m)") ~ as.integer(12) + as.integer(str_extract(Alter, "\\d+(?=m)")),
        str_detect(Alter, "\\d(?=y)") ~ as.integer(12),
        str_detect(Alter, "\\d(?=m)") ~ as.integer(str_extract(Alter, "\\d+(?=m)")),
        str_detect(Alter, "\\d(?=d)") ~ as.integer(0)
      ))
    
    return(tl)
    
  })
  
  observeEvent(tltable(),{

    updateCheckboxGroupInput(inputId = "line", choices = sort(unique(tltable()$Zuchtlinie)) )
    
  })
  
  output$plotzl <- renderPlot({
    
    tltable() %>%
      filter(Zuchtlinie %in% input$line) %>%
      ggplot(aes(x= Alter2, fill = Genotyp))+
      geom_bar()+
      facet_wrap(Zuchtlinie~.)+
      scale_x_continuous(breaks = 0:max(tltable()$Alter2) )+
      theme(legend.position = "top")
    
  })

}


shinyApp(ui, server)
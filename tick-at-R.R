# tickatlab analyzR
#tick@R

#tl2 %>%
#  ggplot(aes(x= Alter2, fill = ParticipatesInActiveMating))+
#  geom_bar()+
#  facet_wrap(Zuchtlinie~.)
#
#tl2 %>%
#  ggplot(aes(x= Alter2, fill = ParticipatesInActiveMating))+
#  geom_bar()+
#  facet_grid(Raum~Zuchtlinie)


## display as a table of numbers the genotypes

library(shiny)
library(tidyverse)
library(readxl)
library(DT)

testv <- c()
## change imm-obst filter

ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 2,
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
                         fluidRow(
                           column(2,
                                  radioButtons("gt_select",choices = "",label = "Zuchtlinie")
                                  ),
                           column(2,
                                  uiOutput("comparison"),
                                  actionButton("submitgt",label = "Update")
                           ),
                           column(6,
                                  dataTableOutput("gt_table")
                                  )
                         )
                         
                         
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
    
    updateRadioButtons(inputId = "gt_select", choices = sort(unique(tltable()$Zuchtlinie)))
    
  })
  
  rb <- reactiveValues()
  
  observeEvent(input$gt_select,{
    vect <- tltable() %>%
      filter(Zuchtlinie %in% input$gt_select) %>%
      distinct(Genotyp) %>%
      pull()
    
    possible_genotypes <- paste(vect,collapse = "\r\n")
    
    gsplit <- str_split(possible_genotypes,pattern = "\\r\\n", simplify = T)
    gsplit2 <- as.vector(gsplit)
    
    
    gt <- sapply(gsplit2, function(x)str_extract(x, pattern = "(?<=: ).+"))
    
    names <- sapply(gsplit2, function(x)str_extract(x,pattern = "^.*:"))
    
    tlist <- tibble(names, gt) %>%
      group_by(names, gt) %>%
      count() %>%
      group_by(names) %>%
      group_split()
    
    length(tlist)
    
    tlist_names <- sapply(1:length(tlist), function(x)tlist[[x]]$names[1])
    
    tlist_gt <- lapply(1:length(tlist), function(x)tlist[[x]]$gt)
    
    rb$namesn <- length(unique(names))
    rb$tlistn <- tlist_names
    rb$tlistgt <- tlist_gt
    
   # insertUI(
   #   selector = '#gt_names',
   #   where = "beforeEnd",
   #   ui = tagList(
   #     actionButton("updateMeta",class="btn btn-info", 
   #                  label = tags$em("Update Table")),
   #     selectInput('preset', 'Presets', c(Choose='', names(presetList)), selectize=TRUE),
   #     textInput(inputId = "varName", 
   #               label = tags$em("Variable Name")))
   # )
    
    #https://www.r-bloggers.com/2019/09/dynamic-ui-elements-in-shiny/
    #  https://stackoverflow.com/questions/46969448/creating-radio-buttons-from-a-loop-and-select-all-in-r-shiny
    #

  })
  
  output$comparison <- renderUI({
    
    myTabs <- lapply(1:rb$namesn, function(i) {
      
      checkboxGroupInput(inputId = paste0("dw",i),label = rb$tlistn[i] ,choices =   
                     rb$tlistgt[[i]]
      )
    })
    
    
    # do.call(tabsetPanel, myTabs)
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

  test <- reactiveVal()
  dws <- reactiveVal()
  
  observeEvent(input$submitgt,{
    
    x <- sapply(1:rb$namesn, function(i)paste0("dw", i))

    
    for( i in 1:rb$namesn){
      
     if(is.null(input[[x[i]]])){
       next
     } else {
       testv <- c(testv, paste0(rb$tlistn[i], " ",input[[x[i]]]))
     }}
      
    testv <- paste0(testv, collapse = "|")
    print(testv)
    
    tt <- tltable() %>%
      filter(Zuchtlinie == input$gt_select) %>%
      filter(str_detect(Genotyp, testv)) 
    
    test(tt)
    
  })
  
  output$gt_table<- renderDataTable({
    
    if(!is.null(test()) ){
      
      test()
      
    }
    
  },options = list(pageLength = 100))
  
  
}


shinyApp(ui, server)
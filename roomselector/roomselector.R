library(shiny)
library(tidyverse)
library(DT)
library(pivottabler)

# create dataframe listing rooms in hotel + features
hotel_data <- data.frame(
  Floor = rep(seq(1:5), times = 100/5),
  rmpoolno = sample(1:4,  size = 100, replace = TRUE), 
  View = sample(c("Ocean", "Mountain", "Carpark"), size = 100, replace = TRUE), 
  Steps = sample(c("Stepfree", "Stairs", "Steps"), size = 100, replace = TRUE)
  )

# rowwise() to apply mutate per row
hotel_data <- hotel_data %>%
  mutate(Number = paste0(Floor, sprintf("%02d",rep(00:49)))) %>%
  mutate(Category = case_when(rmpoolno == 1 ~ "Standard",
                           rmpoolno == 2 ~ "Premium",
                           rmpoolno == 3 ~ "Executive",
                           TRUE ~ "Luxury")) %>%
  rowwise() %>%
  mutate(Size = case_when(Category == "Standard" ~ sample(30:50, 1),
                          Category == "Premium" ~ sample(50:70, 1),
                          Category == "Executive" ~ sample(70:90, 1),
                          TRUE ~ sample(90:120, 1))) %>%
  mutate(Balcony = case_when(Category %in% c("Executive", "Luxury") ~ sample(c("Small", "Large", NA_character_), 1))) %>%
  mutate(Connecting = case_when(Category %in% c("Standard", "Premium") ~ sample(c("Yes", NA_character_), 1, prob = c(0.3, 0.7)))) %>%
  mutate(Extra_bed = case_when(Category != "Standard" ~ sample(c(1, 2, NA_character_), 1, prob = c(0.7, 0.2, 0.1)))) %>%
  ungroup() %>%
  arrange(rmpoolno, Number) %>%
  select(-rmpoolno)

hotel_data <- hotel_data[,c(1, 4, 5, 2, 3, 6:9)]
hotel_data$Category <- factor(hotel_data$Category, levels = unique(hotel_data$Category))
  

#### UI####
ui <- fluidPage(
  tags$head(HTML("<title>Room Selector</title>")),
  titlePanel(""),
  tabsetPanel(               
    tabPanel("Choose the perfect room", 
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 h4("Choose room features"),
                 hr(),
                 selectizeInput('Category', 'Category',
                                c(levels(hotel_data$Category)), 
                                multiple=TRUE, 
                                 options = list(
                                  placeholder = 'Start typing',
                                  onInitialize = I('function() {this.setValue(""); }'))
                 ),
                 selectizeInput("View", "View", 
                                choices = list(
                                  Nature = list("All nature", "Ocean", "Mountain"),
                                  Carpark = list("Carpark")), 
                                multiple = TRUE, 
                                options = list(placeholder = 'Choose view(s)')),
                 
                 checkboxInput("Balcony", "Must have balcony", 
                               value = FALSE),
                 checkboxInput("Connecting", "Must have connecting", 
                               value = FALSE),
                 checkboxInput("Steps", "Must be stepfree", 
                               value = FALSE),
                 checkboxInput("Extra_bed", "Can fit extra bed", 
                               value = FALSE),     
                 sliderInput("Size", "Size:",
                             min = 30, max = 120,
                             value = c(40,120))
               ),
               mainPanel(
                 h1(""),
                 # textOutput("sel_rmpool"),
                 # textOutput("sel_size"),
                 # textOutput("sel_view"),
                 tags$style("#num_rows {font-size:20px;
               display:block; }"),
                 textOutput("num_rows"),
                 dataTableOutput("table")
                 )
               )
             ),
    tabPanel("Rooms summary",
             sidebarLayout(
               sidebarPanel(width = 3,
                            selectInput("selectCols1", "Select for column",
                                        choices = list("Category" , "Floor")
                            ),
                            selectizeInput("selectCols2", "Select second for column",
                                           choices = list("None" ,"Category" , "Floor")
                            ),
                            
                            
                            selectInput("selectRows", "Select for row", 
                                        choices = list("None","View", "Balcony","Room pool" = "Category")
                            ),
                            selectInput("selectMeasure1", "Select a measure",
                                        choices = list("No. rooms","Avg. room size")
                            )
               ),
               
               mainPanel(
                 h4(""),
                 pivottablerOutput('pvt')
               )
             )
    )
  )
  
)

#### server ####
server <- function(input, output) {
 

  output$num_rows <- renderText({
    paste("No. possible rooms:", nrow((filtered_data())))
  })
  
  filtered_data <- reactive({
    filtered <- hotel_data
    
    # filter 1 = category
    if (!is.null(input$Category)) {
      filtered <- filtered %>% filter(.data$Category == input$Category)
    }
    
    #filter 2 = size
    if (!is.null(input$Size)) {
      filtered <- filtered %>% filter(.data$Size >= input$Size[1], .data$Size <= input$Size[2])
    }
    
    #filter 3 = iew
    if (!is.null(input$View)) {
      if ("All nature" %in% input$View) {
        filtered <- filtered %>% filter(.data$View %in% c("Ocean", "Mountain"))
      } else {
        filtered <- filtered %>% filter(.data$View %in% input$View)
      }
    }
    
    #filter 4 = balcony
    if (input$Balcony) {
      filtered <- filtered %>% filter(.data$Balcony != "")
    }
    
    # filter 5 = connecting 
    if (input$Connecting) {
      filtered <- filtered %>% filter(.data$Connecting == "Yes")
    }   
    
    # filter 6 = steps 
    if (input$Steps) {
      filtered <- filtered %>% filter(.data$Steps == "Stepfree")
    }   
    
    # filter 7 = extra bed
    if (input$Extra_bed) {
      filtered <- filtered %>% filter(.data$Extra_bed != "")
    }   
    
    filtered <- filtered
    
    })
  
  # render datatable based on filtered dataset
  output$table <- renderDataTable({
    datatable(filtered_data(),
              rownames = FALSE,
              options = list(paging = FALSE,
                             dom = 'ft',
                             columnDefs = list(list(className = 'dt-head-left', targets = '_all')),
                             language = list(
                               search = "<i class='glyphicon glyphicon-search'></i>",
                               zeroRecords = "Please build your own hotel")
                             )
              )
  })
  
  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(hotel_data)
    
    # rows and columns
    if (input$selectCols1 != "None") { pt$addColumnDataGroups(input$selectCols1) }
    if (input$selectCols2 != "None") { pt$addColumnDataGroups(input$selectCols2) }
    if (input$selectRows != "None") { pt$addRowDataGroups(input$selectRows) }
    
    # measure
    if (input$selectMeasure1 == "No. rooms") {
      pt$defineCalculation(calculationName = "No. rooms",
                           summariseExpression = "n()",
                           caption = "No. rooms")
    }
    else if (input$selectMeasure1 == "Avg. room size") {
      pt$defineCalculation(calculationName = "Avg. room size",
                           summariseExpression = "mean(Size)",
                           format = "%.2f",
                           caption = "Avg")
    }
    
    pt$evaluatePivot()
    pivottabler(pt)
    
    #theme
    simpleBlueTheme <- list(
      fontName="Heveltica, Arial",
      fontSize="1em",
      headerBackgroundColor = "rgb(242, 242, 242)",
      headerColor = "rgb(80, 80, 80)", #text
      cellBackgroundColor = "rgb(255, 255, 255)",
      cellColor = "rgb(80, 80, 80)",
      outlineCellBackgroundColor = "rgb(204, 204, 204)",
      outlineCellColor = "rgb(0, 0, 0)",
      totalBackgroundColor = "rgb(242, 242, 242)",
      totalColor = "rgb(80, 80, 80)",
      borderColor = "rgb(204, 204, 204)"
    )
    pt$theme <- simpleBlueTheme
    pt$renderPivot(styleNamePrefix="t3")
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)

# refs
# http://pivottabler.org.uk
# https://github.com/wleepang/DesktopDeployR deploy as desktop app (windows)

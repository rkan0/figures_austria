library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(scales)


# https://www.data.gv.at/katalog/dataset/d0d994e4-b6e9-4f31-b594-4bea907e4c6a#resources

res = GET("https://www.data.gv.at/katalog/api/3/action/package_show?id=d0d994e4-b6e9-4f31-b594-4bea907e4c6a")
metadata = fromJSON(rawToChar(res$content))
urllist <- metadata[["result"]][["resources"]][["url"]]
# neue Gliederung in 16 Statistische Bezirke
urllist <- str_subset(urllist, "V2014")
# request csv; add new column with year
df <- do.call(rbind, lapply(urllist, function(x)
  cbind(read.csv(x, sep = ";", fileEncoding = "latin1"),
        year = str_sub(x, start = -8, end = -5))
))
data<-df

data <- data %>%
  pivot_longer(c(3:11), names_to = "type", values_to = "number")
hover_info_label <- c("1 Wohnraum", "2 Wohnräumen", "3 Wohnräumen","4 Wohnräumen", "5 Wohnräumen", "6 Wohnräumen","7 Wohnräumen", "mehr als 7 Wohnräumen", "unbekannt")
bar_label <- c("1", "2 ", "3 ","4", "5 ", "6 ","7 ", "mehr als 7 ", "unbekannt")


round_any = function(x, nearest, f = round){f(
  x/ nearest) * nearest
} # from {plyr}


#### UI####
ui <- fluidPage(
  headerPanel(
    fluidRow("Anzahl der Wohnräume in Linzer Wohnungen (2000-2022)")
  ),
  titlePanel("Datenquelle: Stadt Linz; Veröffentlicht durch: data.gv.at"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "num", 
                  label = "Jahr wählen", 
                  choices = 2000:2022,
                  selected = 2000, 
                  multiple = FALSE),
      selectInput(inputId = "bezirknum", 
                  label = "Bezirk wählen", 
                  choices = 1:16,
                  selected = 1, 
                  multiple = FALSE),
      HTML("<hr>"),
      textOutput("table_header"),
      dataTableOutput("table")
    ),
    
    mainPanel(
      htmlOutput("hover_info"),
      HTML("<br>"),
      plotOutput("plot", hover = hoverOpts(id ="plot_hover"))

    )
  )
)

#### server ####

server <- function(input, output) {

  output$selected_num <- renderText({ 
    paste("You have selected this year:", input$num)
  })
  
  
  data_subset <- reactive({
    data %>%
      filter(.data$year == .env$input$num) %>%
      filter(.data$StatBezirkNr == .env$input$bezirknum)%>%
      dplyr::mutate(type = recode(type,  "Wohnungen.mit.einem.Wohnraum" = "Wohnungen.mit.1.Wohnraum"))
  })
  
  y_limit <- reactive({
    round_any(max(data_subset()$number), 5000, f = ceiling)
  }) 
  
  output$plot <- renderPlot({
    ggplot(data = data_subset(), aes(x = type, y = number))+
      geom_bar(stat = "identity") +
      scale_x_discrete(labels=bar_label,
                       name = "Wohnräume")+
      scale_y_continuous(limits = c(0, y_limit()),
                         breaks = seq(0,y_limit(), 500),
                         expand = c(0, 0),
                         labels = label_number(big.mark = "."),
                         name = "Anzahl Wohnungen") +
      theme_classic() +
      theme(axis.title.y = element_text(size = 13, margin = unit(c(0, 5, 0, 0), "mm")),
            axis.title.x = element_text(size = 13),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10)
      )
  })
  
  
  output$hover_info<- renderText({
    if (is.null(input$plot_hover$x)) return("Wenn Sie den Mauszeigen über einen Balken bewegen, wird der Wert angezeigt.")
    else {
      name <- hover_info_label[round(input$plot_hover$x)]
      count <- round(input$plot_hover$x)
      sum <- data_subset()$number[round(input$plot_hover$x)]
      if(round(input$plot_hover$x) == 9) 
        HTML("In <code>", input$num, "</code> ",
             "gab es im <code>", input$bezirknum, "</code>. ",
             "Bezirk <code>", sum, "</code> ",
             "Wohnungen mit unbekannter Anzahl an Wohnräumen.")
      else {
        HTML("In <code>", input$num, "</code> ",
             "gab es im <code>", input$bezirknum, "</code>. ",
             "Bezirk <code>", sum, "</code> ",
             "Wohnungen mit <code>", name, "</code>.")}
    }
  })
  

  output$table_header <- renderText({ 
    HTML("Alle Bezirke in ", input$num)
  })
  
  data_fortable <- reactive({
    data %>%
      filter(.data$year == .env$input$num) %>%
      group_by(StatBezirkNr) %>%  
      dplyr::summarise(Wohnungen = sum(number)) %>%
      bind_cols(Bezirk = unique(data$Statistischer.Bezirk)) %>%
      dplyr:: select(StatBezirkNr, Bezirk, Wohnungen) %>%
      dplyr::rename(Nummer = StatBezirkNr)
  })
  
  output$table <- renderDataTable(
    data_fortable(),
    options = list(dom = "t", 
                   searching = FALSE,
                   columnDefs = list(list(className = 'dt-head-left', targets = '_all')) #not sure why cant align
    )
  )
  
}

shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(httr)
library(XML)
library(readxl)
library(sqldf)
library(readr)
#install.packages("tidygraph")
library(tidygraph)
#install.packages("ggraph")
library(ggraph)
#install.packages("igraph")
library(igraph)
 


dirname <- '~/Desktop/Rshiny/WOSU_2019_Donor/Other Metrics'
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

#Files
Gift_Final_03 <- read_excel("Gift_Final_03.xlsx")
`%notin%` <- Negate(`%in%`)

#static Content
 


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("First time Donors - Network Visualization - Most popular Gift Source"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        sliderInput(inputId = "W1", label = "Donation greater than $", min = 0, max = 10000, value = 100),
        sliderInput(inputId = "W2", label = "Donation less than $", min = 0, max = 10000, value = 350),
        sliderInput(inputId = "W3", label = "Gift Year", min = 2000, max = 2019, value = 2019), 
                    actionButton(inputId = "go", label = "Click Update Year")
        ),
      
      # Show plot of the generated distribution
      mainPanel(
         htmlOutput("yearText"),
         plotOutput("NWPlot"),
         dataTableOutput("NWtable")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
 
  cap <- eventReactive(input$go, {paste("Gift Year is","<font color=\"#FF0000\"><b>", input$W3, "</b></font>")})
  output$yearText <- renderText({cap()})
  dn1 <- eventReactive(input$go, { input$W1 })
  dn2 <- eventReactive(input$go, { input$W2 })
  
  
  d <-  eventReactive(input$go, { input$W3 })
  a <- eventReactive(input$go, { paste("Select * from Gift_Final_03  where Gift_Year=",d(),  sep="") })
  tableNW <- eventReactive(input$go, {paste("select Account_id, Original_gift_source, city, State, Original_Gift_amount 
              from Gift_Final_03 where original_gift_amount >=", dn1(), "and original_gift_amount <", dn2(),  
                                            "and Gift_Year =", d(), 
              "group by Original_gift_source, city order by Original_gift_source, city")
  })
  
  
  
  wq <- eventReactive(input$go, {sqldf(a()) })
  wq1 <- reactive({sqldf(tableNW()) })
  
  
 donation_greater_than <- eventReactive(input$go, {input$W1})
 donation_less_than <- eventReactive(input$go, {input$W2})
  
  
   output$NWPlot <- renderPlot({
    
       nodes <- wq() %>% filter(Original_Gift_Amount >= donation_greater_than() & Original_Gift_Amount < donation_less_than()) %>% 
         select(Original_Gift_Source) 
       nodes_01 <- nodes %>%   mutate(type = "Gift_Source")
       colnames(nodes_01) <- c("name", "type")
       nodes_02<- unique(nodes_01)
       
       nodes_city <- wq() %>% filter(Original_Gift_Amount >= donation_greater_than() & Original_Gift_Amount < donation_less_than()) %>% select(City) 
       nodes_city_01 <- nodes_city %>% mutate(type = "City")
       nodes_city_02 <- unique(nodes_city_01)
       colnames(nodes_city_02) <- c("name", "type")
       nodes_00_01 <- rbind(nodes_02,nodes_city_02 )
       nodes_00 <- unique(nodes_00_01)
       nodes <- nodes_00
       
       edges <- wq() %>% filter(Original_Gift_Amount >= donation_greater_than() & Original_Gift_Amount < donation_less_than()) %>% 
         select(Original_Gift_Source, City, Original_Gift_Amount)
       colnames(edges) <- c("from", "to", "weight")
       edges_01 <- unique(edges)
       edges_01$from <- as.character(edges_01$from)
       edges_01$to <- as.character(edges_01$to)
       
       edges <- edges_01
       
       
       g <- graph_from_data_frame(edges, vertices=nodes, directed=F)
       
       g1 <- as_tbl_graph(g)
       as_tibble(g1 %>% activate(edges))
       as_tbl_graph(g1) %>%
         mutate(betweenness=centrality_betweenness(normalized=T)) %>% ggraph() + geom_edge_link(alpha=.1) +
         geom_node_point(aes(size=betweenness, color=betweenness)) + 
         scale_color_continuous(guide = 'legend') + 
         geom_node_text(aes(label = name, size = .1))  
       
       
   })
  
   # Print data table
   output$NWtable = renderDataTable(
     wq1(),
     options = list(
       pageLength = 50)
       
   )
   
   
   
    
}


# Run the application 
shinyApp(ui = ui, server = server)


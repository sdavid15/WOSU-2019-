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
library(ggmap)
library(httr)
library(XML)
citation("ggmap")
library(geosphere)
library(readxl)
#install.packages("maps")
library(maps)
library(mapproj)
library(sqldf)

dirname <- '~/Desktop/Rshiny/WOSU_2019_Donor'
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

#Files
Gift_Final_02 <- read_excel("Gift_Final_02.xlsx")
# Get US  states abbreviations 
urlCanadaStates <- "https://www.ncbi.nlm.nih.gov/books/NBK7254/"
htmlpageCanada <- GET(urlCanadaStates )
CanadaTabs <-readHTMLTable(rawToChar(htmlpageCanada$content), stringsAsFactors=F)
USTabs1 <- CanadaTabs[[2]]
colnames(USTabs1) <- c("State1", "State")
as_tibble(USTabs1)
usa <- map_data("usa")
state <- map_data("state")
as_tibble(usa)
as_tibble(state)  
stll <- read_excel("stll.xlsx")    
stlab <- USTabs1 %>% right_join(stll, by="State")
stlab_01 <- stlab %>% filter( !is.na(Latitude))
all_states <- map_data("state") 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("First time Donors - US Demography"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        sliderInput(inputId = "W1", label = "Donor Count greater than", min = 0, max = 1000, value = 14),
        sliderInput(inputId = "W2", label = "Donor Count less than", min = 0, max = 1000, value = 100),
        sliderInput(inputId = "W3", label = "Gift Year", min = 2000, max = 2019, value = 2019), 
                    actionButton(inputId = "go", label = "Click Update Year")
        ),
      
      # Show plot of the generated distribution
      mainPanel(
         htmlOutput("yearText"),
         htmlOutput("yearText1"),
         plotOutput("countPlot"),
         plotOutput("retentionPlot"),
         plotOutput("genderPlot"),
         plotOutput("geoPlot"),
         plotOutput("outsideOHPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output$yearText <- eventReactive(input$go, {renderText({paste("Gift Year is","<font color=\"#FF0000\"><b>", input$W3, "</b></font>")})
 # })
  cap <- eventReactive(input$go, {paste("Gift Year is","<font color=\"#FF0000\"><b>", input$W3, "</b></font>")})
  output$yearText <- renderText({cap()})
  
  cap1 <- eventReactive(input$go, {paste("Note: Donor Count Slide bar is only for Fig 1.")})
  output$yearText1 <- renderText({cap1()})
  
  
  d <-  eventReactive(input$go, { input$W3 })
  a <- eventReactive(input$go, { paste("Select * from Gift_Final_02  where Gift_Year=",d(),  sep="") })
  #output$que <- renderPrint({ a() }) 
  wq <- eventReactive(input$go, {sqldf(a()) })
  
 
  
  
   output$countPlot <- renderPlot({
     wq() %>%
       dplyr::group_by(CitySate) %>%
       dplyr::summarise(n = n()) %>% 
       filter(n  >= input$W1 & n <= input$W2) %>%
       ggplot(aes(x = reorder(as.factor(CitySate), n), y = n)) + 
       geom_bar(stat = 'identity', fill = "coral1", width = 0.3) +  
       ggtitle("Fig. 1: Donor counts in Cities") +
       geom_text(aes(label= n), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + 
       ylab("Donor Counts") + xlab("City-State") +
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) ) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
   })
   
   output$genderPlot <- renderPlot({
     #wq()$gender <- fct_infreq( wq()$gender)
     wq() %>%   
       ggplot(aes(x = gender, y= (..count..)/(sum(..count..))))  +
       geom_bar(stat="count", fill = "coral1", width = 0.3) + 
       geom_text(aes(label = scales::percent((..count..)/(sum(..count..))), y =(..count..)/(sum(..count..))), stat = "count", vjust = -.25)  +
       ggtitle("Overall Yearly Gender Distribution  percentage  ") + labs( x= "Gender ", y= "Percent") +
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) ) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
     
     
   })
   
   
   output$retentionPlot <- renderPlot ({
     #Gift_19_19$Account_Status<- fct_infreq( Gift_19_19$Account_Status)
     wq() %>%   
       ggplot(aes(x = Account_Status, y= (..count..)/(sum(..count..))))  +
       geom_bar(stat="count", fill = "coral1", width = 0.3) + 
       geom_text(aes(label = scales::percent((..count..)/(sum(..count..))), y =(..count..)/(sum(..count..))), stat = "count", vjust = -.25)  + ggtitle("Overall Yearly Retention rate  ") + 
       labs( x= "Account Status", y= "Percent") +
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) ) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
     
     
   })
    
   
    
   
   output$geoPlot <- renderPlot({
       
     bbscnt <- wq() %>% group_by(State) %>% dplyr::summarize(n=n() ) %>% ungroup()
     as_tibble(bbscnt)
     stabv <- USTabs1 %>% left_join(bbscnt, by= "State") 
     stabv$State1 <- tolower(stabv$State1)
     colnames(stabv) <- c("region", "State", "n")
     which(stabv$State == "OH")
     
     
     stabv_01 <- stabv %>% filter(!is.na(n))
     stabv_02 <-  state  %>% left_join(stabv_01, by = "region")
     
     all_states <- map_data("state") 
     # Add more states to the lists if you want
     states_positive  <-c("ohio")
     
     stabv_02 %>% ggplot() + 
       geom_polygon(aes(x=long, y=lat, group=group, fill = n ), color = "white") +
        scale_fill_gradientn(colors=c("#2c7bb6","#abd9e9","#ffffbf","#fdae61","#d7191c")) +
       coord_map() +theme_void() + ggtitle("Overall Yearly Spread of donors-All States")  +
       geom_text(data=stlab_01, mapping=aes(Longitude, Latitude, label=str_c(State)), size=2) 
       
       
     
   })  
     
   
     output$outsideOHPlot <- renderPlot({
       
       bbscnt <- wq() %>% group_by(State) %>% dplyr::summarize(n=n() ) %>% ungroup()
       as_tibble(bbscnt)
       stabv <- USTabs1 %>% left_join(bbscnt, by= "State") 
       stabv$State1 <- tolower(stabv$State1)
       colnames(stabv) <- c("region", "State", "n")
       which(stabv$State == "OH")
       
       
       stabv_01 <- stabv %>% filter(!is.na(n))
       stabv_02 <-  state  %>% left_join(stabv_01, by = "region")
       
       all_states <- map_data("state") 
       # Add more states to the lists if you want
       states_positive  <-c("ohio")
       stabv[41, 3] <- 0
       stabv_01 <- stabv %>% filter(!is.na(n))
       stabv_02 <-  state  %>% left_join(stabv_01, by = "region")
       
       
       stabv_02 %>% ggplot() + 
         geom_polygon(aes(x=long, y=lat, group=group, fill=n), color = "white") +
         scale_fill_gradientn(colors=c("#2c7bb6","#abd9e9","#ffffbf","#fdae61","#d7191c")) +
         coord_map() +theme_void() + ggtitle("Overall Yearly Spread of donors-Excluding Ohio")  +
         geom_text(data=stlab_01, mapping=aes(Longitude, Latitude, label=str_c(State)), size=2)
       
     ####
     
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


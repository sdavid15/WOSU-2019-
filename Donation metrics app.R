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
 
 

dirname <- '~/Desktop/Rshiny/WOSU_2019_Donor/Other Metrics/Donation Metrics'
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

#Files
Gift_Final_03 <- read_excel("Gift_Final_03.xlsx")
Gorg <- read_excel("Gorg.xlsx")
Gift_Final_03$Original_Gift_Source <- fct_infreq(Gift_Final_03$Original_Gift_Source)
Gift_Final_03$Solicitation_Method<- fct_infreq( Gift_Final_03$Solicitation_Method)
`%notin%` <- Negate(`%in%`)

 
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("First time Donors - Donation Metrics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       sliderInput(inputId = "W1", label = "Gift Year From", min = 2000, max = 2019, value = 2000), 
        sliderInput(inputId = "W2", label = "Gift Year To ", min = 2000, max = 2019, value = 2019), 
                    actionButton(inputId = "go", label = "Click Update Year")
        ),
      
      # Show plot of the generated distribution
      mainPanel(
         htmlOutput("yearText"),
         plotOutput("DonorPlot"),
         plotOutput("CostPlot"),
         plotOutput("SolMthPlot"),
         plotOutput("GiftSrcPlot"),
         plotOutput("OrgPlot"),
         htmlOutput("OrgText"),
         dataTableOutput("Orgtable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
 
  cap <- eventReactive(input$go, {paste("Gift Year from","<font color=\"#FF0000\"><b>", input$W1, "</b></font>", "to","<font color=\"#FF0000\"><b>", input$W2, "</b></font>")})
  output$yearText <- renderText({cap()})
  
  dn1 <- eventReactive(input$go, { input$W1 })
  dn2 <- eventReactive(input$go, { input$W2 })
  
  a  <- eventReactive(input$go, {paste("select * from Gift_Final_03 where 
                                       Gift_Year >=", dn1(), "and Gift_Year <=", dn2())
  })
  
    b  <- eventReactive(input$go, {paste("select * from Gorg where 
                                       Gift_Year >=", dn1(), "and Gift_Year <=", dn2())

    })
    
    tableOrg <- eventReactive(input$go, {paste("select Account_id, Original_gift_source, CityState, Original_Gift_amount 
              from Gorg where Gift_Year >=", dn1(), "and Gift_Year <=", dn2(), 
                                              "group by Original_gift_source, CityState order by Original_gift_source, CityState")
    })
    
    
  wq <- eventReactive(input$go, {sqldf(a()) })
  og <- eventReactive(input$go, {sqldf(b()) })
  wq1 <- reactive({sqldf(tableOrg()) })
  
  output$DonorPlot <- renderPlot({
    
    wq() %>%   
      ggplot(aes(x = Gift_Year, y= (..count..)/(sum(..count..))))  +
      geom_bar(stat="count", fill = "coral1") + 
      geom_text(aes(label = scales::percent((..count..)/(sum(..count..))), y =(..count..)/(sum(..count..))), stat = "count", vjust = -.5, size = 3)  + 
      ggtitle(" Donors Percentage by Year  ") + labs( x= "Gift Year ", y= "Percent") +theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) ) +
      theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
    
  })
  
   
   output$CostPlot <- renderPlot({
    
       wq() %>%  
      ggplot(aes(x = Gift_Year, colour = Orig_Gift_Has_Prm)) + 
       geom_density(alpha = 1) + ggtitle("Donors-Original Gift has Premium by Years") + 
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1)) + 
       scale_color_manual(values=c("#F8766D", "#39B600" )) + guides(colour = guide_legend(override.aes = list(size=3, alpha = 1))) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
   })
  
   output$SolMthPlot <- renderPlot({
   
   wq() %>%   
     ggplot(aes(x = Solicitation_Method, y= (..count..)/(sum(..count..))))  +
     geom_bar(stat="count", fill = "coral1") + 
     geom_text(aes(label = scales::percent((..count..)/(sum(..count..))), y =(..count..)/(sum(..count..))), stat = "count", vjust = -.25)  + 
       ggtitle("Donors - Solicitation Method used") + labs( x= "Solicitation Method", y= "Percent") +
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) )+
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
   
   })
   
   
   
   output$GiftSrcPlot <- renderPlot({
     wq() %>%   
       ggplot(aes(x = Original_Gift_Source, y= (..count..)/(sum(..count..))))  +
       geom_bar(stat="count", fill = "coral1") + 
       geom_text(aes(label = scales::percent((..count..)/(sum(..count..))), y =(..count..)/(sum(..count..))), stat = "count", vjust = -.25)  + 
       ggtitle("Donors - Original Gift Source") + labs( x= "Original Gift Source", y= "Percent") +
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1) ) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))
     
     
     
   })
   
   
   output$OrgPlot <- renderPlot({
    og() %>% 
       group_by(CityState) %>%
       dplyr::summarise(counts = n()) %>%  
       ggplot(aes(x =reorder(as.factor(CityState), counts), y=counts)) + 
       geom_bar(stat = 'identity', fill = "coral1") + 
       ggtitle("Organizational Donors") + 
       geom_text(aes(label=counts), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25) + 
       theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1)) +
       theme(panel.background = element_blank()) + theme(axis.line = element_line(colour= "black"))+
       labs( x= "Organizational Donors in City-State", y= "Count")
     
     
     
   })
   
   cap1 <- eventReactive(input$go, {paste("Organizational Donors from","<font color=\"#FF0000\"><b>", input$W1, "</b></font>", "to","<font color=\"#FF0000\"><b>", input$W2, "</b></font>")})
   output$OrgText <- renderText({cap1()})
   
   
   # Print data table
   output$Orgtable = renderDataTable(
     wq1(),
     options = list(
       pageLength = 25)
     
   )
    
}


# Run the application 
shinyApp(ui = ui, server = server)


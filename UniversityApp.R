library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(datasets)

load(url("https://github.com/CamillexZhao/STA6233-InteractiveProject/raw/master/2020QSWorldUniversityRankings.Rdata"))


# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("2020 University Comparison (Worldwide)"),  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("size", "University Size",
                   choices = sort(unique(UR_data$Size)),
                   selected = "L"),
      
      selectInput("country1", "Country(University A)",
                  choices = sort(unique(UR_data$Country)) ,
                  selected = "United States"), 
      selectInput("UniversityA", "University A",
                  choices = NULL),    
      
      selectInput("country2", "Country(University B)",
                  choices = sort(unique(UR_data$Country)) ,
                  selected = "United States"), 
      selectInput("UniversityB", "University B",
                  choices = NULL),   
      
    ),
    
    
    mainPanel(
      plotOutput("plot")
    )
  )
)



# Define the server logic
server <- function(session, input, output) {
  
  
  UniversityA <- subset(UR_data, InstitutionName == input$UniversityA, select = c("InstitutionName", input$stats))
  UniversityB <- subset(UR_data, InstitutionName == input$UniversityB, select = c("InstitutionName", input$stats))
  tempdf <- rbind(UniversityA, UniversityB)  
  
  
  #UniversityA <- subset(UR_data, InstitutionName == "Tsinghua University", select = c("InstitutionName", input$stats))
  #UniversityB <- subset(UR_data, InstitutionName == "University of Michigan", select = c("InstitutionName", input$stats))
  #tempdf <- rbind(UniversityA, UniversityB)  
}

newdf <- reshape(tempdf, varying = input$stats,
                 v.names = "University_Stats",
                 timevar = "Comparison_Type",
                 times = input$stats,
                 new.row.names = 1:10000,
                 direction = "long")






output$plot <- renderPlot({
  
  
  
  p <- ggplot(plotData(), aes(x=Comparison_Type, y=University_Stats, fill=InstitutionName)) +
    geom_bar(position="dodge", stat="identity") +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=17,face="bold"),
          legend.text=element_text(size=15)) +
    geom_text(aes(label=sprintf("%.02f", University_Stats)), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size=6)
  
  print(p)
})



observe({
  x1 <- UR_data %>% filter(Country == input$country1, Size == input$size)
  updateSelectInput(session, "UniversityA", "University A", choices =  unique(x1$InstitutionName))
  
  x2 <- UR_data %>% filter(Country == input$country2, Size == input$size)
  updateSelectInput(session, "UniversityB", "University B", choices =  unique(x2$InstitutionName))
})
}


# Run the application
shinyApp(ui = ui, server = server)



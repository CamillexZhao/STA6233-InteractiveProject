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
                  selected = "United Kingdom"), 
      selectInput("UniversityB", "University B",
                  choices = NULL)
    ),
    
    mainPanel(
      plotOutput("URplot")
    )
  )
)

# Define the server logic
server <- function(session, input, output) {
 observe({
    x1 <- UR_data %>% filter(Country == input$country1, Size == input$size)
    updateSelectInput(session, "UniversityA", "University A", choices =  unique(x1$InstitutionName))
    
    x2 <- UR_data %>% filter(Country == input$country2, Size == input$size)
    updateSelectInput(session, "UniversityB", "University B", choices =  unique(x2$InstitutionName))
  })
    
  plotData <- reactive({  
    UniA_data <- filter(UR_data, InstitutionName == input$UniversityA)
    UniB_data <- filter(UR_data, InstitutionName == input$UniversityB)
       
    tempdf <- rbind(UniA_data, UniB_data) 
    
    newdf <- reshape(tempdf, 
                     varying = c("Academic",	"Employer"	,"FacultyStudent", "CitationsperFaculty"	,"InternationalFaculty"	,"InternationalStudents",	"Overall"),
                     v.names = c("University_Stats"),
                     times=c("Academic",	"Employer"	,"FacultyStudent", "CitationsperFaculty"	,"InternationalFaculty"	,"InternationalStudents",	"Overall"),
                     timevar = "Comparison_Type",
                     new.row.names = 1:10000,
                     direction = "long")
    newdf
    
  })
  
  
  output$URplot <- renderPlot({
    ggplot(plotData(), aes(x=Comparison_Type, y=University_Stats, fill=InstitutionName)) +
      geom_bar(position="dodge", stat="identity") +
      theme(axis.text=element_text(size=9),
            axis.title=element_text(size=15,face="bold"),
            legend.text=element_text(size=12)) +
      
      geom_text(aes(label=sprintf("%.02f", University_Stats)), 
                position=position_dodge(width=0.9), 
                vjust=-0.25, size=5)
    
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(ggplot2)
library(GGally)
library(patchwork) 
library(dplyr)
library(plotly)
library(shinythemes)

load('full_df.Rdata')
load('GDP_final_data.Rdata')
load('all_data_list.Rdata')
load('GDP_COUNTRY_CODE.Rdata')

Olympic <- c("Los Angeles", "Seoul", "Barcelona", "Atlanta", "Sydney",
             "Athina", "Beijing", "London", "Rio De Janeiro", "Tokyo")

year_1 <- c("1984","1988","1992","1996","2000","2004","2008","2012","2016","2020")

rm_sport1 <- c("Artistic Swimming", "Basketball", "Equestrian Dressage", "Equestrian Eventing",
              "Equestrian Jumping", "Football", "Handball", "Hockey", "Modern Pentathlon", "Rhythmic Gymnastics",
              "Volleyball", "Water Polo")

Los_Angeles <- all_data[[1]]
age_dist_dat <- setdiff(unique(Los_Angeles$Events),rm_sport1)


rm_sport2 <- c("Football", "Handball", "Hockey", "Modern Pentathlon", "Water Polo")
medal_dist_dat <- setdiff(unique(Los_Angeles$Events),rm_sport2)

mapdata = map_data("world")
GDP_data_updated = GDP_data
GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'United States')] = 'USA'  
GDP_data_updated$Country_Name[which(GDP_data_updated$Country_Name == 'Great Britain')] = 'UK'


ui <- navbarPage("Explore Summer Olympics Data (1984-2020)",
                 theme = shinytheme('darkly'),

    sidebarLayout(
        sidebarPanel( 
          radioButtons("plottype", "Choose type of plots:",choices = list("Plots for individual years" = 1
                                                                          ,"Plots for individual events" = 2)),
            selectInput("year", "Select year",
                        choices = unique(data1$year)),
            
            checkboxGroupInput('checkbox',
                               'Choose Type',
                               choices = c('team','individual'),
                               selected = 'individual'),
            
            selectInput("event", "Choose Event",
                        choices = NULL),

            
    ),
        mainPanel(
          conditionalPanel(
            condition = 'input.plottype == 1',
            h3(textOutput('maptext')),
            plotOutput('mapplot'),
            h3(textOutput("top")),
            plotOutput("histplot"),
            h3(textOutput("low")),
            plotOutput("genderplot"),
            h3(textOutput("seventh")),
            plotlyOutput("GDPPlot")
          ),
          conditionalPanel(
            condition = 'input.plottype == 2',
            h3(textOutput("fourth")),
            plotOutput("pairplot") ,
            h3(textOutput('fifth')),
            plotOutput('boxplot'),
            h3(textOutput("sixth")),
            plotOutput("MultipleLinePlot")
          )
        )
    )
          
)

server <- function(input, output,session) {
  
  year <- reactive({
    subset(data1, year == input$year & Type == input$checkbox)
  })
  observeEvent(year(), {
    choices <- unique(year()$Events)
    updateSelectInput(inputId = "event", choices = choices)
  })
  
  
  
  
  output$top <- renderText({
    
    time <- input$year
    country0 <- Olympic[which(time == year_1, arr.ind = TRUE)]
    paste( "Medal distribution in",time,"Olympics","(",country0,")",sep = " ")
    
  })
  
  output$histplot <- renderPlot({
    index <- which(input$year == year_1, arr.ind = TRUE)
    
    new_dat <- all_data[[index]]
  
    
    g <- ggplot(new_dat, aes(Events)) + scale_fill_brewer(palette = "Spectral")
    
    g + geom_histogram(aes(fill=Medal), 
                       binwidth = .1,
                       stat = "count",
                       col="black", 
                       size=.1)+
      labs(y = "Medal",
           x='') + 
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
    
  })
  
  output$low <- renderText({
    time <- input$year
    country0 <- Olympic[which(time == year_1, arr.ind = TRUE)]
    
    paste("Gender distribution of different Events in",time,"Olympics","(",country0,")")
  })
  
  output$genderplot <- renderPlot({
    index <- which(input$year == year_1, arr.ind = TRUE)
    new_dat <- all_data[[index]]
    
    
    g <- ggplot(new_dat[new_dat$Type == 'individual' & is.na(new_dat$Gender) == F,], aes(Events)) + scale_fill_brewer(palette = "Spectral")
    
    g + geom_histogram(aes(fill=Gender), 
                       binwidth = .1,
                       stat = "count",
                       col="black", 
                       size=.1)+
      labs(y = "count",
           x ='') + 
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
    
    
  })
  
  output$fourth <- renderText({
    
    paste("Plot of association among Age, Height and Weight of medal winners in", input$event)
  })
  
  output$pairplot = renderPlot({
    
    index <- which(input$year == year_1, arr.ind = TRUE)
    new_dat <- subset(data1, data1$Events == input$event
                      & data1$Type == 'individual')
    ggpairs(new_dat, columns = c("Age", "Height", "Weight"), mapping=aes(colour=Medal, alpha = 0.7))
  })
  
  output$fifth <- renderText({
    
    paste("Boxplot of Age, Height and Weight of medal winners in", input$event)
  })
  
  output$boxplot = renderPlot({
    
    index <- which(input$year == year_1, arr.ind = TRUE)
    new_dat <- subset(data1, data1$Events == input$event
                      & data1$Type == 'individual')
    g1 = ggplot(data = new_dat) + 
      geom_boxplot(mapping = aes(x= Medal , y = Height , fill = Medal ))+
      labs(title = 'Height')
    
    g2 = ggplot(data = new_dat) + 
      geom_boxplot(mapping = aes(x= Medal , y = Weight , fill = Medal ))+
      labs(title = 'Weight')
    
    g3 = ggplot(data = new_dat) + 
      geom_boxplot(mapping = aes(x= Medal , y = Age , fill = Medal ))+
      labs(title = 'Age')
    
    g1+g2+g3
      
  })
  
  
  output$sixth <- renderText({
    
    paste("Number of medals won by top 5 dominating countries in ", input$event, 'over 1984 - 2020')
  })
  
  
  output$MultipleLinePlot = renderPlot({
    
    index <- which(input$year == year_1, arr.ind = TRUE)
    
    new_dat <- subset(data1, data1$Events == input$event)
    
    
    dom = names(sort(table(new_dat$Country), decreasing = T)[1:5])
    
    
    data3 = new_dat[new_dat$Country %in% dom == T,][c('year', 'Events', 'Country')]
    
    data4 = data3 %>% group_by(year, Country) %>% 
      summarise(count = n())
    
    ggplot(data4, aes(year, count, col = Country))+
      geom_line(size = 1.5)
    
  })
  
  output$seventh <- renderText({
    
    paste("Effect of GDP on Olympic Success", input$year)
  })
  
  output$GDPPlot = renderPlotly({
    index <- which(input$year == year_1, arr.ind = TRUE)
    
    new_dat1 <- subset(GDP_data, GDP_data$Year == input$year)
    GDP_PLOT = ggplot(new_dat1,aes(x = GDP,y = Medals,
                                               colour = Country))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    theme(legend.position = 'none')
  
  ggplotly(GDP_PLOT)
  })
  
  
  output$maptext = renderText({
    time <- input$year
    country0 <- Olympic[which(time == year_1, arr.ind = TRUE)]
    paste('Medals won by different countries in ',input$year, ' Olympics (',country0,')')
  })
  
  output$mapplot = renderPlot({
    map_ind = match(mapdata$region, GDP_data_updated[GDP_data_updated$Year == input$year,]$Country_Name)
    
    mapdata$map_medal = GDP_data_updated[GDP_data_updated$Year == input$year,]$Medals[map_ind] 
    
    ggplot(mapdata, aes(long, lat, fill = map_medal, group = group))+
      geom_polygon(colour = 'gray') + 
      scale_fill_continuous(low = "#f2d340",
                            high = "#6c1615",
                            guide = "colorbar")+
      labs(fill = 'No. of medals')
  })
  

}
# Run the application 
shinyApp(ui = ui, server = server)

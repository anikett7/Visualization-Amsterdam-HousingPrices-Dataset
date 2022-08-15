library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
housedataraw=read.csv("HousingPrices-Amsterdam-August-2021.csv",header=TRUE)
housedata=na.omit(housedataraw)
ui<-dashboardPage(title="EDA Of Amsterdam Housing Prices Dataset-Visualization Semester Project",
         dashboardHeader(title="EDA Of Amsterdam Housing Dataset",titleWidth=400),
         dashboardSidebar(width=400,sidebarMenu(
            menuItem("Univariate Plots",tabName="menu1"),
            menuItem("Bivariate/Multivariate Plots",tabName="menu2"),
            menuItem("Interactive Maps",tabName="menu3"))),
         dashboardBody(
           tabItems(
             tabItem("menu1",
                       titlePanel("Univariate Plots"),
                       fluidRow(column(width=4,offset=4,selectInput("varble",label="Select Any Variable",choices=c("No. Of Rooms","Area","Price","Longitude","Latitude")))),
                       tabsetPanel(
                       tabPanel("Summary",fluidRow(column(width=4,offset=4,actionButton("view1","Show Summary"))),
                                          fluidRow(column(width=4,offset=3,verbatimTextOutput("plot1")))),
                       tabPanel("Boxplot",fluidRow(column(width=4,offset=4,actionButton("view2","Show Boxplot"))),
                                            fluidRow(plotOutput("plot2"))),
                       tabPanel("Histogram/Barplot",fluidRow(plotOutput("plot3"))))),
             
            tabItem("menu2",
                     titlePanel("Bivariate/Multivariate Plots"),
                     tabsetPanel(
                       tabPanel("Side By Side Boxplot",fluidRow(column(width=4,offset=2,selectInput("vbl1",label="Select the Discrete Variable",choices=c("No. Of Rooms"))),
                                                                column(width=4,selectInput("vbl2",label="Select Any Continuous Variable",choices=c("Area","Price","Longitude","Latitude")))),
                                                       fluidRow(column(width=4,offset=4,actionButton("view4","Show Side By Side Boxplot"))),
                                                       fluidRow(plotOutput("plot4"))),
                       tabPanel("Scatter plot",fluidRow(column(width=4,offset=2,selectInput("vbl3",label="Select 1st Variable",choices=c("No. Of Rooms","Area","Price","Longitude","Latitude"))),
                                                        column(width=4,selectInput("vbl4", label="Select 2nd Variable",choices=c("No. Of Rooms","Area","Price","Longitude","Latitude")))),
                                fluidRow(plotOutput("plot5"))))),
            tabItem("menu3",
                    titlePanel("Interactive Maps Of Amsterdam Houses"),fluidRow(column(width=4,offset=4,selectInput("vblmap",label="Select Any Variable",choices=c("No. Of Rooms","Area","Price")))),
                    fluidRow(leafletOutput("mymap"))))
           
         )
                  )
server<-function(input,output){
  varbledata=reactive({switch(input$varble,
                              "No. Of Rooms"=housedata$Room,
                              "Area"=housedata$Area,
                              "Price"=housedata$Price,
                              "Longitude"=housedata$Lon,
                              "Latitude"=housedata$Lat)})
  df21=reactive({data.frame(varbledata())})
  observeEvent(input$view1,{output$plot1=renderPrint({isolate(summary(varbledata()))})})
  observeEvent(input$view2,{output$plot2=renderPlot({isolate(boxplot(varbledata(),col=rgb(runif(1),runif(1),runif(1)),xlab=input$varble))})})
  output$plot3=renderPlot({
    if (input$varble=="No. Of Rooms"){
      df17=data.frame(housedata$Room)
      ggplot(df17)+geom_bar(aes(x=housedata$Room),fill=rgb(runif(1),runif(1),runif(1)))+scale_x_discrete(limits=1:15)+
      labs(title="Bar Plot",x=input$varble)+theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),
      axis.title=element_text(size=14),axis.text=element_text(size=12))
    }
    else{
    ggplot(data=df21())+geom_histogram(mapping=aes(x=varbledata(),y=..density..),
    fill=rgb(runif(1),runif(1),runif(1)))+labs(x=input$varble,title="Histogram")+theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),
                                                                       axis.title=element_text(size=14),axis.text=element_text(size=12))}})
  
  
  vabl1=reactive({switch(input$vbl1,"No. Of Rooms"=housedata$Room)})
  vabl2=reactive({switch(input$vbl2,
                         "Area"=housedata$Area,
                         "Price"=housedata$Price,
                         "Longitude"=housedata$Lon,
                         "Latitude"=housedata$Lat)})
  vabl3=reactive({switch(input$vbl3,
                         "No. Of Rooms"=housedata$Room,
                         "Area"=housedata$Area,
                         "Price"=housedata$Price,
                         "Longitude"=housedata$Lon,
                         "Latitude"=housedata$Lat)})
  vabl4=reactive({switch(input$vbl4,
                         "No. Of Rooms"=housedata$Room,
                         "Area"=housedata$Area,
                         "Price"=housedata$Price,
                         "Longitude"=housedata$Lon,
                         "Latitude"=housedata$Lat)})
 
   df22=reactive({data.frame(cbind(vabl3(),vabl4()))})
  
  observeEvent(input$view4,{output$plot4=renderPlot({isolate(boxplot(vabl2()~vabl1(),col=rgb(runif(5),runif(5),runif(5)), 
                                                    main="Side By Side Boxplot Against Different No, Of Rooms",xlab=input$vabl1,ylab=input$vabl2))})})
  output$plot5=renderPlot({ggplot(df22())+geom_point(mapping=aes(x=vabl3(),y=vabl4()))+labs(title="Scatter Plot",
                                                     x=input$vbl3,y=input$vbl4)+theme(plot.title=element_text(hjust=0.5,size=15,face="bold"),
                                                     axis.title=element_text(size=14),axis.text=element_text(size=12))})
  
  mapdata=reactive({switch(input$vblmap,
                           "No. Of Rooms"=housedata$Room,
                           "Area"=housedata$Area,
                           "Price"=housedata$Price)})
  output$mymap=renderLeaflet({pal=colorNumeric(palette="RdYlGn",domain=mapdata())
  leaflet(housedata)%>%addTiles()%>%addCircles(lng=~Lon, lat=~Lat,color=~pal(mapdata()))%>%addLegend(pal=pal, values=~mapdata(),title=input$vblmap)})
  
}
shinyApp(ui,server)

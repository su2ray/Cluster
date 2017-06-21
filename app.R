library(shiny)
library(plotrix)
library(ggplot2)
library(gridExtra)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
circleFun <- function(center = c(0,0),r = 1, npoints = 100){
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The Typhoone Megi ETQPF track"),
  
  
   # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("value", "Chose the Typhone center Location time(LST):", 
                  choices = c("2016-09-27 11","2016-09-27 14","2016-09-27 17","2016-09-27 20","2016-09-27 23"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot",  width = "75%",height="2500px")
    )
  )
)

# Define server logic required to draw a histogram
server <-function(input, output) {

  output$distPlot <- renderPlot({
    	  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$value,
           "2016-09-27 11"=1,
		   "2016-09-27 14"=2,
		   "2016-09-27 17"=3,
		   "2016-09-27 20"=4,
		   "2016-09-27 23"=5)
  })
  
  
    i <- datasetInput()
    # intial time (UTC)
    dtg2=16092700+(i-1)*3
    # Typthoon locate time = forecast time (LST)
    dtg=dtg2+8+3
    fn='D:\\Users\\b903\\Documents\\Shiny_work\\etqpf_meta\\yymmddhh_3_3.meta'
    fn<-gsub("yymmddhh",dtg2,fn)
    org.t=read.table(file=fn,header=FALSE)
    org.terrain=read.table(file="D:\\Users\\b903\\Documents\\Shiny_work\\tw_coast.dat",header=FALSE)
    dt=org.t
    
    center=cbind(c(122.6,23.6),c(121.6,24),c(120.8,23.9),c(120.3,23.6),c(120,23.9),c(119.7,24.6))
    
    #---graph 1 : the map
    # range =-0.6
    
    
    cake= data.frame(x=dt[,4],y=dt[,5],g=factor(dt[,11]))
    cent= data.frame(cenx=center[1,],ceny=center[2,])
    terr=data.frame(tx=org.terrain[,1],ty=org.terrain[,2])
  
    cir<- circleFun(c(center[1,i],center[2,i]),0.5,npoints = 800)
    cir2<- circleFun(c(center[1,i-1],center[2,i-1]),0.5,npoints = 200)
    cir3<- circleFun(c(center[1,i+1],center[2,i+1]),0.5,npoints = 200)
    p1<-ggplot()+
      geom_point(data = cake, aes(x,y, col =factor(g))) +
      geom_point(data = cent,aes(cenx,ceny),col="black") +
      geom_line(data = cent,aes(cenx,ceny),col="black") +
      geom_point(data = cir,aes(x,y),col="gray21",size=0.5) +
      geom_point(data = cir2,aes(x,y),col="gray21",size=0.5) +
      geom_point(data = cir3,aes(x,y),col="gray21",size=0.5) +
      geom_point(data = terr,aes(tx,ty),col="gray",size=0.5) + 
      scale_color_manual(breaks = c("1", "2", "3","4"), values=c("red", "green", "blue", "black")) +
      ggtitle(paste(' The Typhone center Location at 20',dtg,sep = "")) + theme(plot.title = element_text(size = 20, face = "bold"))+
      coord_cartesian(xlim=c((center[1,i]-0.6),(center[1,i]+0.6)),ylim=c((center[2,i]-0.6),(center[2,i]+0.6))) +
      theme(panel.background = element_rect(fill = "white",colour="black"))+
      labs(x="lontitude",y="latitude",col="Cluster")
    
  #---graph 2 : the polar

  # north as 0
  cake= data.frame(theta=dt[,7],r=dt[,6],g=factor(dt[,11]))
  p2<-ggplot(data = cake, aes(r,theta, col =factor(g)))+
    geom_point() + coord_polar(theta = "y", start = 0) +
    ggtitle(paste('Typhoon Moving S/D at 20',dtg,sep = "")) + theme(plot.title = element_text(size = 20, face = "bold"))+
    scale_color_manual(breaks = c("1", "2", "3","4"), values=c("red", "green", "blue", "black")) +
    scale_y_continuous(breaks=seq(0, 330, by=30), expand=c(0,0), lim=c(0, 360))+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(panel.background = element_rect(fill = "gray77"))+
    theme(axis.text.x = element_text(face="bold", colour="black", size=18),legend.background=element_rect(colour="black"))+
    labs(col="Cluster")
  
  #---graph 3 : the VerticalWindShear

  cake= data.frame(x=dt[,8],y=factor(dt[,11]))
p3<-ggplot(data = cake, aes(y,x,col=factor(y)))+
 geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=F)+ 
 ggtitle(paste('Vertical Wind Shear at 20',dtg,sep = "")) + theme(plot.title = element_text(size = 20, face = "bold"))+
 scale_color_manual(breaks = c("1", "2", "3","4"), values=c("red", "green", "blue", "black"))+
 theme(legend.position="none")+
     labs(col="Cluster")



#---graph 4 : the TyStrenth

  cake= data.frame(x=dt[,9],y=factor(dt[,11]))
p4<-ggplot(data = cake, aes(y,x,col=factor(y)))+
 geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=F)+ 
  ggtitle(paste('Ty Strenth at 20',dtg,sep = "")) + theme(plot.title = element_text(size = 20, face = "bold"))+
 scale_color_manual(breaks = c("1", "2", "3","4"), values=c("red", "green", "blue", "black"))+
 theme(legend.position="none")+
     labs(col="Cluster")




#---graph 5 : the RainIntensity

  cake= data.frame(x=dt[,10],y=factor(dt[,11]))
p5<- ggplot(data = cake, aes(y,x,col=factor(y)))+
 geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=F)+ 
 ggtitle(paste('Precipitation Intensity at 20',dtg,sep = "")) + theme(plot.title = element_text(size = 20, face = "bold"))+
 scale_color_manual(breaks = c("1", "2", "3","4"), values=c("red", "green", "blue", "black"))+
 theme(legend.position="none")+
     labs(col="Cluster")

  
  multiplot(p1, p2,p3,p4,p5,cols=1)
})
}

shinyApp(ui=ui, server = server)
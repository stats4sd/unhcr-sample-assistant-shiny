type="numeric"
var=2
prob=0.5
moe=0.05
stages=3
stagePop<-c(1000,100,10)
conf=0.95
rho=c(0.25,0,0)
nH=10
poptype="explicit"




library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample Size Calculator for Estimation of Sample Size from Multi-Stage Sample: Solve for N"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(textInput(inputId = "name",label = "Name of Variable"),
    selectInput(inputId = "type",label = "Type of Variable",choices = c("Percentage"="binary","Number"="numeric"),selected=NA),
    uiOutput("prob"),
    uiOutput("sd1"),
    numericInput(inputId = "moe",label = "Desired Margin of Error",value=5,min=0.1,step = 0.1),
    numericInput(inputId = "conf",label = "Desired Confidence Level",value=0.95,min=0.8,max=0.99,step = 0.01),
    selectInput(inputId = "clus",label = "Clustering Level",choices=c("None (rho=0)"=0,"Low (rho=0.1)"=0.1,"Moderate (rho=0.25)"=0.25,
                                                                      "High (rho=0.5)"=0.5),selected = 0.1),
    numericInput(inputId = "nHH",label = "Number of Samples per PSU",value=10,min=1,max=5000,step = 1),
    numericInput(inputId = "stages",label = "Number of Sampling Stages",value=3,min=2,max=6,step = 1), 
    numericInput(inputId = "Population",label = "Expected Population Size",value=500000,min=1,max=99999999999999,step = 10000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("Results",tableOutput("result")),
      tabPanel("N by Margin of Error",plotOutput("Nbymoe")),
      tabPanel("N by Clustering Level",plotOutput("Nbyrho")))
        
))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$prob<- renderUI({
    if(input$type=="binary"){
    numericInput(inputId = "prop",label = "Expected %",value=50,min=0,max=100,step = 1)}
  })
  output$sd1 <- renderUI({
    if(input$type=="numeric"){
      numericInput(inputId = "sd",label = "Standard Deviation",value=100,min=0,max=1000000,step = 0.1)}
  })

  
output$result<-renderTable({
    
     if(input$type=="numeric"){
      SRSn<-(input$sd*qnorm(1-(1-input$conf)/2)/input$moe)**2
      }
    rho<-as.numeric(as.character(input$clus))
    
    if(input$type=="binary"){
      p1<-input$prop/100
      moe<-input$moe/100
      SRSn<-(sqrt(p1*(1-p1))*qnorm(1-(1-input$conf)/2)/moe)**2
    }
    SRSn_FPC<-ceiling((SRSn*input$Population)/(SRSn+input$Population-1))
    
    DEFF1<-(1+(input$nHH-1)*rho)
    
    FinalstageN<-SRSn*DEFF1
    FinalstageN_FPC<-ceiling((FinalstageN*input$Population)/(FinalstageN+input$Population-1))
    
    stage2N<-ceiling(FinalstageN_FPC/input$nHH)
   
    data.frame(text=c("Sample Size Required from Simple Random Sample (1 stage)","Design Effect",
                      "Sample Size Required from Clustered Multi-Stage Sample","Number of Level 2 Clusters Required"),
               value=c(SRSn_FPC,DEFF1,FinalstageN_FPC,stage2N))
    
  },colnames=FALSE,rownames=FALSE)


output$Nbyrho<-renderPlot({
  
  if(input$type=="numeric"){
    SRSn<-(input$sd*qnorm(1-(1-input$conf)/2)/input$moe)**2
  }
  rho<-as.numeric(as.character(input$clus))
  
  if(input$type=="binary"){
    p1<-input$prop/100
    moe<-input$moe/100
    SRSn<-(sqrt(p1*(1-p1))*qnorm(1-(1-input$conf)/2)/moe)**2
  }
  SRSn_FPC<-ceiling((SRSn*input$Population)/(SRSn+input$Population-1))
  
  DEFF1<-(1+(input$nHH-1)*seq(0,1,by=0.001))
  
  FinalstageN<-SRSn*DEFF1
  FinalstageN_FPC<-ceiling((FinalstageN*input$Population)/(FinalstageN+input$Population-1))
  
  p1<-data.frame(DEFF1,FinalstageN_FPC,rho=seq(0,1,by=0.001))
ggplot(data=p1,aes(y=FinalstageN_FPC,x=rho))+geom_line()
  
})

output$Nbymoe<-renderPlot({
  
  if(input$type=="numeric"){
    SRSn<-(input$sd*qnorm(1-(1-input$conf)/2)/seq(input$sd/100,input$sd*100,length.out = 1000))**2
  }
  rho<-as.numeric(as.character(input$clus))
  
  if(input$type=="binary"){
    p1<-input$prop/100
    moe<-input$moe/100
    SRSn<-(sqrt(p1*(1-p1))*qnorm(1-(1-input$conf)/2)/seq(0.01,0.15,by=0.001))**2
  }
  SRSn_FPC<-ceiling((SRSn*input$Population)/(SRSn+input$Population-1))
  
  DEFF1<-(1+(input$nHH-1)*rho)
  
  FinalstageN<-SRSn*DEFF1
  FinalstageN_FPC<-ceiling((FinalstageN*input$Population)/(FinalstageN+input$Population-1))
  
  p1<-data.frame(DEFF1,FinalstageN_FPC,moe=seq(0.01,0.15,by=0.001))
  ggplot(data=p1,aes(y=FinalstageN_FPC,x=moe))+geom_line()
  
})
}


# Run the application 
shinyApp(ui = ui, server = server)


library(readr)
library(ggplot2)
library(cowplot)
library(shiny)
library(shinydashboard)

load("mushroom_correct.RData")

ui<-dashboardPage(
  skin = "red",
  dashboardHeader(
    title="Muahrooms Analysis" 
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mushroom Feature Plot", tabName = "Data",icon = icon("bar-chart-o")),
      menuItem("Testset Prediction",tabName = "testset",icon = icon("eye-open",lib = "glyphicon" ),
               menuSubItem("Testdata",tabName="testdata",icon=icon("open",lib = "glyphicon" )),
               menuSubItem("Prediction Result",tabName = "download",icon = icon("download-alt",lib = "glyphicon" ))),
      menuItem("Poison Prediction",tabName = "choosedata",icon = icon("eye-open",lib = "glyphicon" )),
      menuItem("About", tabName = "about",icon = icon("comment",lib = "glyphicon" ))

      
    )),
  dashboardBody(
    tabItems(      
      tabItem(tabName = "Data",
              fluidRow(
                column(12,
                       box(
                         radioButtons("var",
                                      label = "Mushroom Feature",
                                      choices = c("cap color" = 1, "cap surface" = 2,"cap shape" = 3,
                                                  "bruises" =4,"odor"=5,"gill attachment"=6,"gill spacing"=7,
                                                  "gill size"=8,"gill color"=9, "stalk shape"=10,"stalk root"=11,
                                                  "stalk surface above ring"=12,"stalk surface below ring"=13,
                                                  "stalk color above ring"=14,"stalk color below ring"=15,
                                                  "veil type"=16,"veil color"=17,"ring number"=18,"ring type"=19,
                                                  "spore print color"=20,"population"=21,"habitat"=22),
                                      selected = 1), width = 3) ,
                       box(br(),br(),br(),plotOutput("Plot"),width=9,   br(),
                           h3("Mushroom Feature Plot",align="center"),  br(), br()
                       )
                )
              )
      ),
      tabItem(tabName = "testdata",
              fluidRow(width=20,

                      box(width=12,
                         br(),
                           tags$h4("With this shiny prediction app, you can upload your data and get back predictions.
                                   The model is a Random Forest that predicts whether the mushroom is edible from the features.", style="font-size:150%"),
                         br(),
                          tags$h4("To predict using this model, upload test data in csv format by using the button below.", style="font-size:150%"),
                         br(),
                           tags$h4("Then, go to the", tags$span("Prediction Result",style="color:red"),
                                   tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),
                           br(),
                           br(),
                       column(width=8,
                          fileInput('file1', em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                            accept=c('.csv')))),
                                  
                         uiOutput("sample_input_data_heading"),
                         tableOutput("sample_input_data"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br()
              
              )),
      tabItem(tabName = "download",
              fluidRow(width=20,
                box(width = 12,
                br(),
                tags$h4("After you upload a test dataset, you can download the predictions in csv format by
                               clicking the button below.", 
                               style="font-size:150%"),
                       br(),
                       br(),
                column(width = 8,
                       downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%"))
                ),
                br(),
                br()),
                       uiOutput("sample_prediction_heading"),
                       tableOutput("sample_predictions")
                
                
              )),
      tabItem(tabName = "choosedata",
              fluidRow(
                column(12,
                       tags$h4("Choose the mushroom feature to do the prediction",style="font-size:150%"),
                       br(),
                       box(width = 3,
                         selectInput("x1",
                                     label = "cap color",
                                     choices = c( "b","c","e","g","n","p","r","u","w","y" )
                                    ),
                         selectInput("x2",
                                     label = "cap surface",
                                     choices = c( "f","g","s","y"),
                                     selected = "f"),
                         selectInput("x3",
                                     label = "cap shape",
                                     choices = c( "b","c","f","k","s","x"),
                                     selected = "b"),
                         selectInput("x4",
                                     label = "bruises",
                                     choices = c( "f","t" ),
                                     selected = "f"),
                         selectInput("x5",
                                     label = "odor",
                                     choices = c( "a","c","f","l","m","n","p","s","y" ),
                                     selected = "a"),
                         selectInput("x6",
                                     label = "gill attachment",
                                     choices = c( "a","d","f","n"),
                                     selected = "a")),
                         box(width=3,
                         
                         selectInput("x7",
                                     label = "gill spacing",
                                     choices = c( "c","d","w"),
                                     selected = "c"),
                         selectInput("x8",
                                     label = "gill size",
                                     choices = c( "b","n" ),
                                     selected = "b"),
                         selectInput("x9",
                                     label = "gill color",
                                     choices = c( "b","e","g","h","k","n","o","p","r","u","w","y" ),
                                     selected = "b"),
                         selectInput("x10",
                                     label = "stalk shape",
                                     choices = c( "e","t" ),
                                     selected = "e"),
                         selectInput("x11",
                                     label = "stalk root",
                                     choices = c( "b","c","e","r","u","z" ,"NA"),
                                     selected = "b"),
                         selectInput("x12",
                                     label = "stalk surface above ring",
                                     choices = c( "f","k","s","y" ),
                                     selected = "f")),
                       box(width = 3,
                           selectInput("x13",
                                       label = "stalk surface below ring",
                                       choices = c( "f","k","s","y" ),
                                       selected = "f"),
                           selectInput("x14",
                                       label = "stalk color above ring",
                                       choices = c( "b","c","e","g","n","o","p","w","y"),
                                       selected = "b"),
                           selectInput("x15",
                                       label = "stalk color below ring",
                                       choices = c( "b","c","e","g","n","o","p","w","y"),
                                       selected = "b"),
                           selectInput("x16",
                                       label = "veil type",
                                       choices = c( "p","u" ),
                                       selected = "p"),
                           selectInput("x17",
                                       label = "veil color",
                                       choices = c( "n","o","w","y"),
                                       selected = "n"),
                           selectInput("x18",
                                       label = "ring number",
                                       choices = c( "b","n","o","t"),
                                       selected = "b")),
                           
                           box(width=3,
                               selectInput("x19",
                                           label = "ring type",
                                           choices = c( "c","e","f","l","n","p","s","z"),
                                           selected = "c"),
                               selectInput("x20",
                                           label = "spore print color",
                                           choices = c( "b","h","k","n","o","r","u","w","y"),
                                           selected = "b"),
                               selectInput("x21",
                                           label = "population",
                                           choices = c( "a","c","n","s","v","y"),
                                           selected = "a"),
                               selectInput("x22",
                                           label = "habitat",
                                           choices = c( "d","g","l","p","m","u","w"),
                                           selected = "d")
                               
                               ),
                      box(width=12,textOutput("class"),
                       br())
                       ))),
      
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       box(
                         
                         p("Attribute Information: (classes: edible=e, poisonous=p)"),
                         p("cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s"),
                         p("cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s"),
                         p("cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r,pink=p,purple=u,red=e,white=w,yellow=y"),
                         p("bruises: bruises=t,no=f"),
                         p("odor: almond=a,anise=l,creosote=c,fishy=y,foul=f,musty=m,none=n,pungent=p,spicy=s"),
                         p("gill-attachment: attached=a,descending=d,free=f,notched=n"),
                         p("gill-spacing: close=c,crowded=w,distant=d"),
                         p("gill-size: broad=b,narrow=n"),
                         p("gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g,green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y"),
                         p("stalk-shape: enlarging=e,tapering=t"),
                         p("stalk-root: bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?"),
                         p("stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s"),
                         p("stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s"),
                         p("stalk-color-above-ring:brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y"),
                         p("stalk-color-below-ring:brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y"),
                         p("veil-type: partial=p,universal=u"),
                         p("veil-color: brown=n,orange=o,white=w,yellow=y"),
                         p("ring-number: none=n,one=o,two=t"),
                         p("ring-type:cobwebby=c,evanescent=e,flaring=f,large=l,none=n,pendant=p,sheathing=s,zone=z"),
                         p("spore-print-color:black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y"),
                         p("population: abundant=a,clustered=c,numerous=n,scattered=s,several=v,solitary=y"),
                         p("habitat: grasses=g,leaves=l,meadows=m,paths=p,urban=u,waste=w,woods=d"),
                         width=12
                       )
                )
              ))
    )))


server <-function(input, output){
  options(shiny.maxRequestSize = 800*1024^2)  
  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  read.csv(input$file1$datapath,header = TRUE)
      head(input_data)
    }
  })
  predictions<-reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  read.csv(input$file1$datapath,header = TRUE)
        prediction = predict(rf.1.cv, input_data)
        input_data_with_prediction = cbind(prediction,input_data )
        input_data_with_prediction
        
      })
    }
  })
  

  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable({   
    pred = predictions()
    head(pred)
    
  })
  

  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
  
  
 
  
  predictions1 <- reactive({
   newdata<- data.frame(input$x1,input$x2,input$x3,input$x4,
               input$x5,input$x6,input$x7,input$x8,input$x9,
               input$x10,input$x11,input$x12,
               input$x13,input$x14,input$x15,input$x16,
               input$x17,input$x18,input$x19,input$x20,
              input$x21,input$x22)
   fields <- c("cap.color",
               "cap.surface",
               "cap.shape",
               "bruises",
               "odor",
               "gill.attachment",
               "gill.spacing",
               "gill.size",
               "gill.color",
               "stalk.shape",
               "stalk.root",
               "stalk.surface.above.ring",
               "stalk.surface.below.ring",
               "stalk.color.above.ring",
               "stalk.color.below.ring",
               "veil.type",
               "veil.color",
               "ring.number",
               "ring.type",
               "spore.print.color",
               "population",
               "habitat")
   colnames(newdata) <- fields
   newdata1<- rbind(x_train[1, ] , newdata)
   newdata<-newdata1[-1,]
   prediction1 = predict(rf.1.cv, newdata)
   prediction1
   
 })
  
  output$class <- renderText({ 
    paste("The prediction result is: ",  predictions1())
  })
  
  output$Plot<- renderPlot(
    if(as.numeric(input$var) == 1){
      c1<- ggplot(data=mushrooms, aes(x = cap.color, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$cap.color))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 2){
      c1<- ggplot(data=mushrooms, aes(x = cap.surface, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$cap.surface))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 3){
      c1<- ggplot(data=mushrooms, aes(x = cap.shape, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$cap.shape))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 4){
      c1<- ggplot(data=mushrooms, aes(x = bruises, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$bruises))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 5){
      c1<- ggplot(data=mushrooms, aes(x = odor, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$odor))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 6){
      c1<- ggplot(data=mushrooms, aes(x = gill.attachment, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$gill.attachment))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 7){
      c1<- ggplot(data=mushrooms, aes(x = gill.spacing, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$gill.spacing))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 8){
      c1<- ggplot(data=mushrooms, aes(x = gill.size, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$gill.size))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 9){
      c1<- ggplot(data=mushrooms, aes(x = gill.color, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$gill.color))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 22){
      c1<- ggplot(data=mushrooms, aes(x = habitat, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$habitat))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 10){
      c1<- ggplot(data=mushrooms, aes(x = stalk.shape, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.shape))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 11){
      c1<- ggplot(data=mushrooms, aes(x = stalk.root, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.root))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 12){
      c1<- ggplot(data=mushrooms, aes(x = stalk.surface.above.ring, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.surface.above.ring))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 13){
      c1<- ggplot(data=mushrooms, aes(x = stalk.surface.below.ring, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.surface.below.ring))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 14){
      c1<- ggplot(data=mushrooms, aes(x = stalk.color.above.ring, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.color.above.ring))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 15){
      c1<- ggplot(data=mushrooms, aes(x = stalk.color.below.ring, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$stalk.color.below.ring))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 16){
      c1<- ggplot(data=mushrooms, aes(x = veil.type, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$veil.type))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 17){
      c1<- ggplot(data=mushrooms, aes(x = veil.color, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$veil.color))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 18){
      c1<- ggplot(data=mushrooms, aes(x = ring.number, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$ring.number))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 19){
      c1<- ggplot(data=mushrooms, aes(x = ring.type, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$ring.type))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 20){
      c1<- ggplot(data=mushrooms, aes(x = spore.print.color, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$spore.print.color))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
    else if(as.numeric(input$var) == 21){
      c1<- ggplot(data=mushrooms, aes(x = population, fill = class)) + geom_bar()  + scale_fill_manual("legend", values = c("e" = "pink", "p" = "red")) + ggtitle("")
      df1<-data.frame(table(mushrooms$population))
      df1 = df1[order(df1$Freq, decreasing = TRUE),] 
      myLabel = as.vector(df1$Var1)   
      myLabel = paste(myLabel, "(", round(df1$Freq / sum(df1$Freq) * 100, 2), "%)", sep = "")
      c2 <- ggplot(df1, aes(x = "", y = Freq, fill = Var1)) + geom_bar(stat = "identity", width = 1) +scale_fill_discrete(breaks = df1$Var1, labels = myLabel)+coord_polar(theta = "y") + labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())+ theme(legend.title = element_blank()) 
      plot_grid(c1,c2,ncol = 2,nrow = 1)
    }
  )
  
}

shinyApp(ui, server)

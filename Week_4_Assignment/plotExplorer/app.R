#
# THis app is for Coursers Data Products course week 4
# peernisse@gmail.com
# https://github.com/peernisse/dataProducts/blob/master/Week_4_Assignment/plotExplorer
#

library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Plot Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h5('Pick Species'),
        uiOutput('choose_species'),
        # h5('Pick Y Variable'),
        # uiOutput('choose_Y'),
        # h5('Pick X Variable'),
        # uiOutput('choose_X'),
        width=2
        
      ),#end sidebar layout
      
      # Show some interactive plots
      mainPanel(
        tabsetPanel(type = 'tabs',
            tabPanel("Explorer App Overview",
                     
                       h4("App Description"),
                     p('This app allows the user to explore the `Iris` dataset 
                       from the base R `data` package. The purpose is to demonstrate 
                       R Shiny interactivity and dynamic plots.'),
                     p('This app could be redesigned to take user input files such 
                       as CSV files and render the data in various plots or statistical 
                       summary tables.'),
                     h4('Instructions'),
                     p('Navigate the app using the tabs at the top. The select species box on the left 
                       allow you to select which iris species to plot. No plots will show up until
at least one species is selected. Each plot type also has some additional tools associated with the plot type.'),
                     p('The Plotting Tools tab contains both a scatter plot and a boxplot (scroll down the page)'),
                     h4('Source Code'),
                     p('The source code for this app is available on my Github:'),
                     p(
                       tags$a(href="https://github.com/peernisse/dataProducts/blob/master/Week_4_Assignment/plotExplorer/app.R",
                              "https://github.com/peernisse/dataProducts/blob/master/Week_4_Assignment/plotExplorer/app.R")
                     ),
                     h4('References'),
                     p('Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole. (has iris3 as iris.)'),
                     p('Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan
  McPherson (2019). shiny: Web Application Framework for R. R package
  version 1.3.2. https://CRAN.R-project.org/package=shiny'),
                     p('R Core Team (2018). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.')
                     
                  ),#End overview tab
            
            tabPanel("Plotting Tools",
                     fluidRow(
                         h3('Scatter Plot Comparisons'),
                         fluidRow(
                           column(6,h5('Pick X Variable'),
                                  uiOutput('choose_X')),
                           column(6,h5('Pick Y Variable'),
                                  uiOutput('choose_Y'))
                         ),
                         uiOutput('lines'),
                         plotOutput('scatter')
                     ),#end fluid row,
                     hr(),
                     fluidRow(
                       h3('Boxplot Comparisons'),
                       uiOutput('tsFacetSwap'),
                       plotOutput('boxplot')
                     )#End fluid row
                     
                     
                     
                     )#End plotting panel
          
        )#End tabset panel
         
      )
   )#End main panel
)#End fluid page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  #Load up the iris data
  pData<-reactive({
    
    df<-iris %>% 
      gather(PARAMETER,RESULT,1:4)
    
    return(df)
    
  })#End pData
  
  #Load up the iris data wide for plotting
  plotData<-reactive({
    
    df2<-iris %>% as.data.frame(.)
    
    return(df2)
    
  })#End pData
  
  #Create species picker
  #This loads up by default with no choices available
  output$choose_species<-renderUI({
    
    if(is.null(pData()))
      return()
    spcs<-sort(unique(pData()$Species))
    
    checkboxGroupInput('species',NULL,
                       choices = spcs,
                       selected = NULL)
  })
  
  
  #Make y variable dropdown
  output$choose_Y<-renderUI({
    if(is.null(pData()))
      return()
    
    yvar<-pData() %>% 
      select(PARAMETER) %>% 
      unique(.) %>% 
      arrange(.) %>% 
      pull(.)
    
    pickerInput('yval',choices=yvar)
    
  })#End choose_Y
  
  #Make x variable dropdown
  output$choose_X<-renderUI({
    if(is.null(pData()))
      return()
    
    xvar<-pData() %>% 
      select(PARAMETER) %>% 
      unique(.) %>% 
      arrange(.) %>% 
      pull(.)
    
    pickerInput('xval',choices=xvar)
    
  })#End choose_Y
  
  
  #Create ggplots that are linked to the checkboxes
  #Create fit line toggle for the scatterplot
  output$lines<-renderUI({
    if(is.null(pData()))
      return()
    
    checkboxInput('fitLine',label='Toggle Best-Fit Line')
    
  })#End lines input
  
  #Scatter plot
  output$scatter<-renderPlot({
    
    if(is.null(input$species))
      return()
    
    tsData<-plotData() %>%
        filter(
          Species %in% input$species
        ) %>%
        select(Species,input$xval,input$yval)
    
    if(input$fitLine == TRUE){lns<-'TRUE'} else lns<-'FALSE'
    #Make the plot
     #Must use "aes_string" to get ggplot to recognize dynamic input from input boxes
    ggplot(tsData,aes_string(x=input$xval,y=input$yval,color='Species'))+
      geom_point(size=2)+
      {if(lns==TRUE)geom_smooth(method="lm")}+
      theme(legend.position = 'bottom',legend.title=element_blank())
      
    })#End render scatterplot
  
  #Boxplots and facets
  #Make facet swapping picklist
  #Swap plot faceting variable picklist
  output$tsFacetSwap<-renderUI({
    pickerInput('tsSwap',label = 'Swap Faceting Variables',choices = c('Parameter','Species'),
                selected='Parameter') 
    
  })
  
  output$boxplot<-renderPlot({
    if(is.null(input$species))
      return()
    
    bpData<-pData() %>%
      filter(
        Species %in% input$species
      )
    
    if(input$tsSwap=='Parameter'){
      tsFacet<-'PARAMETER'
      xaxis<- 'Species'
      
    }
    if(input$tsSwap=='Species'){
      tsFacet<-'Species'
      xaxis<- 'PARAMETER'
      
    }
    
      ggplot(bpData,aes_string(x=xaxis,y='RESULT',fill=xaxis))+
      geom_boxplot()+
      facet_wrap(as.formula(paste('~',tsFacet)))+
      theme(legend.position = 'bottom',legend.title=element_blank())
    
    
    
  })#End render boxplot
  
  
  
  
  
  
}#End server component





# Run the application 
shinyApp(ui = ui, server = server)


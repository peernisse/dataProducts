#Shinygadget examples
library(shiny)
library(miniUI)


#1-------------------
myFirstGadget<-function(){
  
  ui<-miniPage(
    gadgetTitleBar('My First Gadget')
  )
  
  server<-function(input,output,session){
    #The done button closes the app
    observeEvent(input$done,{
      stopApp()
    })#End observeEvent
    
  }#End server
  
  runGadget(ui,server)
  
}#End function







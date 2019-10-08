library(shiny)
library(scales)
library(ggplot2)

function(input,output) {
  
  output$plot = renderPlot({
    if (input$unit_abodmen == "cm") abodmen = input$abodmen else abodmen = input$abodmen*2.54
    
    if (input$unit_weight == "lb") weight = input$weight else weight = input$weight*2.20462262
    
    if (input$unit_height == "cm") height = input$height/2.54 else height = input$height
    
    if (input$worh == "Weight") {
      p = -40.32105+0.90438*abodmen-0.13626*weight
    } else {
      p = -2.287e+01-3.903e+05/(abodmen *height)-1.328e+00*abodmen/height+7.335e+03/height
    }
    
    bodyfat.judge = function(gender,bodyfat){
      if (gender == "male"){
        if (bodyfat <= 10) return(NA) #"Extremely below normal range! Please check your input."
        if (10 < bodyfat & bodyfat <= 14) return("Essential fat")
        if (14 < bodyfat & bodyfat <= 21) return("Athletes")
        if (21 < bodyfat & bodyfat <= 25) return("Fitness")
        if (25 < bodyfat & bodyfat <= 32) return("Average")
        if (bodyfat > 31 & bodyfat <= 60) return("Obese")
        if (bodyfat > 60) return(NULL)
      }
      else {
        if (bodyfat <= 3) return(NA) #"Extremely below normal range! Please check your input."
        if (3 < bodyfat & bodyfat <= 6) return("Essential fat")
        if (6 < bodyfat & bodyfat <= 14) return("Athletes")
        if (14 < bodyfat & bodyfat <= 18) return("Fitness")
        if (18 < bodyfat & bodyfat <= 25) return("Average")
        if (bodyfat > 25 & bodyfat <= 60) return("Obese")
        if (bodyfat > 60) return(NULL)
      }
    }
    
    if (!is.null(bodyfat.judge("male", p)) && !is.na(bodyfat.judge("male", p)) )  {
      ggplot(data=data.frame(ingredient=c("fat","other"), value=c(p,100-p)), aes(x="", y=value, fill=ingredient))+
        geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+
        geom_text(aes(y = 90, label = percent(p/100)), size=5)+theme_gray()+
        labs(x = "", y = "")
    }
  })
  
  
  output$bodyfat = renderUI({
    if (input$unit_abodmen == "cm") abodmen = input$abodmen else abodmen = input$abodmen*2.54
    
    if (input$unit_weight == "lb") weight = input$weight else weight = input$weight*2.20462262
    
    if (input$unit_height == "cm") height = input$height/2.54 else height = input$height
    
    if (input$worh == "Weight") {
      p = -40.32105+0.90438*abodmen-0.13626*weight
    } else {
      p = -2.287e+01-3.903e+05/(abodmen*height)-1.328e+00*abodmen/height+7.335e+03/height
    }
    
    bodyfat.judge = function(gender,bodyfat){
      if (gender == "male"){
        if (bodyfat <= 5) return(NA) #"Extremely below normal range! Please check your input."
        if (5 < bodyfat & bodyfat <= 14) return("Essential fat")
        if (14 < bodyfat & bodyfat <= 21) return("Athletes")
        if (21 < bodyfat & bodyfat <= 25) return("Fitness")
        if (25 < bodyfat & bodyfat <= 32) return("Average")
        if (bodyfat > 31 & bodyfat <= 60) return("Obese")
        if (bodyfat > 60) return(NULL)
      }
      else {
        if (bodyfat <= 3) return(NA) #"Extremely below normal range! Please check your input."
        if (3 < bodyfat & bodyfat <= 6) return("Essential fat")
        if (6 < bodyfat & bodyfat <= 14) return("Athletes")
        if (14 < bodyfat & bodyfat <= 18) return("Fitness")
        if (18 < bodyfat & bodyfat <= 25) return("Average")
        if (bodyfat > 25 & bodyfat <= 60) return("Obese")
        if (bodyfat > 60) return(NULL)
      }
    }
    
    str1 <- paste0("You body fat percentage is: ", round(p,1), "%.")
    if (is.null(bodyfat.judge("male", p))) {
      HTML( paste("Your body fat is",round(p,1),"%.", h3("Extremely above the normal range! Please check your input.")) )
    } else {
      if (is.na(bodyfat.judge("male", p))){
        HTML( paste("Your body fat is",round(p,1),"%.",h3("Extremely below the normal range! Please check your input.")) )
      } else HTML(paste(str1, h4("You are:   "), h1(bodyfat.judge("male", p), align = "center"), sep = '<br/>'))
      
    } 
  })
  
  output$space = renderText({"---------------------------------------------------------------------------------------------"})
  output$info = renderText({ "Contact us"})
  output$contact1 = renderText({ "E-mail: yzeng58@wisc.edu"})
  output$contact2 = renderText({ "Telephone number: 608-886-6291"})
  
  output$Note = renderText({"Note: you can choose only one measurement from weight and height you know and ignore the other one. The result may be a little different."})
}
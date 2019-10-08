library(shiny)
library(ggplot2)
library(scales)

 fluidPage(
  titlePanel(h1("BodyFat Calculator")),
  sidebarPanel(h2("Your information"),
               numericInput("abodmen",h3("Circumference of abdomen"), value = 80),
               radioButtons("unit_abodmen","Unit", choices = c("cm","inch")),
               
               selectInput("worh",h3("Choose one measurement"), choices = c("Weight", "Height")),
               
               h5(textOutput("Note")),
               
               numericInput("weight",h5("Weight"), value = 70),
               radioButtons("unit_weight","Unit", choices = c("kg","lb")),
               
               numericInput("height",h5("Height"), value = 175),
               radioButtons("unit_height","Unit", choices = c("cm","inch")),
               
               submitButton()

  ),
  
  mainPanel(
    plotOutput(outputId = "plot"),
    htmlOutput(outputId = "bodyfat"),
    
    
    h3(textOutput("space")),
    h4(textOutput("info")),
    textOutput("contact1"),textOutput("contact2")
  )
  
)
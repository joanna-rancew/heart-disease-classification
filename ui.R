
library(shiny)

  shinyUI(fluidPage(
  
  titlePanel("Heart Disease Data"),
  br(),

  
  tabsetPanel(
    #---------------- prezentacja danych ----------------------#
      tabPanel("Prezentacja danych",
               
      sidebarLayout( #layout dla prezentacji danych
      
        
        sidebarPanel("Wybierz dane do analizy",
          sliderInput("jaki_wiek", "Wiek:",
                      min = 28, max = 77,
                      value = c(30,75)),
          br(),
          selectInput("jaka_plec","Plec", choices = c("Kobieta"=0,"Mezczyzna"=1),multiple=FALSE)
       
        ),
        
        
        mainPanel(
            tabsetPanel(
              
               tabPanel("Cholesterol a stan zdrowia", plotOutput("plot_density")),
               tabPanel("Tetno a stan zdrowia", 
                        plotOutput("plot_hist_chory"), 
                        plotOutput("plot_hist_zdrowy")
                        ),
               
               
               
               tabPanel("Tetno - dla mezczyzn palacych i niepalacych", selectInput("czy_pali","Wybierz do analizy", choices = c("nie pali"=0,"pali"=1),multiple=FALSE),
                        plotOutput("distPlot")) 
               )
        )  
      )
  ),
  
  #---------------- test oraz przewidywanie  ----------------------#
  tabPanel("Korelacja danych",
           plotOutput("plot_corr")
           ),
  
  
  
  
  tabPanel("Test oraz analiza danych uzytkownika",
           
           
           br(),
           textOutput("text"),
           br(),
           
           radioButtons(
             "test_sex",
             "Plec",
             inline = TRUE,
             choiceNames = c("Mezczyzna","Kobieta"),
             choiceValues = c("1","0")
           ),
           numericInput(
             "test_age",
             "Wiek",
             min=0,
             value=40
            
           ),
           radioButtons(
             "test_cp",
             "Typ bolu w klatce piersiowej",
             inline = FALSE,
             choiceNames = c("Typowa dusznosc","Nietypowa dusznosc, dlawica","Inny rodzaj bolu","Bez objawow"),
             choiceValues = c("1","2","3","4")
           ),
           radioButtons(
             "test_painloc",
             "Gdzie wystepuje bol?",
             inline = TRUE,
             choiceNames = c("Czesc podrodkowa","Inne","Nie wystepuje"),
             choiceValues = c("1","0","0")
           ),
           numericInput(
             "test_trestbps",
             "Spoczynkowe cisnienie krwi w mm Hg",
             min=70,
             value=120
             
           ),
           numericInput(
             "test_chol",
             "Poziom cholesterolu w mg/dl",
             min=60,
             value=100
             
           ),
           radioButtons(
             "test_fbs",
             "Cukier we krwi na czczo, w mg/dl",
             inline = TRUE,
             choiceNames = c("Powyzej 120 mg/dl","Ponizej 120 mg/dl"),
             choiceValues = c("1","0")
           ),
           numericInput(
             "test_thalach",
             "Maksymalne tetno podczas wysilku",
             min=55,
             value=120
             
           ),
           radioButtons(
             "test_exang",
             "Czy wystepuje dlawica lub dusznosc podczas wysilku?",
             inline = TRUE,
             choiceNames = c("Tak","Nie"),
             choiceValues = c("1","0")
           ),
           radioButtons(
             "test_painexer",
             "Czy wystepuje bol prowokowany wysilkiem?",
             inline = TRUE,
             choiceNames = c("Tak","Nie"),
             choiceValues = c("1","0")
           ),
           radioButtons(
             "test_relrest",
             "Czy  bol prowokowany wysilkiem ustepuje po odpoczynku?",
             inline = TRUE,
             choiceNames = c("Tak","Nie", "Bol nie jest prowokowany wysilkiem"),
             choiceValues = c("1","0","1")
           ),
           
           actionButton("button","Przeslij")
           
           
           
           )
  
  
  )
))
  


shinyUI(fluidPage(theme = shinytheme("cyborg"),
       navbarPage("Analiza plač", 
        tabPanel("Histogram",
        titlePanel(title=h2("Analiza glede na povprečno urno postavko", align="center")),
        sidebarLayout(
        sidebarPanel(
                      sliderInput(inputId = "bini",
                                  label = "Število skupin:",
                                  min = 1,
                                  max = 100,
                                  value = 10,
                                  )
                    ), #side panel
                    mainPanel(
                      plotOutput(outputId = "distPlot")
                    ) # main panel
                  )), # tab panel & sidbar layout
          tabPanel("Analiza po državah",
          titlePanel(title=h2("Analiza plač glede na državo", align="center")),
          sidebarLayout(
          sidebarPanel(
                                sliderInput(inputId = "leto",
                                            label = "Leto:",
                                            min=2006,
                                            max=2018,
                                            value=2010,
                                            sep = "",
                                            step=1),
                                br(),
                                br(),
                                selectInput(inputId = "drzava",
                                            label = "Država:",
                                            choices = c(sort(unique(tabela2$State)))
                                )
                   ),
                        mainPanel(
                                plotOutput(outputId = "distPlot2")
                                )
                   )), # 2. tab 
          tabPanel("Makroekonomski kazalci",
          sidebarLayout(
          sidebarPanel(        
                                selectInput(inputId = "drzava2",
                                            label = "Država:",
                                            choices = c(sort(unique(tabela2$State)))
                        )),
                        mainPanel(
                                plotOutput(outputId = "distPlot3"),
                                plotOutput(outputId = "distPlot4")
                        ) 
                   )),# tab panel 3
        tabPanel("ZDA kategorije",
        sidebarLayout(
        sidebarPanel(
                selectInput(inputId = "tabela",
                            label = "Atribut:",
                            choices =  c("Povprečne plače v ZDA (urna postavka)", 
                                         "Povprečne plače v ZDA (letna postavka)", 
                                         "Mediana plač v ZDA (urna postavka)", 
                                         "Mediana plač v ZDA (letna postavka)",
                                         "Zaposlenost v ZDA")
                            
                                )
                ),
                 mainPanel(
                         plotOutput(outputId = "distPlot5")
                        )
              ))
          ) # NAV
        )) #shiny in fluid         
                  
                  

     

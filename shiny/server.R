
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(nat.ha %>% filter(sredina == "mean"), aes(x=h)) +
      geom_histogram(aes(y=..density..), 
                     bins=input$bini, fill="#2BFFF2", color="black") +
      geom_density(alpha=.3, fill="#4EFF2B") +
      xlab("Urna postavka") + 
      ylab("Relativna frekvenca") +
      ggtitle("Histogram pojavljenosti povprečnih plač")
  })
  
    output$distPlot2 <- renderPlot({
      y <- st.ha %>% filter(state==input$drzava, leto==input$leto) %>% 
                    filter(sredina=="mean")
      print( ggplot(y) + 
               aes(x = leto, y = h) +
               geom_boxplot(fill="green", colour="green" , alpha=I(0.7)) +
               geom_point(size=0.2, colour="blue") +
               labs(title=paste('Analiza plač v zvezni državi', input$drzava)) +
               scale_x_continuous(name = "Leto", breaks = seq(input$leto,input$leto,1)) + 
               scale_y_continuous(name = "Urna mediana plača glede na poklic", 
                                   breaks = seq(20,120,20), limits = c(20,120)) + 
               geom_jitter(alpha=I(0.4)) 
       )
    })
    
      output$distPlot3 <- renderPlot({ 
          z1 <- tabela %>% filter(State==input$drzava2)
          z2 <- tabela %>% filter(State=="United States")
          z3 <- rbind(z1, z2)
          print( ggplot(z3) +
                   aes(x = leto, y=GDP, col=State) + 
                   geom_point(size=2) +
                   geom_line(size=1)  +
                   xlab("Leto") +
                   ylab("BDP per capita") +
                   labs(title="Primerjava BDP per capita.",  color='Zvezna država')  
          )
      })
      
      output$distPlot4 <- renderPlot ({
        w1 <- st.ha %>% filter(state==input$drzava2, sredina=="median") 
        w2 <- h_mean_c 
        names(w1) <- names(w2) 
        w3 <- rbind(w1, w2)
        print( ggplot(w3) +
                 aes(x = leto, y=h, col=state) + 
              # geom_point(size=2) +
                 xlab("Leto") +
                 ylab("Povprečna urna postavka") +
                 labs(title="Primerjava urnih postavk.", color='Zvezna država') +
                 stat_smooth(method = "lm") 
        )
      })
    
      output$distPlot5 <- renderPlot({
        if ( input$tabela=="Povprečne plače v ZDA (urna postavka)" | input$tabela=="Povprečne plače v ZDA (letna postavka)") 
        { mapdb <- st.ha %>% filter(sredina=="mean") 
        } else if ( input$tabela=="Mediana plač v ZDA (urna postavka)" | input$tabela=="Mediana plač v ZDA (letna postavka)") 
        { mapdb <- st.ha %>% filter(sredina=="median") 
        } else {  mapdb <-  mapdb <- t_e_s
        }
        if (input$tabela=="Povprečne plače v ZDA (urna postavka)" | input$tabela=="Mediana plač v ZDA (urna postavka)")
        {mapdb1 <- mapdb %>%
          drop_na(h) %>%
          group_by(state) %>% 
          summarise(povprecje= mean(h))
        a <- tmap_style("col_blind") 
        } else if (input$tabela=="Povprečne plače v ZDA (letna postavka)" | input$tabela=="Mediana plač v ZDA (letna postavka)")
        {mapdb1 <- mapdb %>%
          drop_na(a) %>%
          group_by(state) %>% 
          summarise(povprecje= mean(a))
        a <- tmap_style("natural") 
        } else { mapdb1 <- mapdb %>%
          drop_na(emp) %>%
          filter(occ_code=="00-0000") %>%
          group_by(state) %>%
          summarise(povprecje= mean(emp)) 
        a <- tmap_style("gray") 
        }  
        
        
        print (
          tm_shape(merge(zemljevid, mapdb1, by.x="STATE_NAME", by.y="state")) +
          tm_polygons("povprecje", title=input$tabela, legend.hist=TRUE) +
          tm_layout(legend.outside=TRUE) +
          a
              )
      })  
})

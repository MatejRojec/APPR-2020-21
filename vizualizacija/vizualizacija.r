# 3. faza: Vizualizacija podatkov

# GLEDE AVRAGE TOP in BOTTOM 
a_mean_08_T <- nat.ha %>% drop_na(h) %>% arrange(h) %>% filter(sredina=="mean") %>% filter(leto=="2008") %>%  tail(n = 10) 
a_mean_08_B <- nat.ha %>% drop_na(h) %>% arrange(h) %>% filter(sredina == "mean") %>% filter(leto=="2008") %>% head(n = 15)

# GRAF, KI ANALIZIRA ZAPOSLITEV

graf1 <- t_e %>% filter(occ_code == "00-0000") %>%
  ggplot(aes(x=leto, y=emp)) + 
  geom_line(size=1.5, colour="green") + 
  geom_point(size=2, colour="blue") +
  ylab("Število zaposlednih") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(name = "Leto", breaks = seq(2006,2018,1)) + 
  labs(title="Zaposlenost po letih") +
  stat_smooth(method = "lm") +
  theme(axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"),
        axis.text.x = element_text(face="plain", color="red", 
                                   size=10, angle=7),
        axis.text.y = element_text(face="bold", color="red", 
                                   size=10, angle=7))

# PORAZDELITEV TOP3 PLAC PO LETIH
joined <- inner_join(nat.ha, kode, by="occ_code") 
pr <- joined %>% filter(sredina=="mean") 
top <- pr[pr$occ_code %in% c("29-1067","29-1061","29-1023"), ]  %>% 
  rename(poklic=occ_title)
top$poklic = paste0('Top:', top$poklic)


graf2 <- top %>%
  ggplot(aes(x=poklic, y=h)) +
  xlab("Poklic") + 
  ylab("Povprečna urna postavka plač") + 
  geom_boxplot(fill="red", colour="red" , alpha=I(0.2)) +
  geom_jitter(alpha=I(0.2)) +
  geom_point() + 
  labs(title="Porazdelitev plač boljše plačanih poklicev") 

# TOP 3 GRAFICNO PO DREVESIH

# graf4* <- top %>%
#  ggplot(aes(x=leto,y=HM)) +
#  geom_line(color="blue") + 
#  geom_point() + 
#  facet_grid(~poklic)

# graf4* <- top %>%
#   ggplot(aes(x=leto,y=HM)) +
#   geom_line(color="blue") + 
#   geom_point() +
#   facet_grid(occ_title~.)

# TOP 3 GLEDE NA URNE POSTAVEK 

graf3 <- top %>%
  filter(leto!="2019") %>%
  ggplot(aes(x=leto, y=h, col=poklic)) +
  xlab("Leto") +
  ylab("Povprečna plača na uro") +
  labs(title="Povprečna urna plača treh najboljše plačanih poklicev.") +
  geom_line(size=1) +
  geom_point(size=2) +
  scale_colour_manual(values = c("Red4", "Violetred", "Limegreen"))

# BOTTOM 3 GLEDE NA AVRAGE WAGE 
bottom <- pr[pr$occ_code %in%  c("39-5093","35-9021","41-2011","35-3031"), ] %>% 
  rename(poklic=occ_title)
bottom$poklic = paste0('Bottom:', bottom$poklic)

graf4 <- bottom %>%
  ggplot(aes(x=leto, y=a)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("Leto") +
  ylab("Povprečna plača (letno)") +
  labs(title="Povprečna plača dveh najslabše plačanih poklicev.") +
  facet_wrap(~poklic, ncol=2) 

# Zaposlenost top 3 poklicev in bot. 2 poklicev
joined_emp <- inner_join(t_e, kode, by="occ_code") 
emoloyment <- joined_emp
bottomemp <- emoloyment[emoloyment$occ_code %in%  c("39-5093","35-9021","41-2011","35-3031"), ] %>% 
  rename(poklic=occ_title)
bottomemp$poklic = paste0('Bottom:', bottomemp$poklic)

topemp <- emoloyment[emoloyment$occ_code %in% c("29-1067","29-1061","29-1023"), ]  %>% 
  rename(poklic=occ_title)
topemp$poklic = paste0('Top:', topemp$poklic)
together <- rbind(bottomemp, topemp)

graf5 <- together %>% filter(leto=="2018") %>%
  ggplot() +
  aes(x="", y=emp, fill=poklic) + 
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  geom_col(width=1) +
  coord_polar(theta="y") + xlab("") + ylab("") +
  labs(title="Deleži poklicev (najboljše plačanih v primeravi z najmanj) v letu 2018")+
  guides(fill=guide_legend("Poklic"))

graf6 <- together %>% 
   filter(leto!="2019") %>%
   ggplot(aes(x=leto)) +
   geom_col(aes(y=emp, fill=poklic)) +
   scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
   xlab("Leto") +
   ylab("Zaposlenost") +
   labs(title="Deleži poklicev (najboljše plačanih v primeravi z najmanj)") 

# POVPREČNE PLAČE (URNE) V ZDA 

zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/states_21basic.zip", "states",
                             encoding="UTF-8") 
map1_db <- st.ha  %>%
  filter(sredina=="median") %>%
  drop_na(h) %>%
  group_by(state) %>% 
  summarise(povprecje= mean(h))

zem1 <-  tm_shape(merge(zemljevid, map1_db, by.x="STATE_NAME", by.y="state"), simplify = 0.2) +
  tm_polygons("povprecje",title="Povprečne plače v ZDA",legend.hist=TRUE,
              palette = c("springgreen","lightblue","navy")) +
  tm_layout(legend.outside=TRUE) +
  tm_text("STATE_ABBR", size=0.3) 

# GDP PP v Aljaski

graf8 <- GDP_by_state %>% filter(State=="Alaska") %>%
  ggplot(aes(x=leto, y=GDP)) + 
  geom_point() +
  geom_line()

podatki <- GDP_by_state %>% filter(State=="Alaska")

graf9 <-  ggplot(podatki, aes(x=leto, y=GDP, size =GDP)) +
  geom_point(color="Violetred") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_light() +
  labs(title = 'Leto: {floor(frame_time)}',
       x = 'Leto',
       y = 'GDP PP') +
  scale_size(name="GDP PC v Aljaski",range = c(1,15)) +
  transition_time(leto) +
  shadow_wake(0.2) 

animacija <- animate(graf9, renderer = gifski_renderer(), rewind=TRUE)


































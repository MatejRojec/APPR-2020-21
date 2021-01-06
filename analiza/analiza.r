# 4. faza: Analiza podatkov

# Podatke katere bomo modelirali

podatki <- GDP_by_state %>% filter(State=="Alaska")

#Prileganje podatkom

# ggpairs(leto.norm %>% select(H_MEAN, leto, leto, GDP))

# Različni modeli predikcij gibanja 

g <- ggplot(podatki, aes(x=leto, y=GDP)) + geom_point()

lin <- lm(data=podatki, leto ~ GDP)
g1 <- g +  geom_smooth(method="lm", se = FALSE) 

kv <- lm(data=podatki, leto ~ GDP + I(GDP^2))
g2 <- ggplot(podatki, aes(x=leto, y=GDP)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y ~ x+ I(x^2) ) 


mls <- loess(data=podatki, leto ~ GDP)
g3 <- g + geom_smooth(method = "loess")              


# REGRESIJA OZ. NAPOVED

quadratic <- lm(data = podatki, GDP ~ I(leto) + I(leto^2))
years <- data.frame(leto=seq(2019, 2020, 1))
prediction <- mutate(years, GDP=predict(quadratic, years))

regression <- podatki %>% ggplot(aes(x=leto, y=GDP)) + 
  geom_smooth(method='lm', fullrange=TRUE, color='red', formula=y ~ poly(x,2,raw=TRUE)) +
  geom_point(size=2, color="blue") + 
  geom_point(data=prediction %>% filter(leto >= 2019), color="green", size=3) +
  scale_x_continuous('Leto', breaks = seq(2011, 2020, 1), limits = c(2011,2020)) +
  ylab("BDP per capita") +
  labs(title = "Napoved BDP per capita.")


# METODA MODITELJEV

st2 <- st %>% filter(occ_code!="00-0000") %>% na.omit() %>% 
      filter(state!="Guam") %>%
      filter(state!="Virgin Islands") %>%
      filter(state!="Puerto Rico") %>%
      filter(state!="District of Columbia") %>%
      group_by(state) %>% 
      summarise(povprecje1 = mean(h_median), 
                povprecje2 = mean(h_mean),
                povprecje3 = mean(a_median),
                povprecje4 = mean(a_mean),
                povprecje5 = mean(emp))

data.norm <- st2 %>% .[c(2,3,4,5,6)] %>% scale()
rownames(data.norm) <- st2$state
# Velikokrat ponovimo 
k <- kmeans(data.norm, 5, nstart=1000)

# head(k$cluster, n=15)
# table(k$cluster)
# Pogled v centoride: View(k$centers)
# Vsota kvadratov odstopanj: k$tot.withinss

skupine <- data.frame(state=st2$state, skupina=factor(k$cluster))


zemljevid1 <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/states_21basic.zip", "states",
                             encoding="UTF-8") 
                          
#names(zemljevid)

zem2 <- tm_shape(merge(zemljevid1, skupine, by.x="STATE_NAME", by.y="state")) + tm_polygons("skupina", showNA = FALSE) 
            









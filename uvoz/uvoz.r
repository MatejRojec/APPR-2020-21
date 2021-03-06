# 2. faza obdelave podatkov: Uvoz

# Uvoz podatkov

dec <- grp <- c()
dec[6:9] <- grp[10:19] <- "."
dec[10:19] <- grp[6:9] <- ","

nat <- lapply(6:18, function(leto)
  sprintf("podatki/N_%02d.csv", leto) %>%
    read_delim(delim=";", na=c("", "#", "*", "**", "***"), trim_ws=TRUE,
               locale=locale(encoding="UTF-8", decimal_mark=dec[leto],
                             grouping_mark=grp[leto]),
               col_names=FALSE, skip=1) %>% # imena stolpcev se razlikujejo, zato bomo podali svoja
    transmute(leto=2000+leto, occ_code=factor(X1),
              occ_title=str_to_sentence(X2) %>% str_replace("[*]", ""),
              emp=X4, h_mean=X6, a_mean=X7, h_median=X11, a_median=X16)) %>%
  bind_rows(read_csv2("podatki/N_19.csv", na=c("", "#", "*", "**", "***")) %>%
              transmute(leto=2019, occ_code, occ_title, emp=tot_emp,
                        h_mean, a_mean, h_median, a_median)) %>%
  unique() # nekatere vrstice se ponovijo, tako da se teh znebimo

kode<- nat %>% select(occ_code, occ_title) %>%
  group_by(occ_code) %>% slice_head(n=1) 

t_e <- nat %>% select(leto,occ_code,emp)

nat.ha <- nat %>% select(occ_code, leto, h_mean, a_mean, h_median, a_median) %>%
  pivot_longer(c(-occ_code, -leto), names_to="podatek_sredina", values_to="vrednost") %>%
  separate(podatek_sredina, c("podatek", "sredina"), sep="_") %>%
  pivot_wider(names_from=podatek, values_from=vrednost) 

h_mean_c <- nat.ha %>% 
  mutate(state="United States") %>% 
  .[c(1,2,6,3,4,5)]

st <- lapply(6:18, function(leto)
  sprintf("podatki/S_%02d.csv", leto) %>%
    read_delim(delim=";", na=c("", "#", "*", "**", "***"),
               locale=locale(encoding="UTF-8", decimal_mark=dec[leto],
                             grouping_mark=grp[leto])) %>%
    transmute(leto=2000+leto, state=STATE, occ_code=OCC_CODE, emp=TOT_EMP,
              h_mean=H_MEAN, a_mean=A_MEAN, h_median=H_MEDIAN, a_median=A_MEDIAN)) %>%
  bind_rows(read_csv2("podatki/S_19.csv", na=c("", "#", "*", "**", "***")) %>%
              transmute(leto=2019, state=area_title, occ_code, emp=tot_emp,
                        h_mean, a_mean, h_median, a_median)) %>% unique()


t_e_s <- st %>% select(leto, state, occ_code, emp)

st.ha <- st %>% select(occ_code, leto, state, h_mean, a_mean, h_median, a_median) %>%
  pivot_longer(c(-occ_code, -leto, -state), names_to="podatek_sredina", values_to="vrednost") %>%
  separate(podatek_sredina, c("podatek", "sredina"), sep="_") %>%
  pivot_wider(names_from=podatek, values_from=vrednost)

# Tabela iz wikipedije: 

link <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP_per_capita"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table(dec=",") 
tabela[[1]] <- parse_number(tabela[[1]], na="—") 
tabela2 <- tabela  %>% drop_na(Rank)
tabela[[1]] <- NULL
tabela <- tabela %>% pivot_longer(2:9, names_to = "leto") %>% rename(GDP=value)
tabela[[2]] <- parse_number(tabela[[2]])
GDP_by_state <- tabela


#   SLABŠI UVOZ 
#uvozi <- function(ime_datoteke){
#  ime <- paste0("podatki/", ime_datoteke, ".csv")
#  tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
#    select(1:2, 4,6,7,11,16) 
#  return(tabela)
#}
# nat6 <- uvozi("N_06")
# 
# nat7 <- uvozi("N_07")
# 
# nat8 <- uvozi("N_08")
# 
# nat9 <- uvozi("N_09")
# 
# nat10 <- uvozi("N_10")
# 
# nat11 <- uvozi("N_11")
# 
# nat12 <- uvozi("N_12")
# 
# nat13 <- uvozi("N_13")
# 
# nat14 <- uvozi("N_14")
# 
# nat15 <- uvozi("N_15")
# 
# nat16 <- uvozi("N_16")
# 
# nat17 <- uvozi("N_17")
# 
# nat18 <- uvozi("N_18")



# total employment
#      
#      employment1 <- function(tabela,leto){
#        total_employment <- tabela %>% 
#          select(occ_title,tot_emp) %>% 
#          rename(emp=tot_emp) %>% 
#          mutate(leto=c(leto)) %>% 
#          drop_na(emp) %>%
#          .[c(1,3,2)] 
#        return(total_employment)
#      }
#      
#      employment2 <- function(tabela,leto){
#        total_employment <- tabela %>% 
#          select(OCC_TITLE,TOT_EMP) %>% 
#          rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
#          mutate(leto=c(leto)) %>% 
#          drop_na(emp) %>%
#          .[c(1,3,2)] 
#        return(total_employment)
#      }
#      
#      total_employment1 <- employment1(nat6,2006)
#      
#      total_employment11 <- employment1(nat7,2007)
#      
#      total_employment2 <- employment1(nat8,2008)
#      
#      total_employment22 <- employment1(nat9,2009)
#      
#      total_employment3 <- employment2(nat10,2010)
#      
#      total_employment33 <- employment2(nat11,2011)
#      
#      total_employment4 <- employment2(nat12,2012)
#      
#      total_employment44 <- employment2(nat13,2013)
#      
#      total_employment5 <- employment2(nat14,2014)
#      
#      total_employment55 <- employment2(nat15,2015)
#      
#      total_employment6 <- employment2(nat16,2016)
#      
#      total_employment66 <- employment2(nat17,2017)
#      
#      total_employment7 <- employment2(nat18,2018)
#      
#      
#      t_e <- rbind(total_employment1, total_employment2, total_employment3, total_employment4, 
#              total_employment5, total_employment6, total_employment7, total_employment11,
#              total_employment22, total_employment33, total_employment44, total_employment55,
#              total_employment66) %>%
#              mutate(emp2=parse_number(emp, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#              arrange(emp2) %>% 
#              select(1,2,4) %>%
#              rename(emp=emp2)

#       # H_MEAN
#       
#       hmean1 <- function(tabela,leto){
#         h_mean <- tabela %>% 
#           select(occ_title,h_mean) %>% 
#           rename(HM=h_mean) %>% 
#           mutate(leto=c(leto)) %>% 
#           .[c(1,3,2)] %>%
#           drop_na(HM)
#         h_mean$HM <- as.numeric(as.character(h_mean$HM))
#         return(h_mean)
#       }
#       
#       hmean2 <- function(tabela,leto){
#         h_mean <- tabela %>% 
#           select(OCC_TITLE,H_MEAN) %>% 
#           rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
#           mutate(leto=c(leto)) %>% 
#           .[c(1,3,2)] %>%
#           drop_na(HM)
#         return(h_mean)
#       }
#       
#       
#       h_mean1 <- hmean1(nat6, 2006)
#       
#       h_mean11 <- hmean1(nat7, 2007)
#       
#       h_mean2 <- hmean1(nat8, 2008)
#       
#       h_mean22 <- hmean1(nat9, 2009)
#       
#       h_mean3 <- hmean2(nat10,2010)
#       
#       h_mean33 <- hmean2(nat11,2011)
#       
#       h_mean4 <- hmean2(nat12,2012)
#       
#       h_mean44 <- hmean2(nat13,2013)
#       
#       h_mean5 <- hmean2(nat14,2014)
#       
#       h_mean55 <- hmean2(nat15,2015)
#       
#       h_mean6 <- hmean2(nat16,2016)
#       
#       h_mean66 <- hmean2(nat17,2017)
#       
#       h_mean7 <- hmean2(nat18,2018)
#       
#       
#       h_mean <- rbind(h_mean1,h_mean2,h_mean3,h_mean4,h_mean6,h_mean7,
#                       h_mean11, h_mean22, h_mean33, h_mean44, h_mean55, h_mean66) %>%
#                     arrange(HM) 
#       
#       h_mean_c <- h_mean %>%
#         mutate(STATE="United States") %>%
#         rename(OCC_TITLE=occ_title) %>%
#     .[c(4,1,2,3)]
  
#     # A_MEAN
#     
#     amean1 <- function(tabela,leto){
#       a_mean <- tabela %>% 
#         select(occ_title,a_mean) %>% 
#         rename(AM=a_mean) %>% 
#         mutate(leto=c(leto)) %>% 
#         drop_na(AM) %>%
#         .[c(1,3,2)] 
#       return(a_mean)
#     }
#     
#     amean2 <- function(tabela,leto){
#       a_mean <- tabela %>% 
#         select(OCC_TITLE,A_MEAN) %>% 
#         rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
#         mutate(leto=c(leto)) %>% 
#         drop_na(AM) %>%
#         .[c(1,3,2)] 
#       return(a_mean)
#     }
#     
#     a_mean1 <- amean1(nat6, 2006)
#     
#     a_mean2 <- amean1(nat8, 2008)
#     
#     a_mean3 <- amean2(nat10,2010)
#     
#     a_mean4 <- amean2(nat12,2012)
#     
#     a_mean5 <- amean2(nat14,2014)
#     
#     a_mean6 <- amean2(nat16,2016)
#     
#     a_mean7 <- amean2(nat18,2018)
#     
#     a_mean <- rbind(a_mean1,a_mean2,a_mean3,a_mean4,a_mean5,a_mean6,a_mean7) %>%
#             mutate(AM2=parse_number(AM, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#             arrange(AM2) %>% 
#             select(1,2,4) %>%
#             rename(AM=AM2)
#     
# H_MEDIAN


#     hmedian1 <- function(tabela,leto){
#       h_med <- tabela %>% 
#         select(occ_title,h_median) %>% 
#         rename(HME=h_median) %>% 
#         mutate(leto=c(leto)) %>% 
#         drop_na(HME) %>%
#         .[c(1,3,2)] 
#       return(h_med)
#     }
#     
#     hmedian2 <- function(tabela,leto){
#       h_med <- tabela %>% 
#         select(OCC_TITLE,H_MEDIAN) %>% 
#         rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
#         mutate(leto=c(leto)) %>% 
#         drop_na(HME) %>%
#         .[c(1,3,2)] 
#       h_med$HME <- as.numeric(as.character(h_med$HME))
#       return(h_med)
#     }
#     
#     
#     
#     h_median1 <- hmedian1(nat6,2006)
#     
#     h_median2 <- hmedian1(nat8,2008)
#     
#     h_median3 <- hmedian2(nat10,2010)
#     
#     h_median4 <- hmedian2(nat12,2012)
#     
#     h_median5 <- hmedian2(nat14,2014)
#     
#     h_median6 <- hmedian2(nat16,2016)
#     
#     h_median7 <- hmedian2(nat18,2018)
#     
#     h_med <- h_med <- rbind(h_median1, h_median2, h_median3, h_median4, h_median5, h_median6, h_median7)    %>%
#             arrange(HME)  
#     
#    # A_MEDIAN
#    
#    
#    amedian1 <- function(tabela,leto){
#      a_med <- tabela %>% 
#        select(occ_title,a_median) %>% 
#        rename(AME=a_median) %>% 
#        mutate(leto=c(leto)) %>% 
#        drop_na(AME) %>%
#        .[c(1,3,2)] 
#      return(a_med)
#    }
#    
#    amedian2 <- function(tabela,leto){
#      a_med <- tabela %>% 
#        select(OCC_TITLE,A_MEDIAN) %>% 
#        rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
#        mutate(leto=c(leto)) %>% 
#        drop_na(AME) %>%
#        .[c(1,3,2)] 
#      return(a_med)
#    }
#    
#    a_median1 <- amedian1(nat6,2006)
#    
#    a_median2 <- amedian1(nat8,2008)
#    
#    a_median3 <- amedian2(nat10,2010)
#    
#    a_median4 <- amedian2(nat12,2012)
#    
#    a_median5 <- amedian2(nat14,2014)
#    
#    a_median6 <- amedian2(nat16,2016)
#    
#    a_median7 <- amedian2(nat18,2018)
#    
#    a_med <- rbind(a_median1, a_median2, a_median3, a_median4, a_median5, a_median6, a_median7) %>%
#              mutate(AME2=parse_number(AME, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#              arrange(AME2) %>% 
#              select(1,2,4) %>%
#              rename(AME=AME2)
#    # STATE DATA  
#        
#        uvozi_s1 <- function(ime_datoteke){
#          ime <- paste0("podatki/", ime_datoteke, ".csv")
#          tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
#            select(3,5,7,9,10,14,19) 
#          return(tabela)
#        }
#        
#        uvozi_s2 <- function(ime_datoteke){
#          ime <- paste0("podatki/", ime_datoteke, ".csv")
#          tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
#            select(3,5,7,11,12,16,21) 
#          return(tabela)
#        }
#        
#        st6 <- uvozi_s1("S_06")
#        
#        st8 <- uvozi_s1("S_08")
#        
#        st10 <- uvozi_s2("S_10")
#        
#        st12 <- uvozi_s2("S_12")
#        
#        st14 <- uvozi_s2("S_14")
#        
#        st16 <- uvozi_s2("S_16")
#        
#        st18 <- uvozi_s2("S_18")
#    
#    # total employment
#    
#    
#    employment3 <- function(tabela,leto){
#      total_employment_state <- tabela %>% 
#        select(STATE,OCC_TITLE,TOT_EMP) %>% 
#        rename(emp=TOT_EMP) %>% 
#        mutate(leto=c(leto)) %>% 
#        .[c(1,3,2,4)] %>%
#        drop_na(emp) 
#      return(total_employment_state)
#    }
#    
#    total_employment_state_1 <- employment3(st6,2006)
#    
#    total_employment_state_2 <- employment3(st8,2008)
#    
#    total_employment_state_3 <- employment3(st10,2010)
#    
#    total_employment_state_4 <- employment3(st12,2012)
#    
#    total_employment_state_5 <- employment3(st14,2014)
#    
#    total_employment_state_6 <- employment3(st16,2016)
#    
#    total_employment_state_7 <- employment3(st18,2018)
#    
#    t_e_s <- rbind(total_employment_state_1,total_employment_state_2,
#            total_employment_state_3, total_employment_state_4,total_employment_state_5,
#            total_employment_state_6,total_employment_state_7) %>%
#            mutate(emp2=parse_number(emp, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#            arrange(emp2) %>% 
#            select(1,3,4,5) %>%
#            rename(emp=emp2) 

# h. mean by state

#    hmean3 <- function(tabela,leto){
#      hmean_state <- tabela %>% 
#        select(STATE,OCC_TITLE,H_MEAN) %>% 
#        rename(HM=H_MEAN) %>% 
#        mutate(leto=c(leto)) %>% 
#        .[c(1,2,4,3)] %>%
#        drop_na(HM)
#      hmean_state$HM <- as.numeric(as.character(hmean_state$HM))
#      return(hmean_state)
#    }
#    
#    h_mean_state_1 <- hmean3(st6, 2006)
#    
#    h_mean_state_2 <- hmean3(st8, 2008)
#    
#    h_mean_state_3 <- hmean3(st10,2010)
#    
#    h_mean_state_4 <- hmean3(st12,2012)
#    
#    h_mean_state_5 <- hmean3(st14,2014)
#    
#    h_mean_state_6 <- hmean3(st16,2016)
#    
#    h_mean_state_7 <- hmean3(st18,2018)
#    
#    h_mean_s <- rbind(h_mean_state_1, h_mean_state_2,h_mean_state_3,h_mean_state_4,
#                       h_mean_state_5,h_mean_state_6,h_mean_state_7) %>% arrange(HM) 
#    
#      
#    # a. mean. by state
#    
#    amean3 <- function(tabela,leto){
#      amean_state <- tabela %>% 
#        select(STATE,OCC_TITLE,A_MEAN) %>% 
#        rename(AM=A_MEAN) %>% 
#        mutate(leto=c(leto)) %>% 
#        .[c(1,2,4,3)] %>%
#        drop_na(AM)
#      return(amean_state)
#    }
#    
#    a_mean_state_1 <- amean3(st6, 2006)
#    
#    a_mean_state_2 <- amean3(st8, 2008)
#    
#    a_mean_state_3 <- amean3(st10,2010)
#    
#    a_mean_state_4 <- amean3(st12,2012)
#    
#    a_mean_state_5 <- amean3(st14,2014)
#    
#    a_mean_state_6 <- amean3(st16,2016)
#    
#    a_mean_state_7 <- amean3(st18,2018)
#    
#    a_mea_s <- rbind(a_mean_state_1, a_mean_state_2, a_mean_state_3, a_mean_state_4, 
#                a_mean_state_5, a_mean_state_6, a_mean_state_7) %>%
#                mutate(AM2=parse_number(AM, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#                arrange(AM2) %>% 
#                select(1,2,3,5) %>%
#                rename(AM=AM2)
#    
#    # H_MEDIAN
#    
#    hmedian3 <- function(tabela,leto){
#      h_median_state <- tabela %>% 
#        select(STATE,OCC_TITLE,H_MEDIAN) %>% 
#        rename(HME=H_MEDIAN) %>% 
#        mutate(leto=c(leto)) %>% 
#        drop_na(HME) %>%
#        .[c(1,2,4,3)] %>%
#        drop_na(HME)
#      h_median_state$HME <- as.numeric(as.character(h_median_state$HME))
#      return(h_median_state)
#    }
#    
#    
#    h_median_state_1 <- hmedian3(st6,2006)
#    
#    h_median_state_2 <- hmedian3(st8,2008)
#    
#    h_median_state_3 <- hmedian3(st10,2010)
#    
#    h_median_state_4 <- hmedian3(st12,2012)
#    
#    h_median_state_5 <- hmedian3(st14,2014)
#    
#    h_median_state_6 <- hmedian3(st16,2016)
#    
#    h_median_state_7 <- hmedian3(st18,2018)
#    
#    
#    ###### TU JE NEKAJ NAROBE PRI SUMMERISE JE TREBA ŠE POPRAVITI
#    
#    h_med_s <- rbind(h_median_state_1, h_median_state_2, h_median_state_3, 
#                     h_median_state_4, h_median_state_5, h_median_state_6, h_median_state_7) %>%
#      arrange(HME)
#    
#    # A_MEDIAN
#    
#    amedian3 <- function(tabela,leto){
#      a_median_state <- tabela %>% 
#        select(STATE,OCC_TITLE,A_MEDIAN) %>% 
#        rename(AME=A_MEDIAN) %>% 
#        mutate(leto=c(leto)) %>% 
#        drop_na(AME) %>%
#        .[c(1,2,4,3)] 
#      return(a_median_state)
#    }
#    
#    a_median_state_1 <- amedian3(st6,2006)
#    
#    a_median_state_2 <- amedian3(st8,2008)
#    
#    a_median_state_3 <- amedian3(st10,2010)
#    
#    a_median_state_4 <- amedian3(st12,2012)
#    
#    a_median_state_5 <- amedian3(st14,2014)
#    
#    a_median_state_6 <- amedian3(st16,2016)
#    
#    a_median_state_7 <- amedian3(st18,2018)
#    
#    a_med_s <- rbind(a_median_state_1, a_median_state_2, a_median_state_3, 
#              a_median_state_4, a_median_state_5, a_median_state_6, a_median_state_7) %>%
#              mutate(AME2=parse_number(AME, locale=locale(decimal_mark = ",",grouping_mark = ".") )) %>% 
#              arrange(AME2) %>% 
#              select(1,2,3,5) %>%
#              rename(AME=AME2)
#    
#    
#    # Tabela iz wikipedije: https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP_per_capita
#    
#    link <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP_per_capita"
#    stran <- html_session(link) %>% read_html()
#    tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#      .[[1]] %>% html_table(dec=",") 
#    tabela[[1]] <- parse_number(tabela[[1]], na="—") 
#    tabela2 <- tabela  %>% drop_na(Rank)
#    tabela[[1]] <- NULL
#    tabela <- tabela %>% pivot_longer(2:9, names_to = "leto") %>% rename(GDP=value)
#    tabela[[2]] <- parse_number(tabela[[2]])
#    GDP_by_state <- tabela


















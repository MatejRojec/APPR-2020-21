library(dplyr)
library(tidyr)
library(readr)

# read.csv("podatki/N_06.csv",sep=";", na=c("#","*")) %>% filter(h_mean=="18.84")  
# read.csv("podatki/N_06.csv",sep=";", na=c("#","*"))  %>% filter(h_mean > 18.84) 
# col.names = c("OCC koda","OCC naziv","grupa","celotna zaposlenost","5","6","7","8","9","10","11","12",
# 13","14","15","16","17","18","19"))


# NATIONAL DATA


uvozi <- function(ime_datoteke){
  ime <- paste0("podatki/", ime_datoteke, ".csv")
  tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
    select(1:2, 4,6,7,11,16) 
  return(tabela)
}


nat6 <- uvozi("N_06")

nat8 <- uvozi("N_08")

nat10 <- uvozi("N_10")

nat12 <- uvozi("N_12")

nat14 <- uvozi("N_14")

nat16 <- uvozi("N_16")

nat18 <- uvozi("N_18")

# total employment

employment1 <- function(tabela,leto){
  total_employment <- tabela %>% 
    select(occ_title,tot_emp) %>% 
    rename(emp=tot_emp) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,3,2)] 
  return(total_employment)
}

employment2 <- function(tabela,leto){
  total_employment <- tabela %>% 
    select(OCC_TITLE,TOT_EMP) %>% 
    rename(occ_title=OCC_TITLE, emp=TOT_EMP) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,3,2)] 
  return(total_employment)
}

total_employment1 <- employment1(nat6,2006)

total_employment2 <- employment1(nat8,2008)

total_employment3 <- employment2(nat10,2010)

total_employment4 <- employment2(nat12,2012)

total_employment5 <- employment2(nat14,2014)

total_employment6 <- employment2(nat16,2016)

total_employment7 <- employment2(nat18,2018)


# H_MEAN

hmean1 <- function(tabela,leto){
  h_mean <- tabela %>% 
    select(occ_title,h_mean) %>% 
    rename(HM=h_mean) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,3,2)] %>%
    drop_na(HM)
  return(total_employment)
}

hmean2 <- function(tabela,leto){
  h_mean <- tabela %>% 
    select(OCC_TITLE,H_MEAN) %>% 
    rename(occ_title=OCC_TITLE, HM=H_MEAN) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,3,2)] %>%
    drop_na(HM)
  return(total_employment)
}


h_mean1 <- hmean1(nat6, 2006)

h_mean2 <- hmean1(nat8, 2008)

h_mean3 <- hmean2(nat10,2010)

h_mean4 <- hmean2(nat12,2012)

h_mean5 <- hmean2(nat14,2014)

h_mean6 <- hmean2(nat16,2016)

h_mean7 <- hmean2(nat18,2018)


# A_MEAN

amean1 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(occ_title,a_mean) %>% 
    rename(AM=a_mean) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(AM) %>%
    .[c(1,3,2)] 
  return(a_mean)
}

amean2 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(OCC_TITLE,A_MEAN) %>% 
    rename(occ_title=OCC_TITLE, AM=A_MEAN) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(AM) %>%
    .[c(1,3,2)] 
  return(a_mean)
}

a_mean1 <- amean1(nat6, 2006)

a_mean2 <- amean1(nat8, 2008)

a_mean3 <- amean2(nat10,2010)

a_mean4 <- amean2(nat12,2012)

a_mean5 <- amean2(nat14,2014)

a_mean6 <- amean2(nat16,2016)

a_mean7 <- amean2(nat18,2018)

# H_MEDIAN


hmedian1 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(occ_title,h_median) %>% 
    rename(HME=h_median) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(HME) %>%
    .[c(1,3,2)] 
  return(a_mean)
}

hmedian2 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(OCC_TITLE,H_MEDIAN) %>% 
    rename(occ_title=OCC_TITLE, HME=H_MEDIAN) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(HME) %>%
    .[c(1,3,2)] 
  return(a_mean)
}



h_median1 <- hmedian1(nat6,2006)

h_median2 <- hmedian1(nat8,2008)

h_median3 <- hmedian2(nat10,2010)

h_median4 <- hmedian2(nat12,2012)

h_median5 <- hmedian2(nat14,2014)

h_median6 <- hmedian2(nat16,2016)

h_median7 <- hmedian2(nat18,2018)


# A_MEDIAN


amedian1 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(occ_title,a_median) %>% 
    rename(AME=a_median) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(AME) %>%
    .[c(1,3,2)] 
  return(a_mean)
}

amedian2 <- function(tabela,leto){
  a_mean <- tabela %>% 
    select(OCC_TITLE,A_MEDIAN) %>% 
    rename(occ_title=OCC_TITLE, AME=A_MEDIAN) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(AME) %>%
    .[c(1,3,2)] 
  return(a_mean)
}

a_median1 <- amedian1(nat6,2006)

a_median2 <- amedian1(nat8,2008)

a_median3 <- amedian2(nat10,2010)

a_median4 <- amedian2(nat12,2012)

a_median5 <- amedian2(nat14,2014)

a_median6 <- amedian2(nat16,2016)

a_median7 <- amedian2(nat18,2018)

# STATE DATA  

uvozi_s1 <- function(ime_datoteke){
  ime <- paste0("podatki/", ime_datoteke, ".csv")
  tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
    select(3,5,7,9,10,14,19) 
  return(tabela)
}

uvozi_s2 <- function(ime_datoteke){
  ime <- paste0("podatki/", ime_datoteke, ".csv")
  tabela <- read.csv2(ime, fileEncoding = "UTF-8" , na=c("#","*","**","","***"),) %>%
    select(3,5,7,11,12,16,21) 
  return(tabela)
}

st6 <- uvozi_s1("S_06")

st8 <- uvozi_s1("S_08")

st10 <- uvozi_s2("S_10")

st12 <- uvozi_s2("S_12")

st14 <- uvozi_s2("S_14")

st16 <- uvozi_s2("S_16")

st18 <- uvozi_s2("S_18")

# total employment


employment3 <- function(tabela,leto){
  total_employment_state <- tabela %>% 
    select(STATE,OCC_TITLE,TOT_EMP) %>% 
    rename(emp=TOT_EMP) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,3,2,4)] %>%
    drop_na(emp)
  return(total_employment_state)
}

total_employment_state_1 <- employment3(st6,2006)

total_employment_state_2 <- employment3(st8,2008)

total_employment_state_3 <- employment3(st10,2010)

total_employment_state_4 <- employment3(st12,2012)

total_employment_state_5 <- employment3(st14,2014)

total_employment_state_6 <- employment3(st16,2016)

total_employment_state_7 <- employment3(st18,2018)


# h. mean by state

hmean3 <- function(tabela,leto){
  hmean_state <- tabela %>% 
    select(STATE,OCC_TITLE,H_MEAN) %>% 
    rename(HM=H_MEAN) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,2,4,3)] %>%
    drop_na(HM)
  return(hmean_state)
}

h_mean_state_1 <- hmean3(st6, 2006)

h_mean_state_2 <- hmean3(st8, 2008)

h_mean_state_3 <- hmean3(st10,2010)

h_mean_state_4 <- hmean3(st12,2012)

h_mean_state_5 <- hmean3(st14,2014)

h_mean_state_6 <- hmean3(st16,2016)

h_mean_state_7 <- hmean3(st18,2018)

# a. mean. by state

amean3 <- function(tabela,leto){
  amean_state <- tabela %>% 
    select(STATE,OCC_TITLE,A_MEAN) %>% 
    rename(AM=A_MEAN) %>% 
    mutate(leto=c(leto)) %>% 
    .[c(1,2,4,3)] %>%
    drop_na(AM)
  return(amean_state)
}

a_mean_state_1 <- amean3(st6, 2006)

a_mean_state_2 <- amean3(st8, 2008)

a_mean_state_3 <- amean3(st10,2010)

a_mean_state_4 <- amean3(st12,2012)

a_mean_state_5 <- amean3(st14,2014)

a_mean_state_6 <- amean3(st16,2016)

a_mean_state_7 <- amean3(st18,2018)



# H_MEDIAN


hmedian3 <- function(tabela,leto){
  h_median_state <- tabela %>% 
    select(STATE,OCC_TITLE,A_MEAN,H_MEDIAN) %>% 
    rename(HME=H_MEDIAN) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(HME) %>%
    .[c(1,2,4,3)] %>%
    drop_na(HME)
  return(h_median_state)
}


h_median_state_1 <- hmedian3(st6,2006)

h_median_state_2 <- hmedian3(st8,2008)

h_median_state_3 <- hmedian3(st10,2010)

h_median_state_4 <- hmedian3(st12,2012)

h_median_state_5 <- hmedian3(st14,2014)

h_median_state_6 <- hmedian3(st16,2016)

h_median_state_7 <- hmedian3(st18,2018)


# A_MEDIAN


amedian3 <- function(tabela,leto){
  a_median_state <- tabela %>% 
    select(STATE,OCC_TITLE,A_MEAN,A_MEDIAN) %>% 
    rename(AME=A_MEDIAN) %>% 
    mutate(leto=c(leto)) %>% 
    drop_na(AME) %>%
    .[c(1,2,4,3)] 
  return(a_median_state)
}

a_median_state_1 <- amedian3(st6,2006)

a_median_state_2 <- amedian3(st8,2008)

a_median_state_3 <- amedian3(st10,2010)

a_median_state_4 <- amedian3(st12,2012)

a_median_state_5 <- amedian3(st14,2014)

a_median_state_6 <- amedian3(st16,2016)

a_median_state_7 <- amedian3(st18,2018)

# bdp per capita data

bdp_pc <- read.csv2("podatki/GDP_PC_USA.csv") %>% drop_na(USA) %>% rename(Year=1)

# bdp per capita per purchising power 

bdp_pc_ppp <- read.csv2("podatki/GDP_PC_PPP_USA.csv")  %>% drop_na(USA) 










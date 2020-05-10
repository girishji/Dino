##

options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

source(file = 'rpp.R')

pasywa %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))
aktywa %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))

rzis %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))

gv <- function(rok, tabl, row_n) {
  n <- (select(tabl, !!rok) %>% slice(row_n))[[1]]
  return(ifelse(is.na(as.numeric(n)), 0, as.numeric(n)))
}

wskaźniki_fin <- function() {
  
  wskaźniki = tribble(
    ~rok,
    '2015',
    '2016',
    '2017',
    '2018',
    '2019',
  )
  
  ratio <- function(nr, dr) {
    return(round(nr / dr, digits = 2))
  }
  
  ratio100 <- function(nr, dr) {
    return(round(nr / dr * 100, digits = 1))
  }
  
  ##  Wskaźniki płynności
  
  wskaźniki <- wskaźniki %>% 
    add_column(`1.1` = map_dbl(lata, ~ ratio(gv(.x, aktywa, 21), 
                                             gv(.x, pasywa, 23)))) %>% 
    add_column(`1.2` = map_dbl(lata, ~ ratio((gv(.x, aktywa, 24) + 
                                                gv(.x, aktywa, 13) + 
                                                gv(.x, aktywa, 35)), gv(.x, pasywa, 23))))
  
  
  ## Wskaźniki zadłużenia 
  wskaźniki <- wskaźniki %>% 
    add_column(`2.1` = map_dbl(lata, ~ ratio(gv(.x, pasywa, 10), 
                                             gv(.x, aktywa, 47)))) %>% 
    add_column(`2.2` = map_dbl(lata, ~ ratio(gv(.x, pasywa, 16), 
                                             gv(.x, aktywa, 47)))) %>% 
    add_column(`2.3` = map_dbl(lata, ~ ratio(gv(.x, rpp, 4) + gv(.x, rpp, 2), 
                                             gv(.x, pasywa, 16) + 
                                               gv(.x, pasywa, 23) - 
                                               gv(.x, aktywa, 35))))
  
  
  wskaźniki
  
  ## Wskaźniki operacyjności
  
  wskaźniki <- wskaźniki %>% 
    add_column(`3.1` = map_dbl(lata, ~ ratio(gv(.x, rzis, 5) -
                                               gv(.x, rzis, 6) -
                                               gv(.x, rzis, 9),                      
                                             gv(.x, rzis, 1))))
  
  #
  
  sredni <- function(tabl, row_n) {
    prev <- ''
    res <- c(1)
    for (rok in lata) {
      if (prev == '') {
        prev <- rok
      } else {
        av <- (gv(prev, tabl, row_n) + gv(rok, tabl, row_n)) / 2
        res <- c(res, av)
        prev <- rok
      }
    }
    return(res)
  }
  
  nr <- lata %>% map_dbl(~ gv(.x, rzis, 1))
  col_n <- nr / sredni(aktywa, 47) * 100
  wskaźniki <- wskaźniki %>% 
    add_column(`3.2` = col_n) %>% 
    mutate_at(vars(`3.2`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  #
  nr <- sredni(aktywa, 24) * 365
  dr <- lata %>% map_dbl(~ gv(.x, rzis, 1))
  wskaźniki <- wskaźniki %>% 
    add_column(`3.3` = (nr / dr)) %>% 
    mutate_at(vars(`3.3`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  #
  nr <- sredni(pasywa, 10) * 365
  dr <- lata %>% map_dbl(~ gv(.x, rzis, 1))
  wskaźniki <- wskaźniki %>% 
    add_column(`3.4` = (nr / dr)) %>% 
    mutate_at(vars(`3.4`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  #
  nr <- sredni(aktywa, 22) * 365
  dr <- lata %>% map_dbl(~ gv(.x, rzis, 1))
  wskaźniki <- wskaźniki %>% 
    add_column(`3.5` = (nr / dr)) %>% 
    mutate_at(vars(`3.5`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  
  ## Wskaźniki rentowności
  
  wskaźniki <- wskaźniki %>% 
    add_column(`4.1` = map_dbl(lata, ~ ratio100(gv(.x, rzis, 5) -
                                                  gv(.x, rzis, 6) -
                                                  gv(.x, rzis, 9),
                                                gv(.x, rzis, 1))))
  
  
  wskaźniki <- wskaźniki %>% 
    add_column(`4.2` = map_dbl(lata, ~ ratio100(gv(.x, rzis, 42),
                                                gv(.x, rzis, 1))))
  
  ## Wskaźniki rynkowe
  
  # ROA
  nr <- lata %>% map_dbl(~ gv(.x, rzis, 42))
  dr <- sredni(aktywa, 47)
  wskaźniki <- wskaźniki %>% 
    add_column(`5.1` = (nr / dr * 100)) %>% 
    mutate_at(vars(`5.1`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  # ROE
  nr <- lata %>% map_dbl(~ gv(.x, rzis, 42))
  dr <- sredni(pasywa, 1)
  wskaźniki <- wskaźniki %>% 
    add_column(`5.2` = (nr / dr * 100)) %>% 
    mutate_at(vars(`5.2`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  # ROI
  nr <- lata %>% map_dbl(~ gv(.x, rzis, 25) + gv(.x, rzis, 26))
  dr <- sredni(aktywa, 47)
  wskaźniki <- wskaźniki %>% 
    add_column(`5.3` = (nr / dr * 100)) %>% 
    mutate_at(vars(`5.3`), ~ ifelse(row_number() == 1, '-', round(.x, digits = 1)))
  
  return(wskaźniki)  
}

wskaźniki <- wskaźniki_fin()
wskaźniki

## Wskaźniki rynkowe

get_cur <- function(tabl, row_n) {
  n <- (tabl %>% select(`2019`) %>% slice(row_n))[[1]]
  return(ifelse(is.na(as.numeric(n)), 0, as.numeric(n)))
}

# P/E

share_p = 170
num_shares <- 98040000

zysk <- get_cur(rzis, 42)
PE <- share_p * num_shares / (as.numeric(zysk) * 1000)
PE

# P/BV
#bv <- 
bv <- get_cur(aktywa, 47) - get_cur(aktywa, 2) - get_cur(pasywa, 10)
P_BV <- share_p * num_shares / (as.numeric(bv) * 1000) 
bv

# DPS
# 0 (row 37 of RPP)


## Jeronimo Martins


jmf <- 'data/First9MonthsJeronimoMartinsReport2019-converted.xlsx'
jrzis <- read_excel(jmf, sheet = 12)
jbilans <- read_excel(jmf, sheet = 13)
jrpp <- read_excel(jmf, sheet = 15)

jbilans.g <- function(clm) { 
  res <- c((jbilans %>% slice(clm) %>% select(3))[[1]],
           (jbilans %>% slice(clm) %>% select(4))[[1]])
  res <- res %>% 
    map(~ ifelse(is.na(as.numeric(.x)), 0, as.numeric(.x))) %>% 
    unlist(use.names = F)
  return(res)
}

jrzis.g <- function(clm) { 
  res <- c((jrzis %>% slice(clm) %>% select(3))[[1]],
           (jrzis %>% slice(clm) %>% select(4))[[1]])
  res <- res %>% 
    map(~ ifelse(is.na(as.numeric(.x)), 0, as.numeric(.x))) %>% 
    unlist(use.names = F)
  return(res)
}



jm <- tibble('rok' = c('2019', '2018'))

#1.1
ak.ob <- jbilans.g(13) 
zo.bz <- jbilans.g(44) 
jm <- jm %>% add_column('1.1' = ak.ob / zo.bz)

pmo <- jbilans.g(16) + jbilans.g(17) + jbilans.g(18) + jbilans.g(19) 
jm <- jm %>% add_column('1.2' = pmo / zo.bz)

## 2
zo.og <- jbilans.g(44) + jbilans.g(38)
ak.og <- jbilans.g(21)
jm <- jm %>% add_column('2.1' = zo.og / ak.og)

zo.dł <- jbilans.g(38) 
jm <- jm %>% add_column('2.2' = zo.dł / ak.og)

amort <- c(528383, 269431)
zy.nt <- jrzis.g(8)
ka.wł <- jbilans.g(30)
ka.ob <- ak.og - ka.wł
za.ef <- ka.ob - pmo
jm <- jm %>% add_column('2.3' = (amort + zy.nt) / za.ef)

#3

ko.wł.sp <- jrzis.g(7) + jrzis.g(9) + jrzis.g(10) + jrzis.g(11)
pr.n.sp <- jrzis.g(6)
jm <- jm %>% add_column('3.1' =  -1 * ko.wł.sp / pr.n.sp)

jm <- jm %>% add_column('3.2' =  pr.n.sp / c(mean(ak.og), -1))
  
nal <- jbilans.g(17) + jbilans.g(16)
jm <- jm %>% add_column('3.3' =  c(mean(nal), -1) * 365 / pr.n.sp)

jm <- jm %>% add_column('3.4' =  c(mean(zo.og), -1) * 365 / pr.n.sp)

zap <- jbilans.g(14)
jm <- jm %>% add_column('3.5' =  c(mean(zap), -1) * 365 / pr.n.sp)

# rentowności

zy.nt <- jrzis.g(21) 
jm <- jm %>% add_column('4.1' =  -1 * zy.nt / ko.wł.sp)

jm <- jm %>% add_column('4.2' =  zy.nt / pr.n.sp)

#ROA
jm <- jm %>% add_column('5.1' =  100 * zy.nt / c(mean(ak.og), -1))

#ROE
jm <- jm %>% add_column('5.2' =  100 * zy.nt / c(mean(ka.wł), -1))

#ROI
jm <- jm %>% add_column('5.3' =   100 * (jrzis.g(12) + jrzis.g(13))/ c(mean(ak.og), -1))

jm <- jm %>% 
  mutate_at(vars(-1), ~ifelse(.x < 0, Inf, .x)) %>% 
  mutate_at(vars(-1), ~ifelse(is.infinite(.x), '-', round(.x, digits = 2)))


#jm %>% select(8:16)
jm <- jm %>% arrange(rok)
wskaźniki <- bind_rows(wskaźniki, jm)

wskaźniki %>% select(8:16)
  




##############################################################
## Du Pont

dp <- tribble(
  ~rok,
  '2015',
  '2016',
  '2017',
  '2018',
  '2019',
)

rzis %>% slice(42) %>% select()

map(dp$rok, ~ (rzis %>% slice(42) %>% select(.x))[[1]])



dp <- dp %>% 
  add_column(`Zysk netto` = 
               map_dbl(dp$rok, ~ gv(.x, rzis, 42))) %>% 
  add_column(`Przychody netto ze sprzedaży` = 
               map_dbl(dp$rok, ~ gv(.x, rzis, 1))) %>% 
  add_column(`Aktywa ogółem` =            
               map_dbl(dp$rok, ~ gv(.x, aktywa, 47))) %>% 
  add_column(`Zobowiązania ogółem` =            
               map_dbl(dp$rok, ~ gv(.x, pasywa, 10))) %>% 
  add_column(`Kapitał ogółem` =            
               map_dbl(dp$rok, ~ gv(.x, aktywa, 47)))

dp <- dp %>% 
  mutate(`Rentowność sprzedaży netto` =
           `Zysk netto` / `Przychody netto ze sprzedaży` * 100,
         `Rotacja aktywów` = 
           `Przychody netto ze sprzedaży` / `Aktywa ogółem`,
         `Rentowność aktywów` = 
           `Rentowność sprzedaży netto` * `Rotacja aktywów`,
         `Struktura kapitału` =
           `Zobowiązania ogółem` / `Kapitał ogółem`,
         `Rentowność kapitału własnego` =
           `Rentowność aktywów` / (1 - `Struktura kapitału`))

dp2 <- tibble(`W tys. zł.` = names(dp)[-1])
for (r in dp$rok) {
  dp2 <- dp2 %>% 
    add_column(!!r := dp %>% 
                 filter(rok == r) %>% 
                 select(-`rok`) %>% 
                 unlist(., use.names=FALSE))
}
dp2 <- dp2 %>% 
  mutate_at(vars(starts_with('20')), ~round(.x, digits = 2))

names(aktywa_st)

dp2 <- dp2 %>% 
  mutate_at(vars(2:6), ~ ifelse(row_number() %in% c(6, 8, 10), 
                                      str_c(.x, "%"), as.character(.x))) 
  

wsk2 <- tibble(
  'Wyszczególnienie' = c(
    'Wskaźnik bieżącej płynności',
    'Wskaźnik szybkiej płynności',
    'Wskaźnik podwyższonej płynności',
    'Wskaźnik poziomu zadłużenia',
    'Wskaźnik zadłużenia kapitału własnego',
    'Wskaźnik zadłużenia długoterminowego',
    'Wskaźnik operacyjności (poziomu kosztów)',
    'Wskaźnik rotacji aktywów (produktywność aktywów)',
    'Wskaźnik cyklu należności',
    'Wskaźnik cyklu zobowiązań',
    'Wskaźnik cyklu zapasów',
    'Wskaźnik rentowności sprzedaży netto',
    'Zwrot z aktywów (ROA)',
    'Zwrot z kapitału własnego (ROE)',
    'Zwrot z inwestycji (ROI)')
)
              
wsk2 <- wsk2 %>% add_column('2015' = (wskaźniki %>% slice(1) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2016' = (wskaźniki %>% slice(2) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2017' = (wskaźniki %>% slice(3) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2018' = (wskaźniki %>% slice(4) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2019' = (wskaźniki %>% slice(5) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2018_j' = (wskaźniki %>% slice(6) %>% unlist(use.names = F))[-1])
wsk2 <- wsk2 %>% add_column('2019_j' = (wskaźniki %>% slice(7) %>% unlist(use.names = F))[-1])

# add %
wsk2 <- wsk2 %>% 
  mutate_at(vars(3:6, 8), ~ ifelse(row_number() > 12, str_c(.x, '%'), as.character(.x)))
wsk2









##

options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

source(file = 'rpp.R')

pasywa %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))
aktywa %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))

rzis %>% select(c(1:3)) %>% mutate_at(1, ~str_replace(.x, '^[Ia-z1-9V-]', str_c('  ', .x)))

wskaźniki = tribble(
  ~rok,
  '2015',
  '2016',
  '2017',
  '2018',
  '2019',
)

gv <- function(rok, tabl, row_n) {
  n <- (select(tabl, !!rok) %>% slice(row_n))[[1]]
  return(ifelse(is.na(as.numeric(n)), 0, as.numeric(n)))
}

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

wskaźniki

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

dp2 <- tibble(`W tys.` = names(dp)[-1])
for (r in dp$rok) {
  dp2 <- dp2 %>% 
    add_column(!!r := dp %>% 
                 filter(rok == r) %>% 
                 select(-`rok`) %>% 
                 unlist(., use.names=FALSE))
}
dp2 <- dp2 %>% 
  mutate_at(vars(starts_with('20')), ~round(.x, digits = 2))
  

              










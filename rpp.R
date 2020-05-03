## 

# https://rlang.r-lib.org/reference/quasiquotation.html

options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

fdata <- c(
  'data/R-2016-Dino-Polska-Sprawozdanie-Finansowe-skonwertowany.xlsx',
  'data/R_2018_Dino_Polska_Sprawozdanie_Finansowe-skonwertowany.xlsx',
  'data/2019_Dino_Polska_Sprawozdanie-Finansowe-UoR--converted.xlsx'
)

# Read
rpp <- fdata %>% map(read_excel, sheet = 8)

# Clean up stray data
col_name <- 'Wyszczegónienie'

rpp[[1]] <- rpp[[1]] %>% rename(!!col_name := 1, `2015` = '...4', `2016` = '...3') %>% 
  filter(between(row_number(), 2, n() - 2)) %>% select(-'...2') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name))) %>% 
  replace_na(list(`2015` = '-', `2016` = '-'))

rpp[[2]] <- rpp[[2]] %>% rename(!!col_name := 1, `2017` = '...4', `2018` = '...3') %>% 
  filter(between(row_number(), 2, n() - 3)) %>% select(-'...2') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name))) %>% 
  replace_na(list(`2017` = '-', `2018` = '-'))

rpp[[3]] <- rpp[[3]] %>% rename(!!col_name := 1, `2018` = 4, `2019` = 3) %>% 
  filter(between(row_number(), 2, n() - 3)) %>% select(-'...2') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name)),
         `2019` = str_replace_all(`2019`, ' ', ''),
         `2018` = str_replace_all(`2018`, ' ', '')) %>% 
  replace_na(list(`2019` = '-', `2018` = '-'))

# rpp[[2]] %>% anti_join(rpp[[3]], by = col_name)
# rpp[[3]] %>% anti_join(rpp[[2]], by = col_name)
# rpp[[2]] %>% anti_join(rpp[[1]], by = col_name)
# rpp[[1]] %>% anti_join(rpp[[2]], by = col_name)

rpp <- rpp[[3]] %>% left_join(rpp[[2]] %>% select(-`2018`), by = col_name) %>% 
  left_join(rpp[[1]], by = col_name) %>% 
  replace_na(list(`2015` = '-', `2016` = '-')) %>% 
  select(`col_name`, `2015`, `2016`, `2017`, `2018`, `2019`)

rpp <- rpp %>% mutate_at(vars(`2018`, `2019`), ~ str_replace(., '\\(', '-')) %>% 
  mutate_at(vars(`2018`, `2019`), ~ str_replace(., '\\)', '')) %>% 
  mutate_at(vars(starts_with('201')), ~if_else(row_number() %in% c(1, 15, 29), '', as.character(.x))) 
  
rpp

## Dynamika

lata <- c('2015', '2016', '2017', '2018', '2019')
{
  prev <- ''
  for (rok in lata) {
    if (prev == '') {
      prev <- rok
    } else {
      cname <- str_c(rok, '/', prev)
      f <- function(tbl, num, row_num) {
        prev_val <- (select(tbl, !!prev) %>% slice(row_num))[[1]]
        prev_val <- as.integer(prev_val)
        val <- as.numeric(num)
        val <- round((val - prev_val) / prev_val * 100, digits = 1)
        return (ifelse(is.na(val), '-', val))
      }
      #aktywa <- aktywa %>% mutate(!!cname := f(aktywa, !!sym(rok), row_number()))
      rpp <- rpp %>% mutate(!!cname := f(rpp, !!sym(rok), row_number()))
      prev <- rok
    }
  }
}

rpp %>% select(!contains('/'))
rpp

## Struktura

rpp_str <- tribble(
  ~Wyszczególnienie,
  '1. Zysk netto',
  '2. Amortyzacja',
  '3. Korekty wyniku (zysku/straty)',
  '4. zmiana zapotrzebowania na kapitał obrotowy netto',
  'Przepływy pieniężne netto z działalności operacyjnej',
)

rpp_str

for (rok in c('2017', '2018', '2019')) {
  zn <- (select(rpp, !!rok) %>% slice(2))[[1]]
  am <- (select(rpp, !!rok) %>% slice(4))[[1]]
  f <- function(numstr) {
    return(ifelse(is.na(as.numeric(numstr)), 0, as.numeric(numstr)))
  }
  kw <- f((select(rpp, !!rok) %>% slice(6))[[1]]) +
    f((select(rpp, !!rok) %>% slice(7))[[1]]) +
    f((select(rpp, !!rok) %>% slice(8))[[1]]) +
    f((select(rpp, !!rok) %>% slice(13))[[1]])
  kw <- round(kw, digits = 1)
  pp <- (select(rpp, !!rok) %>% slice(14))[[1]]
  zz <- f(zn) + f(am) - f(pp) + kw
  zz <- round(zz, digits = 1)
  
  rpp_str <- rpp_str %>% 
    add_column(!!rok := c(zn, am, kw, zz, pp))
}

rpp_str

for (rok in c('2017', '2018', '2019')) {
  n_name <- str_c(rok, '_st')
  f <- function(tabl, num) {
    dr <- (select(tabl, !!rok) %>% slice(n()))[[1]]
    dr <- as.numeric(dr)
    return(ifelse(is.na(as.numeric(num)), '-', 
                  round((as.numeric(num) / as.numeric(dr) * 100), digits = 1)))
  }
  rpp_str <- rpp_str %>% mutate(!!n_name := f(rpp_str, !!sym(rok)))
}

rpp_str


## Wsaźniki

wskaźniki = tribble(
  ~rok,
  '2015',
  '2016',
  '2017',
  '2018',
  '2019',
)

# 1. Wskaźniki struktury przepływów pieniężnych

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(2))[[1]]
  dr = (select(tabl, !!rok) %>% slice(14))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`1.1` = (lata %>% map_dbl(f, rpp)))

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(4))[[1]]
  dr = (select(tabl, !!rok) %>% slice(14))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`1.2` = (lata %>% map_dbl(f, rpp)))

wskaźniki

# 2. Wskaźniki wystarczalności środków pieniężnych 


# 3. Wskaźniki wydajności pieniężnej


  
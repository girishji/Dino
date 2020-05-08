## 

#options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

fdata <- c(
  'data/R-2016-Dino-Polska-Sprawozdanie-Finansowe-skonwertowany.xlsx',
  'data/R_2017_Dino_Polska_Sprawozdanie_Finansowe-converted.xlsx',
  'data/R_2018_Dino_Polska_Sprawozdanie_Finansowe-skonwertowany.xlsx',
  'data/2019_Dino_Polska_Sprawozdanie-Finansowe-UoR--converted.xlsx'
)

# Read RZiS
rzis = fdata %>% map(read_excel, sheet = 6)

# Clean up stray data

col_name <- 'Wyszczegónienie'

rzis[[1]] <- rzis[[1]] %>% rename(!!col_name := 1, `2015` = '...4', `2016` = '...3') %>%
  select(-'...2') %>% 
  slice(-1L) 

rzis[[2]] <- rzis[[2]] %>% rename(!!col_name := 1, `2016` = '...4', `2017` = '...3') %>%
  select(-'...2') %>% filter(between(row_number(), 3, n()))

rzis[[3]] <- rzis[[3]] %>% rename(!!col_name := 1, `2017` = '...4', `2018` = '...3') %>%
  select(-'...2') %>% filter(between(row_number(), 2, n() - 1)) 

rzis[[4]] <- rzis[[4]] %>% rename(!!col_name := 1, '2018' = '...4', '2019' = '...3') %>% 
  select(-c('...2', '...5', '...6')) %>% slice(-1L) %>% 
  mutate_all(funs(str_replace_all(., '\r|\n', ''))) %>% 
  mutate(`2019` = str_replace_all(`2019`, ' ', ''),
         `2018` = str_replace_all(`2018`, ' ', ''))

# combine
rzis <- rzis[[4]] %>% 
  left_join(rzis[[3]] %>% select(-`2018`), by = col_name) %>% 
  left_join(rzis[[1]], by = col_name) %>% 
  replace_na(list(`2015` = '-', `2016` = '-')) %>% 
  select(`col_name`, `2015`, `2016`, `2017`, `2018`, `2019`)

# replace '-' signs in 2015, 2016
rzis <- rzis %>% 
  mutate_at(vars(`2015`, `2016`),
            ~ if_else(str_starts(.x, '-') & !str_ends(.x, '-'), 
                      str_replace(.x, '-', ''), as.character(.x)))
            
rzis %>% 
  mutate_at(vars(1), ~ ifelse(str_starts(.x, 'I|V'), str_c('  ', .x), .x)) %>% 
  mutate_at(vars(1), ~ ifelse(str_starts(.x, '\\-'), str_c('    ', .x), .x))

rzis

## Skorygowane

inflacya <- tribble(
  ~rok, ~val,
  '2016', -0.005,
  '2017', 0.019,
  '2018', 0.018,
  '2019', 0.02,
)

for (i in 1:length(inflacya$rok)) {
  r <-  slice(inflacya, i)$rok
  v <-  slice(inflacya, i)$val
  n <- str_c(r, '_sk')
  rzis <- rzis %>% 
    mutate(!!n := ifelse(is.na(as.numeric(!!sym(r))), !!sym(r), 
                         round(as.numeric(!!sym(r)) / (1 + as.numeric(v)))))
}

## Struktura

lata <- c('2015', '2016', '2017', '2018', '2019')

rzis_st <- rzis %>% 
  select(!contains('_sk')) %>% 
  add_row(!!col_name := 'Przychody ogółem', .before = 1) %>% 
  add_row(!!col_name := 'Koszty ogółem', .before = 2)

for (rok in lata) {
  po <- rzis_st %>% slice(3, 18, 28) %>% select(rok) %>% mutate_all(~ as.numeric(.x)) %>% sum() 
  ko <- rzis_st %>% slice(7, 23, 35) %>% select(rok) %>% mutate_all(~ as.numeric(.x)) %>% sum() 
  rzis_st <- rzis_st %>% 
    mutate_at(vars(contains(rok)), ~ ifelse(row_number() == 1, po, as.character(.x)))
  rzis_st <- rzis_st %>% 
    mutate_at(vars(contains(rok)), ~ ifelse(row_number() == 2, ko, as.character(.x)))
}

rzis_st <- rzis_st %>% select(-c('2015', '2016'))
rzis_st

for (rok in (rzis_st %>% names())[-1]) {
  t <- (rzis_st %>% slice(1) %>% select(rok))[[1]]
  t <- as.numeric(t)
  n <- str_c(rok, '_st')
  rzis_st <- rzis_st %>% mutate(!!n := ifelse(is.na(as.numeric(!!sym(rok))), 
                                              as.character(!!sym(rok)),
                                              round(as.numeric(!!sym(rok)) / t * 100, digits = 1)))
}

rzis_st

## Dynamika

{
  prev <- ''
  for (rok in lata) {
    if (prev == '') {
      prev <- rok
    } else {
      cname <- str_c(prev, '/', str_trunc(rok, 2, "left", ""))
      f <- function(tbl, num, row_num) {
        prev_val <- (select(tbl, !!prev) %>% slice(row_num))[[1]]
        prev_val <- as.integer(prev_val)
        val <- as.numeric(num)
        val <- round((val - prev_val) / prev_val * 100, digits = 1)
        return (ifelse(is.na(val), '-', val))
      }
      rzis <- rzis %>% mutate(!!cname := f(rzis, !!sym(rok), row_number()))
      prev <- rok
    }
  }
}

rzis %>% select(!contains('_'))

## Wsaźniki
wskaźniki = tribble(
  ~rok,
  '2015',
  '2016',
  '2017',
  '2018',
  '2019',
)

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(25))[[1]]
  dr = (select(tabl, !!rok) %>% slice(15))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`1` = (lata %>% map_dbl(f, rzis)))

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(39))[[1]]
  dr = (select(tabl, !!rok) %>% slice(25))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`2` = (lata %>% map_dbl(f, rzis)))

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(39))[[1]]
  dr = (select(tabl, !!rok) %>% slice(15))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`3` = (lata %>% map_dbl(f, rzis)))

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(42))[[1]]
  dr = (select(tabl, !!rok) %>% slice(15))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`4` = (lata %>% map_dbl(f, rzis)))

f <- function(rok, tabl) {
  nr = (select(tabl, !!rok) %>% slice(40))[[1]]
  dr = (select(tabl, !!rok) %>% slice(39))[[1]]
  return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
}
wskaźniki <- wskaźniki %>% 
  add_column(`5` = (lata %>% map_dbl(f, rzis)))

wskaźniki


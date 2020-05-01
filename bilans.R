## 

# https://rlang.r-lib.org/reference/quasiquotation.html

#options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

fdata <- c(
  'data/R-2016-Dino-Polska-Sprawozdanie-Finansowe-skonwertowany.xlsx',
  'data/R_2018_Dino_Polska_Sprawozdanie_Finansowe-skonwertowany.xlsx',
  'data/2019_Dino_Polska_Sprawozdanie-Finansowe-UoR--converted.xlsx'
)

# Read
aktywa = fdata %>% map(read_excel, sheet = 4)
pasywa = fdata %>% map(read_excel, sheet = 5)

# Clean up stray data
col_name = 'Wyszczegónienie'

aktywa[[1]] <- aktywa[[1]] %>% 
  rename(`...1` = 1, `2015` = '...6', `2016` = '...5') %>% 
  filter(between(row_number(), 3, n())) %>% 
  select(-'...4') %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2015` = str_squish(`2015`),
         `2016` = str_squish(`2016`)) %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>%
  mutate(!!col_name := str_squish(!!sym(col_name)))

aktywa[[2]] <- aktywa[[2]] %>% 
  rename(`...1` = 1, `2017` = '...6', `2018` = '...5') %>% 
  filter(between(row_number(), 3, n() - 1)) %>% 
  select(-'...4') %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2017` = str_squish(`2017`),
         `2018` = str_squish(`2018`)) %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name)))
 
aktywa[[3]] <- aktywa[[3]] %>% 
  rename(`...1` = 1, `2018` = '...6', `2019` = '...5') %>% 
  filter(between(row_number(), 1, n() - 1)) %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2019` = ifelse(is.na(`2019`), `...4`, `2019`),
         `2019` = str_replace_all(`2019`, ' ', ''),
         `2018` = str_replace_all(`2018`, ' ', '')) %>% 
  select(-'...4') %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name)))

aktywa <- aktywa[[3]] %>% 
  left_join(aktywa[[2]] %>% select(-`2018`), by = col_name) %>% 
  left_join(aktywa[[1]], by = col_name) %>% 
  replace_na(list(`2015` = '-', `2016` = '-')) %>% 
  select(`col_name`, `2015`, `2016`, `2017`, `2018`, `2019`)

aktywa

##

pasywa[[1]] <- pasywa[[1]] %>% 
  rename(`...1` = 1, `2015` = '...6', `2016` = '...5') %>% 
  filter(between(row_number(), 3, n())) %>% 
  select(-'...4') %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2015` = str_squish(`2015`),
         `2016` = str_squish(`2016`)) %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>%
  mutate(!!col_name := str_squish(!!sym(col_name)))

pasywa[[2]] <- pasywa[[2]] %>% 
  rename(`...1` = 1, `2017` = '...6', `2018` = '...5') %>% 
  filter(between(row_number(), 3, n() - 1)) %>% 
  select(-'...4') %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2017` = str_squish(`2017`),
         `2018` = str_squish(`2018`)) %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name)))

pasywa[[3]] <- pasywa[[3]] %>% 
  rename(`...1` = 1, `2018` = '...6', `2019` = '...5') %>% 
  filter(between(row_number(), 1, n() - 1)) %>% 
  replace_na(list(`...1` = '', `...2` = '', `...3` = '')) %>% 
  mutate(`...1` = str_squish(`...1`),
         `...2` = str_squish(`...2`),
         `...3` = str_squish(`...3`),
         `2019` = ifelse(is.na(`2019`), `...4`, `2019`),
         `2019` = str_replace_all(`2019`, ' ', ''),
         `2018` = str_replace_all(`2018`, ' ', '')) %>% 
  select(-'...4') %>% 
  unite(!!col_name, `...1`, `...2`, `...3`, sep = ' ') %>% 
  mutate(!!col_name := str_squish(!!sym(col_name)))

pasywa <- pasywa[[3]] %>% 
  left_join(pasywa[[2]] %>% select(-`2018`), by = col_name) %>% 
  mutate(`2017` = ifelse(row_number() == 29, '818505', `2017`)) %>% 
  left_join(pasywa[[1]], by = col_name) %>% 
  replace_na(list(`2015` = '-', `2016` = '-')) %>% 
  select(`col_name`, `2015`, `2016`, `2017`, `2018`, `2019`)

# pasywa[[2]] %>% anti_join(pasywa[[3]], by = col_name)
# pasywa[[3]] %>% anti_join(pasywa[[2]], by = col_name)
# pasywa[[2]] %>% anti_join(pasywa[[1]], by = col_name)
# pasywa[[1]] %>% anti_join(pasywa[[2]], by = col_name)

## Skorygowane

## https://www.nbp.pl/homen.aspx?f=/en/publikacje/raport_inflacja/projekcja_inflacji_2019.html
inflacya <- tribble(
  ~rok, ~val,
  '2016', -0.005,
  '2017', 0.019,
  '2018', 0.018,
  '2019', 0.02,
)
#filter(inflacya, rok == '2018')$val

# if statement is not vectorized, 
# see https://stackoverflow.com/questions/14170778/interpreting-condition-has-length-1-warning-from-if-function

skoryguj <- 
  function(.numer, .rok, inflacya) {
    val = filter(inflacya, rok == .rok)$val
    ifelse((.numer == '-'), '-', 
            round(as.numeric(.numer) / (1 + as.numeric(val))))
  }

aktywa <- aktywa %>% 
  mutate(`2016_sk` = skoryguj(`2016`, '2016', inflacya = inflacya),
         `2017_sk` = skoryguj(`2017`, '2017', inflacya = inflacya),
         `2018_sk` = skoryguj(`2018`, '2018', inflacya = inflacya),
         `2019_sk` = skoryguj(`2019`, '2019', inflacya = inflacya))

pasywa <- pasywa %>% 
  mutate(`2018` = str_replace(`2018`, '\\(', '-'),
         `2018` = str_replace(`2018`, '\\)', '')) %>% 
  mutate(`2016_sk` = skoryguj(`2016`, '2016', inflacya = inflacya),
         `2017_sk` = skoryguj(`2017`, '2017', inflacya = inflacya),
         `2018_sk` = skoryguj(`2018`, '2018', inflacya = inflacya),
         `2019_sk` = skoryguj(`2019`, '2019', inflacya = inflacya))              


# Summarize

aktywa <- aktywa %>% 
  filter(row_number() %in% c(1, 2, 4:14, 18, 21, 22, 24, 35, 44, 47)) %>% 
  mutate(!!col_name := ifelse(row_number() %in% c(5:9), 
                              str_c('    ', !!sym(col_name)), !!sym(col_name)),
         !!col_name := ifelse(row_number() %in% c(4, 10, 13), 
                              str_c('  ', !!sym(col_name)), !!sym(col_name)),
         !!col_name := str_trunc(!!sym(col_name), 50))

pasywa <- pasywa %>% 
  filter(row_number() %in% c(1:3, 6:13, 16, 19, 23, 24, 28, 29, 40:42, 45)) %>% 
  mutate(!!col_name := ifelse(row_number() %in% c(10, 11, 13, 15:17, 19:20), 
                              str_c('    ', !!sym(col_name)), !!sym(col_name)),
         !!col_name := ifelse(row_number() %in% c(2:7, 9, 12, 14, 18), 
                              str_c('  ', !!sym(col_name)), !!sym(col_name)),
         !!col_name := str_trunc(!!sym(col_name), 50))

## Struktura

for (rok in c('2015', '2016', '2017', '2018', '2019')) {
  n_name = str_c(rok, '_t')
  dr = (select(aktywa, !!rok) %>% slice(n()))[[1]]
  dr = as.numeric(dr)
  f <- function(num, dr) {
    ifelse((num == '-'), '-', 
           round((as.numeric(num) / as.numeric(dr) * 100), digits = 1))
  }
  aktywa <- aktywa %>% mutate(!!n_name := f(!!sym(rok), dr))
}

aktywa %>% select(11:15)

for (rok in c('2015', '2016', '2017', '2018', '2019')) {
  n_name = str_c(rok, '_t')
  dr = (select(pasywa, !!rok) %>% slice(n()))[[1]]
  dr = as.numeric(dr)
  f <- function(num, dr) {
    ifelse((num == '-'), '-', 
           round((as.numeric(num) / as.numeric(dr) * 100), digits = 1))
  }
  pasywa <- pasywa %>% mutate(!!n_name := f(!!sym(rok), dr))
}

pasywa %>% select(11:15)

## Struktura wewnętrzna






         
  
## 

# https://rlang.r-lib.org/reference/quasiquotation.html

#
options(tibble.print_min = Inf) 

library('tidyverse')
library('readxl')

fdata <- c(
  'data/R-2016-Dino-Polska-Sprawozdanie-Finansowe-skonwertowany.xlsx',
  'data/R_2018_Dino_Polska_Sprawozdanie_Finansowe-skonwertowany.xlsx',
  'data/2019_Dino_Polska_Sprawozdanie-Finansowe-UoR--converted.xlsx'
)

# Read
aktywa <- fdata %>% map(read_excel, sheet = 4)
pasywa <- fdata %>% map(read_excel, sheet = 5)

# Clean up stray data
col_name <- 'Wyszczegónienie'

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

lata <- c('2015', '2016', '2017', '2018', '2019')

for (rok in lata) {
  n_name <- str_c(rok, '_st')
  f <- function(tabl, num) {
    dr <- (select(tabl, !!rok) %>% slice(n()))[[1]]
    dr <- as.numeric(dr)
    return(ifelse(is.na(as.numeric(num)), '-', 
           round((as.numeric(num) / as.numeric(dr) * 100), digits = 1)))
  }
  aktywa <- aktywa %>% mutate(!!n_name := f(aktywa, !!sym(rok)))
  pasywa <- pasywa %>% mutate(!!n_name := f(pasywa, !!sym(rok)))
}

aktywa %>% select(!ends_with('_sk'))
pasywa %>% select(!ends_with('_sk'))

## Struktura wewnętrzna

for (rok in lata) {
  rok_n <- str_c(rok, '_sw')
  f <- function(num, dr) {
    dr <- as.numeric(dr)
    return (ifelse((num == '-'), '-',
                   round((as.numeric(num) / as.numeric(dr) * 100), digits = 1)))
  }
  dr1 <- (select(aktywa, !!rok) %>% slice(1))[[1]]
  dr2 <- (select(aktywa, !!rok) %>% slice(15))[[1]]
  aktywa <- aktywa %>% 
    mutate(!!rok_n := ifelse(row_number() < 15, f(!!sym(rok), dr1), f(!!sym(rok), dr2)),
           !!rok_n := ifelse(row_number() == 20, '-', !!sym(rok_n)))
  
  dr1 <- (select(pasywa, !!rok) %>% slice(1))[[1]]
  dr2 <- (select(pasywa, !!rok) %>% slice(8))[[1]]
  pasywa <- pasywa %>% 
    mutate(!!rok_n := ifelse(row_number() < 8, f(!!sym(rok), dr1), f(!!sym(rok), dr2)),
           !!rok_n := ifelse(row_number() == 21, '-', !!sym(rok_n)))
}
aktywa %>% select(!matches('_sk|_st'))
pasywa %>% select(!matches('_sk|_st'))

## Dynamika

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
      pasywa <- pasywa %>% mutate(!!cname := f(pasywa, !!sym(rok), row_number()))
      prev <- rok
    }
  }
}
aktywa %>% select(contains('/'))
pasywa %>% select(contains('/'))

## Wsaźniki

wskażniki <- tribble(
  ~wskaźnik,
  'Wskaźnik pokrycia aktywów trwałych kapitałem własnym (kap. wł. ÷ ak.tr.)',
  'Wskaźnik pokrycia aktywów trwałych kapitałem stałym (kap. st. ÷ ak. tr.)',
  'Wskaźnik pokrycia aktywów obrotowych zobowiązianimi trótkoterminowymi (zob. kr ÷ ak. ob.)',
  'Udział kapitału obrotowego netto w finansowaniu aktywów ogółem (kap. ob. n. ÷ ak. og.)',
  'Udział kapitału obrotowego netto w finansowaniu aktywów obrotowych przedsiębiorstwa (kap. ob. n. ÷ ak. ob.)',
)
  
for (rok in lata) {
  new_col <- c()
  f <- function(nr, dr) {
    return(round(as.numeric(nr) / as.numeric(dr) * 100, digits = 1))
  }
  kap_wł <- (select(pasywa, !!rok) %>% slice(1))[[1]]
  akt_tr <- (select(aktywa, !!rok) %>% slice(1))[[1]]
  new_col <- c(new_col, f(kap_wł, akt_tr))
  zob_dł <- (select(pasywa, !!rok) %>% slice(12))[[1]]
  kap_st <- as.integer(kap_wł) + as.integer(zob_dł)
  new_col <- c(new_col, f(kap_st, akt_tr))
  
  zob_kr <- (select(pasywa, !!rok) %>% slice(14))[[1]]
  akt_ob <- (select(aktywa, !!rok) %>% slice(15))[[1]]
  new_col <- c(new_col, f(zob_kr, akt_tr))
  akt_og <- (select(aktywa, !!rok) %>% slice(20))[[1]]
  maj_tr <- (select(aktywa, !!rok) %>% slice(3))[[1]]
  kap_ob_netto <- kap_st - as.integer(maj_tr)
  new_col <- c(new_col, f(kap_ob_netto, akt_og))
  new_col <- c(new_col, f(kap_ob_netto, akt_ob))
  wskażniki <- wskażniki %>% add_column(!!rok := new_col)
}

wskażniki

# pasywa %>% select(contains('Wysz'))
# select(pasywa, `2015`) %>% slice(1)
# select(pasywa, `2015`) %>% slice(12)
# pasywa 
# aktywa %>% select(contains('Wysz'))
  
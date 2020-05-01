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
rpp = fdata %>% map(read_excel, sheet = 8)

# Clean up stray data
col_name = 'Wyszczegónienie (w tys. zł.)'

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

rpp


##

pasywa <- pasywa[[3]] %>% left_join(pasywa[[2]] %>% select(-`2018`), by = col_name) %>% 
  mutate(`2017` = ifelse(row_number() == 29, '818505', `2017`)) %>% 
  left_join(pasywa[[1]], by = col_name) %>% 
  replace_na(list(`2015` = '-', `2016` = '-')) %>% 
  select(`col_name`, `2015`, `2016`, `2017`, `2018`, `2019`)



# pasywa[[2]] %>% anti_join(pasywa[[3]], by = col_name)
# pasywa[[3]] %>% anti_join(pasywa[[2]], by = col_name)
# pasywa[[2]] %>% anti_join(pasywa[[1]], by = col_name)
# pasywa[[1]] %>% anti_join(pasywa[[2]], by = col_name)



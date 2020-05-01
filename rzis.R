## 

#options(tibble.print_min = Inf) 
# default is 10
# getOption('tibble.print_min')

#view(last(rzis))
#last(rzis)[, 1] %>% print(n = Inf)
#names(rzis[[1]])
#colnames(rzis[[1]])
#rownames(rzis[[1]])
# [[]] and xx$y to get value of cell or column
#class(rzis[[1]])
#slice(first(rzis)[, 1], -1)
#first(rzis)[, 1]
#%>% 
#  replace_na(list(`2015` = '-', `2016` = '-'))

# rzis[[4]]
# 
# rzis[[4]] %>% anti_join(rzis[[1]], by = rzis_col_name)
# rzis[[1]] %>% anti_join(rzis[[4]], by = rzis_col_name)
# 

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
rzis_col_name = 'RACHUNEK ZYSKÓW I STRAT (WARIANT PORÓWNAWCZY)'
rzis[[1]] <- rzis[[1]] %>% rename(`2015` = '...4', `2016` = '...3') %>%
  select(-'...2') %>% slice(-1L) %>%
  mutate(`2015` = abs(as.integer(`2015`)), `2016` = abs(as.integer(`2016`)))
rzis[[2]] <- rzis[[2]] %>% rename(`2016` = '...4', `2017` = '...3') %>%
  select(-'...2') %>% filter(between(row_number(), 3, n())) %>%
  mutate(`2016` = abs(as.integer(`2016`)), `2017` = abs(as.integer(`2017`)))
rzis[[3]] <- rzis[[3]] %>% rename(`2017` = '...4', `2018` = '...3') %>%
  select(-'...2') %>% filter(between(row_number(), 2, n() - 1)) %>%
  mutate(`2017` = as.integer(`2017`), `2018` = as.integer(`2018`))
rzis[[4]] <- rzis[[4]] %>% rename('2018' = '...4', '2019' = '...3') %>% 
  select(-c('...2', '...5', '...6')) %>% slice(-1L) %>% 
  mutate_all(funs(str_replace_all(., '\r|\n', ''))) %>% 
  mutate(`2019` = str_replace_all(`2019`, ' ', ''),
         `2018` = str_replace_all(`2018`, ' ', ''),
         `2018` = as.integer(`2018`), `2019` = as.integer(`2019`))

# Combine
# 2017 numbers underwent massive correction in report of 2018
rzis_razem <- rzis[[4]] %>% left_join(rzis[[3]] %>% select(-`2018`), by = rzis_col_name) %>% 
  left_join(rzis[[1]], by = rzis_col_name)

rzis_s <- rzis_razem %>% filter(row_number() %in% c(1, 5, 15, 16, 21, 25, 26, 33, 39:42)) %>% 
  replace(is.na(.), 0)

# Struktura
rzis_s <- rzis_s %>% add_row(`RACHUNEK ZYSKÓW I STRAT (WARIANT PORÓWNAWCZY)` = '   Przychody ogółem', .before = 1) %>% 
  add_row(`RACHUNEK ZYSKÓW I STRAT (WARIANT PORÓWNAWCZY)` = '   Koszty ogółem', .before = 2)

for (yr in c('2015', '2016', '2017', '2018', '2019')) {
  rzis_s[[yr]][[1]] <- rzis_s[[yr]][[3]] + rzis_s[[yr]][[6]] + rzis_s[[yr]][[9]]
  rzis_s[[yr]][[2]] <- rzis_s[[yr]][[4]] + rzis_s[[yr]][[7]] + rzis_s[[yr]][[10]]
}

rzis_s

rzis_s <- rzis_s %>% mutate(`2019_s` = `2019` / rzis_s[['2019']][[1]] * 100,
                            `2018_s` = `2018` / rzis_s[['2018']][[1]] * 100,
                            `2017_s` = `2017` / rzis_s[['2017']][[1]] * 100,
                            `2016_s` = `2016` / rzis_s[['2016']][[1]] * 100, 
                            `2015_s` = `2015` / rzis_s[['2015']][[1]] * 100)

rzis_s

# ggplot(data = rzis_s) +
#   geom_col(mapping = aes(x = get(rzis_col_name), y = `2019_s`, fill = get(rzis_col_name))) +
#   geom_col(mapping = aes(x = get(rzis_col_name), y = `2018_s`, fill = get(rzis_col_name))) 

# Dynamika
rzis_d <- rzis_s


options(tibble.print_min = 10) 

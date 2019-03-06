library(tidyverse)
library(ltm)

B1 <- read_csv('YUB1.csv',col_names = F) %>% 
  rename(ID=X1,Name=X2,
         Q1.1 = X3, Q1.2 = X4, Q1.3 = X5, Q1.4 = X6, Q1.5 = X7,
         Q2.1 = X8, Q2.2 = X9, Q2.3 = X10,Q2.4 = X11,Q2.5 = X12,
         Q3.1 = X13,Q3.2 = X14,Q3.3 = X15,Q3.4 = X16,Q3.5 = X17,
         Q3.6 = X18,Q3.7 = X19,Q3.8 = X20,Q3.9 = X21,Q3.10= X22,
         Q4.1 = X23,Q4.2 = X24,Q4.3 = X25,Q4.4 = X26,Q4.5 = X27,
         Q5.1 = X28,Q5.2 = X29,Q5.3 = X30,Q5.4 = X31,Q5.5 = X32
         )

B1 %>% dplyr::select(starts_with('Q')) %>% rasch -> result.rasch

plot(result.rasch,type='ICC')
plot(result.rasch,type='IIC',items=0)

B2 <- read_csv('YUB2.csv',col_names = F) %>% 
  rename(ID=X1,Name=X2,
         Q21.1 = X3, Q21.2 = X4, Q21.3 = X5, Q21.4 = X6, Q21.5 = X7,
         Q21.6 = X8, Q21.7 = X9,
         Q22.1 = X10,Q22.2 = X11,Q22.3 = X12,Q22.4 = X13,Q22.5 = X14,
         Q22.6 = X15,Q22.7 = X16,Q22.8 = X17,
         Q23.1 = X18,Q23.2 = X19,Q23.3 = X20,Q23.4 = X21,Q23.5 = X22,
         Q24.1 = X23,Q24.2 = X24,Q24.3 = X25,Q24.4 = X26,Q24.5 = X27,
         Q24.6 = X28,Q24.7 = X29,Q24.8 = X30,
         Q25.1 = X31,Q25.2 = X32,
         Q26.1 = X33,Q26.2 = X34,Q26.3 = X35,Q26.4 = X36,
         Q27.1 = X37,Q27.2 = X38,Q27.3 = X39,Q27.4 = X40
  )

B2 %>% dplyr::select(starts_with('Q')) %>% rasch -> result.rasch2

plot(result.rasch2,type='ICC')
plot(result.rasch2,type='IIC',items=0)
result.rasch2$coefficients %>% data.frame %>%
  tibble::rownames_to_column() %>% arrange(beta.i)


B3 <- read_csv('YUB3.csv',col_names = F) %>% 
  rename(ID=X1,Name=X2,Q31=X3,Q32=X4,Q33=X5,Q34=X6,Q35=X7,Q36=X8,Q37=X9,Q38=X10)

B3 %>% dplyr::select(starts_with('Q')) %>% rasch -> result.rasch3

B1 %>% dplyr::full_join(B2,key='ID') %>%  full_join(B3,key=ID) -> fullDat

fullDat %>% dplyr::select(starts_with('Q')) -> resp

rasch(resp) -> model
ltm::factor.scores.rasch(model,resp.patterns = resp) -> scores

fullDat$SC <- scores$score.dat$z1
options('tibble.print_max' = 100)
fullDat %>% dplyr::select('ID','SC') %>% mutate(rate=ceiling(SC*10+80)) %>% 
  arrange(ID) %>%print()

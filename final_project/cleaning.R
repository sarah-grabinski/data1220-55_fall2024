library(janitor)
library(naniar)
library(tidyverse)

url <- 'https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2022.csv'

raw_chr <- read_csv(
  url, 
  skip = 1, 
  guess_max = 2000)

colnames(raw_chr)



clean_chr <- raw_chrf[, 
                   c(1:7, 34, 39, 44, 49, 70, 75, 85, 90, 
                     95, 100, 105, 110, 130, 140, 146, 152, 153, 173,
                     183, 188, 193, 198, 218, 
                     223, 228, 233, 238, 258, 263, 273, 
                     282, 302, 307, 312, 332, 352, 
                     372, 392, 397, 402, 407, 412, 417, 422, 442, 462, 
                     467, 472, 483, 488, 493, 503, 513, 523, 528, 553, 
                     558, 563, 568, 573, 578, 598, 619, 639, 646, 651, 
                     656, 661, 666, 671, 676, 711, 716, 721)] |>
  filter(county_ranked == 1) |>
  mutate(statecode = str_pad(statecode, 2, '0', side = 'left'),
         countycode = str_pad(countycode, 3, '0', side = 'left'),
         fipscode = str_pad(fipscode, 5, '0', side = 'left'), 
         census_region = case_when(
           statecode %in% c('09', '23', '25', '33', '44', '50', 
                            '34', '36', '42') ~ 'Northeast', 
           statecode %in% c('18', '17', '26', '39', '55', 
                            '19', '20', '27', '29', '31', 
                            '38', '46') ~ 'Midwest', 
           statecode %in% c('10', '11', '12', '13', '24', '37', 
                            '45', '51', '54', '01', '21', '28', 
                            '47', '05', '22', '40', '48') ~ 'South', 
           statecode %in% c('04', '08', '16', '35', '30', 
                            '49', '32', '56', '02', '06', 
                            '15', '41', '53') ~ 'West', 
           T ~ NA
         ), 
         census_division = case_when(
           statecode %in% 
             c('09', '23', '25', '33', '44', '50') ~ 'New England', 
           statecode %in% 
             c('34', '36', '42') ~ 'Middle Atlantic', 
           statecode %in% 
             c('18', '17', '26', '39', '55') ~ 'East North Central',
           statecode %in% 
             c('19', '20', '27', '29', '31', 
               '38', '46') ~ 'West North Central', 
           statecode %in% 
             c('10', '11', '12', '13', '24', '37', 
               '45', '51', '54') ~ 'South Atlantic', 
           statecode %in% 
             c('01', '21', '28', '47') ~ 'East South Central', 
           statecode %in%
             c('05', '22', '40', '48') ~ 'West South Central', 
           statecode %in% 
             c('04', '08', '16', '35', '30', 
               '49', '32', '56') ~ 'Mountain', 
           statecode %in% 
             c('02', '06', '15', '41', '53') ~ 'Pacific', 
           T ~ NA
         )) |>
  relocate(c(census_region, census_division), .after = 'county')

clean_chr |> 
  filter(row_number() %in% sample(1:nrow(clean_df), 750)) |>
  summarize(n = n(), 
            .by = c('census_region', 'census_division'))


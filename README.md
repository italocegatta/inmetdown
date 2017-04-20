# Pacote em R para acesso ao banco de dados das estações climaticas automaticas do INMET

## Instalação

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(xml2, rvest, dplyr, tidytext, tidyr, lubridate, stringr)
devtools::install_github("italocegatta/inmetdown")
```

## Uso

Para consultar as estações cadastradas na base de dados, utilize a função `aws_stations()`.

``` r
aws_stations()
# A tibble: 530 × 9
      id state                      city        lat       lon   alt      start status
   <chr> <chr>                     <chr>      <dbl>     <dbl> <dbl>     <date>  <chr>
1   C891    AA         Projeto Criosfera -84.000000 -79.49420  1285 2014-01-01     On
2   A108    AC           Cruzeiro do Sul  -7.610773 -72.68133   220 2015-05-14     On
3   A140    AC            Epitaciolândia -11.023752 -68.73518   225 2009-09-25     On
4   A138    AC                     Feijó  -8.142654 -70.34359   157 2009-09-24    Off
5   A137    AC      Marechal Thaumaturgo  -8.950008 -72.78678   221 2009-10-27    Off
6   A102    AC Parque Estadual Chandless  -9.358353 -69.92626   185 2008-08-05     On
7   A136    AC              Porto Walter  -8.267197 -72.74784   205 2009-10-29    Off
8   A104    AC                Rio Branco  -9.957844 -68.16517   224 2008-07-11     On
9   A353    AL                 Arapiraca  -9.804551 -36.61920   237 2008-04-27     On
10  A355    AL                  Coruripe -10.128482 -36.28635    82 2008-09-06     On
# ... with 520 more rows, and 1 more variables: url <chr>
```

A importação dos dados é feita através da função `aws_import()` e aceita a consulta de várias estações em conjunto.

``` r
aws_import(id = "A108", start =  Sys.Date(), end = Sys.Date(), small = TRUE)
# A tibble: 17 × 8
      id                data t_max t_min ur_max ur_min    rad     p
   <chr>              <dttm> <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl>
1   A108 2017-04-20 00:00:00  24.7  24.2     96     94 0.0000     0
2   A108 2017-04-20 01:00:00  24.4  24.0     96     96 0.0000     0
3   A108 2017-04-20 02:00:00  24.3  23.9     96     96 0.0000     0
4   A108 2017-04-20 03:00:00  24.3  24.1     96     95 0.0000     0
5   A108 2017-04-20 04:00:00  24.1  24.0     95     93 0.0000     0
6   A108 2017-04-20 05:00:00  24.1  23.8     95     93 0.0000     0
7   A108 2017-04-20 06:00:00  23.9  23.7     95     95 0.0000     0
8   A108 2017-04-20 07:00:00  23.7  23.3     96     95 0.0000     0
9   A108 2017-04-20 08:00:00  23.7  23.4     96     96 0.0000     0
10  A108 2017-04-20 09:00:00  23.5  23.3     96     96 0.0000     0
11  A108 2017-04-20 10:00:00  23.5  23.2     96     96 0.0000     0
12  A108 2017-04-20 11:00:00  23.3  23.2     97     96 0.0000     0
13  A108 2017-04-20 12:00:00  23.5  23.3     96     94 0.1246     0
14  A108 2017-04-20 13:00:00  23.8  23.5     94     91 0.4112     0
15  A108 2017-04-20 14:00:00  25.2  23.8     92     83 1.1000     0
16  A108 2017-04-20 15:00:00  25.8  24.9     87     79 1.3480     0
17  A108 2017-04-20 16:00:00  27.2  25.8     81     69 1.9140     0
```

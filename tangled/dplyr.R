library(dplyr)
library(data.table)
library(tidyr)

mtcars

mtcarsR <-
  mtcars %>%
  tibble::rownames_to_column('car_name') %>%
  data.table::data.table()
mtcarsR %>%
  head()

mtcarsR %>%
  dplyr::group_by(cyl, carb) %>%
  dplyr::select(mpg, hp, cyl, carb) %>%
  dplyr::summarise(meanMPG=mean(mpg, na.rm=TRUE),
                   meanCYL=mean(hp,  na.rm=TRUE),
                   .groups = 'drop')  %>%
  filter(meanMPG > 20) %>%
  data.table::data.table()

tmp <- dplyr::group_by(mtcarsR, cyl, carb)
tmp <- dplyr::select(tmp, mpg, hp, cyl, carb)
tmp <- dplyr::summarise(tmp,
                        meanMPG=mean(mpg, na.rm=TRUE),
                        meanCYL=mean(hp,  na.rm=TRUE),
                        .groups = 'drop')
tmp <- dplyr::filter(tmp, meanMPG > 20)
tmp <- data.table::data.table(tmp)
tmp

data.table::data.table(dplyr::filter(dplyr::summarise(dplyr::select(dplyr::group_by(mtcarsR,
                                                                                    cyl,
                                                                                    carb),
                                                                    mpg,
                                                                    hp,
                                                                    cyl,
                                                                    carb),
                                                      meanMPG=mean(mpg, na.rm=TRUE),
                                                      meanCYL=mean(hp,  na.rm=TRUE),
                                                      .groups = 'drop'),
                                     meanMPG > 20))

mtcarsR %>%
  dplyr::filter(cyl>4 & carb==4)

mtcarsR %>%
  dplyr::slice(15:20)

mtcarsR %>%
  dplyr::distinct(cyl, carb)

mtcarsR %>%
  dplyr::arrange(cyl, desc(gear))

mtcarsR %>%
  dplyr::select(cyl:drat) %>%
  head()

mtcarsR %>%
  dplyr::select(displacement=disp, cyl) %>%
  head()

mtcarsR %>%
  dplyr::rename(displacement=disp, weight=wt) %>%
  head()

mtcarsR %>%
  dplyr::mutate(mpc=mpg/cyl, impc=1/mpc) %>%
  head()

mtcarsR %>%
  dplyr::transmute(mpc=mpg/cyl, mpd=mpg/disp) %>%
  head()

mtcarsR %>%
  dplyr::summarize(mean_disp=mean(disp), sd_disp=sd(disp), mean_wt=mean(wt)) %>%
  data.table::data.table()

mtcarsR %>%
  dplyr::group_by(cyl) %>%
  dplyr::summarize(mean_disp_by_cyl=mean(disp), .groups = 'drop') %>%
  data.table::data.table()

mtcarsR %>%
  dplyr::group_by(cyl, gear) %>%
  dplyr::summarize(, mean_disp_by_cyl_and_gear=mean(disp), .groups = 'drop') %>%
  data.table::data.table()

mtcarsR %>%
  dplyr::group_by(cyl, gear) %>%
  dplyr::mutate(mean_disp_by_cyl_and_gear=mean(disp)) %>%
  data.table::data.table()

carSurvey <- data.table::fread(header=T, text='
               car_name, GEARS, word
              Mazda RX4,     4, ZoomZoom
          Mazda RX4 Wag,     4, ZoomZoom
     Cadillac Fleetwood,     3, RollingCouch
    Lincoln Continental,     3, RollingCouch
               Delorean,    16, TimeWarp
          Porsche 914-2,     5, SuperCar
           Lotus Europa,     5, SuperCar
           Ferrari Dino,     5, SuperCar
           Ferrari Dino,     5, SuperRedCar
             Volvo 142E,     4, BoxeyButGood
   ')
carSurvey

mtcarsR %>%
  dplyr::inner_join(carSurvey, by=c('car_name', 'gear'='GEARS'))

mtcarsR %>%
  dplyr::left_join(carSurvey, by=c('car_name', 'gear'='GEARS'))

mtcarsR %>%
  dplyr::right_join(carSurvey, by=c('car_name', 'gear'='GEARS'));

mtcarsR %>%
  dplyr::full_join(carSurvey, by=c('car_name', 'gear'='GEARS'));

mtcarsR %>%
  dplyr::semi_join(carSurvey, by=c('car_name', 'gear'='GEARS'));

mtcarsR %>%
  dplyr::inner_join(rename(carSurvey, gear=GEARS, survey_word=word), by=c('car_name', 'gear'));

longData <-
  mtcarsR %>%
  dplyr::select(gear, carb, mpg) %>%
  dplyr::group_by(gear, carb) %>%
  dplyr::summarise(mpg=mean(mpg), .groups = 'drop') %>%
  data.table::data.table()
longData

wideData <-
  longData %>%
  tidyr::pivot_wider(names_from=carb,
                     names_prefix='carb_',            ### This prepends "carb_" to the column titles created from the  carb column
                     values_from=mpg) %>%
  data.table::data.table()
wideData

wideData %>%
  tidyr::pivot_longer(cols=2:7,
                      names_to="carb",
                      names_prefix='carb_',                     ### Wack the "carb_" we added to the column names.
                      names_transform = list(carb=as.integer),  #### Previous line gets rid of the "carb_", but left a string.  Convert it to integers.
                      values_drop_na=TRUE,                      ### Get rid of the NA values we created when we made teh wide dta
                      values_to='mpg') %>%
  data.table::data.table()

library(dplyr)
mtcars
mtcarsR <- tibble::rownames_to_column(mtcars, 'car_name')
mtcarsR
dplyr::filter(mtcarsR, cyl>4 & carb==4)
dplyr::slice(mtcarsR, 15:20)
dplyr::distinct(mtcarsR, cyl, carb)
dplyr::arrange(mtcarsR, cyl, desc(gear))
dplyr::select(mtcarsR, cyl:drat)
dplyr::select(mtcarsR, displacement=disp, cyl)
dplyr::rename(mtcarsR, displacement=disp, weight=wt)
dplyr::mutate(mtcarsR, mpc=mpg/cyl, impc=1/mpc)
dplyr::transmute(mtcarsR, mpc=mpg/cyl, mpd=mpg/disp)
dplyr::summarize(mtcars, mean_disp=mean(disp), sd_disp=sd(disp), mean_wt=mean(wt))
dplyr::summarize(dplyr::group_by(mtcars, cyl), mean_disp_by_cyl=mean(disp))
dplyr::summarize(dplyr::group_by(mtcars, cyl, gear), mean_disp_by_cyl_and_gear=mean(disp))
dplyr::mutate(dplyr::group_by(mtcars, cyl, gear), mean_disp_by_cyl_and_gear=mean(disp))
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
dplyr::inner_join(mtcarsR, carSurvey, by=c('car_name', 'gear'='GEARS'));
dplyr::left_join(mtcarsR, carSurvey, by=c('car_name', 'gear'='GEARS'));
dplyr::right_join(mtcarsR, carSurvey, by=c('car_name', 'gear'='GEARS'));
dplyr::full_join(mtcarsR, carSurvey, by=c('car_name', 'gear'='GEARS'));
dplyr::semi_join(mtcarsR, carSurvey, by=c('car_name', 'gear'='GEARS'));
dplyr::inner_join(mtcarsR, rename(carSurvey, gear=GEARS, survey_word=word), by=c('car_name', 'gear'));
mtcarsR %>%
  dplyr::group_by(cyl, carb) %>%
  dplyr::select(mpg, hp, cyl, carb) %>%
  dplyr::summarise(meanMPG=mean(mpg, na.rm=TRUE),
                   meanCYL=mean(hp,  na.rm=TRUE))  %>%
  filter(meanMPG > 20)
tmp <- dplyr::group_by(mtcarsR, cyl, carb)
tmp <- dplyr::select(tmp, mpg, hp, cyl, carb)
tmp <- dplyr::summarise(tmp,
                        meanMPG=mean(mpg, na.rm=TRUE),
                        meanCYL=mean(hp,  na.rm=TRUE))
tmp <- dplyr::filter(tmp, meanMPG > 20)
tmp
dplyr::filter(dplyr::summarise(dplyr::select(dplyr::group_by(mtcarsR,
                                                             cyl,
                                                             carb),
                                             mpg,
                                             hp,
                                             cyl,
                                             carb),
                               meanMPG=mean(mpg, na.rm=TRUE),
                               meanCYL=mean(hp,  na.rm=TRUE)),
              meanMPG > 20)

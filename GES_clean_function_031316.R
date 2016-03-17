#This script was created Anastasia Vishnyakova to prepare GES NASS data
#function will loop through a list of files and produce output for each year
#Updated on 03/13/16

#set working directory
setwd("C:/Users/Anastasia/Dropbox/Statistics/GES")

RecodeGES <- function(year){
library(plyr)       
################################################################################
################################################################################
#read in data
person <- read.delim(paste("GES", year, "flat/person.txt", sep = ""))
names(person) <- tolower(names(person))
vehicle <- read.delim(paste("GES", year, "flat/vehicle.txt", sep = ""))
names(vehicle) <- tolower(names(vehicle))
accident <- read.delim(paste("GES", year, "flat/accident.txt", sep = ""))
names(accident) <- tolower(names(accident))
if (year >= 2011) drimpair <- read.delim(paste("GES", year, "flat/drimpair.txt", sep = ""))
if (year == 2010) drimpair <- read.delim(paste("GES", year, "flat/impair.txt", sep = ""))
names(drimpair) <- tolower(names(drimpair))
################################################################################
#rest_use definitions changed. Name changed in 2010
if(year == 2010) person$rest_use <- person$rest_sys #for 2010 only
if (year == 2010) person$rest_use <- mapvalues(person$rest_use,
                                             from = c(21,22, 23, 28, 30, 31, 37,38,39,40,41,42,43),
                                             to   = c(3, 2, 1, 8, 0, 7, 10, 11, 12, 4, 17, 5, 16))
if (year == 2010) person$veh_no <- person$vehno
if (year == 2010) person$per_no <- person$perno
if (year == 2010) vehicle$dr_zip <- vehicle$dzipcode
if (year == 2010) vehicle$veh_no <- vehicle$vehno
if (year == 2010) accident$veh_no <- accident$vehno
if (year == 2010) accident$veh_no <- accident$vehno
if (year == 2010) drimpair$drimpair <- drimpair$mimpair
if (year == 2010) drimpair$veh_no <- drimpair$vehno

#merge manner of collision to person file
person <- merge(person,
                subset(accident, select = c(casenum, mancol_im)),
                by = "casenum", 
                all.x = TRUE)
#for year 2010 only, merge driver's zip code to person data
if (year == 2010) person <- merge(person,
                                  subset(vehicle, select = c(casenum, veh_no, dr_zip)),
                                  by = c("casenum", "veh_no"),
                                  all.x = TRUE)
#for year 2010 only, merge special use vehicles 
if (year == 2010) person <- merge(person,
                        subset(vehicle, select = c(casenum, veh_no, spec_use)),
                        by = c("casenum", "veh_no"),
                        all.x = TRUE)
#for year 2010 only, merge rollover
if (year == 2010) person <- merge(person,
                                  subset(vehicle, select = c(casenum, veh_no, rollover)),
                                  by = c("casenum", "veh_no"),
                                  all.x = TRUE)
#for year 2010 only, merge psustrat
if (year == 2010) person <- merge(person,
                                  subset(accident, select = c(casenum,psustrat)),
                                  by = c("casenum"),
                                  all.x = TRUE)

#keep all data  - important for creating design object
#using person data, create a filter that will mark child passangers
person$filter <- 0
person$filter[person$age_im <= 4 & #age > 4, no change 2010-2014
              person$spec_use == 0 & #not a special use vehicle, no change 2010-2014
              person$seat_im != 0 & #not in a driving seat, no change 2010-2014
              person$per_typ == 2 & #not a driver, no change 2010-2014
              person$mancol_im != 0 & #collision not with vehicle in transport, no change 2010-2014
             !(person$rest_use %in% c(96,97,98,99)) & #limit restraints in vehicles, changes in definition
             person$rollover == 0] <- 1 #no rollover, no changse 2010-2014

#create a surveybase table with only survey variables
surveybase <- subset(person, select = c(casenum,
                                        stratum,
                                        per_no,
                                        pj,
                                        psu,
                                        psustrat,
                                        veh_no,
                                        weight,
                                        filter))

#create a subset of cases for children
child <- subset(person, person$age_im <= 4 & #age > 4
                        person$spec_use == 0 & #not a special use vehicle
                        person$seat_im != 0 & #not in a driving seat
                        person$per_typ == 2 & #not a driver
                        person$mancol_im != 0 & #collision not with vehicle in transport, name change in 2010
                        !(person$rest_use %in% c(96,97,98,99)) & #limit restraints in vehicles
                        person$rollover == 0,#no rollover,
                select = c(casenum,
                           veh_no,
                           age_im,
                           injsev_im,
                           per_no,
                           rest_mis,
                           rest_use,
                           seat_im,
                           sex_im)) 
################################################################################
#recode some variables for child data
#child injury - recode is compatible across time
child$injury_rec <- 1
child$injury_rec[child$injsev_im < 3] <- 0
child$injury_rec <- factor(child$injury_rec,labels = c("Not injured", "Injured"))

#sex - this is consistent from 2010 - ...
child$sex_im <- factor(child$sex_im, labels = c("Male", "Female"))

#restraint use
child$restraint[child$rest_use == 7] <- "None"
child$restraint[child$rest_use == 12] <- "Booster"
child$restraint[child$rest_use %in% c(3,2,1,8)] <- "Belt"
child$restraint[child$rest_use %in% c(10, 11, 4)]<- "Child"
child$restraint <- factor(child$restraint)
child$restraint <- relevel(child$restraint,ref = "None" )

#seat position
child$seat_im[child$seat_im >= 11 & child$seat_im <= 19] <- 1
child$seat_im[child$seat_im >= 21 & child$seat_im <= 55] <- 2
child$seat_im <- factor(child$seat_im, labels = c("Front", "Rear"))

#final subset of child variables
child <- subset(child, select = c(casenum,
                                  veh_no,
                                  per_no,
                                  age_im,
                                  restraint,
                                  sex_im,
                                  seat_im, 
                                  injury_rec
                                  ))

################################################################################
#summarize drive characteristics
driver <- subset(person, 
                 spec_use == 0 & seat_im != 0 & per_typ == 1 & 
                mancol_im != 0, 
                 select = c(casenum,
                            veh_no,
                            age_im,
                            rest_use,
                            sex_im))

#driver above/below 25 years old
driver$age_dr <- driver$age_im
driver$age_25 <- 0
driver$age_25[driver$age_im < 25] <- 1
driver$age_25 <- factor(driver$age_25, labels = c("<25", "25+"))        

#use of seat belts by driver
driver$restraint_dr[driver$rest_use == 7] <- "None"
driver$restraint_dr[driver$rest_use %in% c(3,2,1,8)] <- "Belt"
driver$restraint_dr[driver$rest_use %in% c(97,98,99)]<- "Other/Unknown"
driver$restraint_dr <- factor(driver$restraint_dr)
driver$restraint_dr <- relevel(driver$restraint_dr,ref = "None" )

#sex of dirver
driver$sex_dr <- factor(driver$sex_im, labels = c("Male", "Female"))

#subset a list of dirver variables to use in merge
driver <- subset(driver, select = c(casenum,
                                    veh_no,
                                    age_dr,
                                    age_25,
                                    sex_dr,
                                    restraint_dr))
#do a left join on casenum and veh_no
child <- merge(x = child, y = driver, by = c("casenum", "veh_no"), all.x = TRUE) 

################################################################################
#create a file of nondriver passangers except children
passenger <- subset(person, 
                 spec_use == 0 & seat_im != 0 & per_typ == 2 & 
                         mancol_im != 0 & age_im > 4, 
                 select = c(casenum 	,
                            veh_no	,
                            per_no,
                            rest_use 	,
                            sex_im,
                            seat_im,
                            age_im))

################################################################################
#did others in car wear seat belts?
passenger$restraint_p <- "Yes"
passenger$restraint_p[passenger$rest_use %in% c(0,7,97,98,99)]<- "No/Unknown"
passenger$restraint_p <- factor(passenger$restraint_p)
passenger$restraint_p <- relevel(passenger$restraint_p,ref = "No/Unknown" )

#calculate total passengers in the car who are not children
total_passengers <- subset(as.data.frame(with(passenger,table(casenum, veh_no))),
                   Freq != 0)

names(total_passengers) <- c("casenum", "veh_no",  "total_passengers")

#calculate number with seatbelt among non-children passengers
passenger_seatbelt_pct <- subset(as.data.frame(
                                   with(subset(passenger, restraint_p == "Yes"), 
                                        table(casenum, veh_no))),
                                 Freq != 0 )


names(passenger_seatbelt_pct) <-  c("casenum", "veh_no", "passenger_seatbelt")   

#merge the table of total passengers and passengers with belts
total_passengers <- merge(x = total_passengers, 
                          y = passenger_seatbelt_pct,
                          by = c("casenum", "veh_no"),
                          all.x = TRUE)

#merge the total_passengers table with child data
child <- merge(x=child, 
              y= total_passengers, 
              by = c("casenum", "veh_no"), 
              all.x = TRUE)

child$total_passengers[is.na(child$total_passengers)] <- 0
child$passenger_seatbelt[is.na(child$passenger_seatbelt)] <- 0
child$passenger_seatbelt_pct <- child$passenger_seatbelt/child$total_passengers
child$passenger_seatbelt_pct_cat[child$total_passengers == 1] <- 'All wear belts'
child$passenger_seatbelt_pct_cat[child$total_passengers != 1] <- 'Not all wear belts'
child$passenger_seatbelt_pct_cat[child$total_passengers == 0] <- 'Only child passenger'

################################################################################
#summarize selected data from the vehicle file
#recode body type
#body type, no changes 2010-2015
vehicle$body_type <- NULL
vehicle$body_type[vehicle$bdytyp_im <= 13 | vehicle$bdytyp_im == 17] <- "Passenger"
vehicle$body_type[vehicle$bdytyp_im >= 14 & vehicle$bdytyp_imp <= 19] <- "Utility"
vehicle$body_type[vehicle$bdytyp_im >= 20 & vehicle$bdytyp_im <= 29] <- "Van"
vehicle$body_type[vehicle$bdytyp_im >= 30 ] <- "Truck"

#speeding
# codes 2010 - 2014 consistent with recode below
#using indicator variable
vehicle$speed <- 0
vehicle$speed[vehicle$speedrel >= 1 & vehicle$speedrel <= 5] <- 1
vehicle$speed <- factor(vehicle$speed, labels = c("Not speeding",
                                                     "Speeding"))

#merge child and vehicle data
vehicle <- subset(vehicle, select = c(veh_no, casenum, dr_zip, body_type, speed))
child  <- merge(x=child, y= vehicle, by = c("casenum", "veh_no"), all.x = TRUE)


################################################################################
#read in driver impairment data
#select a subset of drivers under influence of alchol - value 9 
drimpair <- subset(drimpair, 
                   drimpair == 9, 
                   selec = c(casenum,
                             veh_no))
drimpair$impared_alc <- 1
#merge driver impared cases with child data

child <- merge(child, drimpair, by = c("casenum", "veh_no"), all.x = TRUE)
child$impared_alc[is.na(child$impared_alc)] <- 0
child$impared_alc <- factor(child$impared_alc, labels = c("No", "Yes"))
################################################################################
#add American Community Survey data
load("Analysis/acs.rda")
acs$dr_zip <- as.numeric(as.character(acs$geoid2))
acs$geoid2 <- NULL
acs$geoid <- NULL
acs$geolabel <- NULL
child <- merge(x=child, y=acs, by = "dr_zip", all.x = TRUE)

################################################################################
#connect child data to the surveybase data

#this is needed so that I don't have to list variables in child table
child <- merge(x = surveybase,
              y = child,
              by = c("casenum", "per_no", "veh_no"),
              all.x = TRUE)
child$surveyyear <- year
################################################################################
#save child data as an RDA file 

#this creates a new data frame
assign(paste0("child", year), child)

save(list=paste0("child", year), file = paste0("child",year, ".rda"))
}

#test function 
#lapply(2010:2014, RecodeGES)



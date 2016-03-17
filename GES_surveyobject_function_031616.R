# This is code to create a survey object and combine data for GES
# Updated on March 13, 2016 by Anastasia Vishnyakova

#set working directory 
setwd("C:/Users/Anastasia/Dropbox/Statistics/GES")

#survey design library
library(survey)

#create survey objects for a set of years
#this function takes a data files for multiple years of survey as input
#data need to be stored in directory as "dfname_year.Rda" format
#function produces two outputs: 
# (1) a dataframe with combined years of survey data that can be imported into 
#another software
# (2) a survey design object with replicate weights that can be used by survey package
CreateSurveyObjectGES <- function (year, #years of survey
                                   dfname, #name of data frame as it is stored in directory
                                   lonelypsuoption = "certainty", #pass option for lonely PSU
                                   repweightsmethod = "Jkn" #pass option for replicate weights
                                   )
{
#read in data and store in a list 
filenames <- paste0(dfname, year, ".Rda")
temp  <- lapply(filenames, function(x){get(load(file = x))})
#calculate number of variables before replicate weights are made
nvars <- ncol(temp[[1]])

options(survey.lonely.psu= lonelypsuoption)
#specify a design object and add replicate weights to the list of data frames
for (i in seq_along(year)){
        temp[[i]]$pjstrat <- 1
        design <- svydesign(id = ~ psu + pj + casenum,
                            strata  = ~ psustrat + pjstrat + stratum,
                            weights = ~ weight,
                            data = temp[[i]])
        temp_jkn  <- as.svrepdesign(design, 
                                     mse = TRUE,
                                     compress = FALSE)
        #save replicate weights in a separate dataframe
        repweights <- data.frame(unclass(temp_jkn$repweights))
        temp[[i]] <-cbind(temp[[i]], repweights)
}
#combine all data into one dataframe object
temp_rw <- do.call("rbind", temp)
#save one combined data frame with all survey years
assign(paste0(dfname, "_rw"), temp_rw)
save(list = paste0(dfname,"_rw"), file = paste0(dfname,"_rw", ".Rda"))

#create an survey design object that combines data from all dataframes
temp_design <- svrepdesign(variables = temp_rw[,1:nvars],
                            repweights = temp_rw[,(nvars+2):ncol(temp_rw)],
                            combined.weights = FALSE,
                            weights = temp_rw$weight)
assign(paste0(dfname, "_design"), temp_design)
save(list = paste0(dfname,"_design"), file = paste0(dfname,"_design", ".Rda"))
}





















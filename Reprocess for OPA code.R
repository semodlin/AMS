#setting working directory to where we have our processed data 
setwd("C:/Users/sem00033/WVUM & HSC/Barone Gibbs, Bethany - nuMoM2b Team Folder (1)/AMS")

#identifying event files exported by Erik's code 
eventfile<- list.files(pattern = "_eventfile.csv", recursive = T) 
eventfile<- eventfile[-1] #removing an an extra archived file 

#I copied files - keeping an original in their home, and a copy of all
# all participants in a new folder for this new processing 

process_dat(dat_source = "Magee Womens", subject = "4100300R",
            sleep_source = "AMSNuMoM2bHHS2_DATA_2024-03-27_1139.csv")

for(i in 1:length(eventfile)){

  temp.events <- read.csv(
    paste0("C:/Users/sem00033/WVUM & HSC/Barone Gibbs, Bethany - nuMoM2b Team Folder (1)/AMS/",
           "c. Participant Data"),
           eventfile[i])

  file.copy(
    from = 
      paste0("C:/Users/sem00033/WVUM & HSC/Barone Gibbs, Bethany - nuMoM2b Team Folder (1)/AMS/",
             "c. Participant Data"),
             eventfile[i])
    to = 
      paste0("C:/Users/sem00033/WVUM & HSC/Barone Gibbs, Bethany - nuMoM2b Team Folder (1)/AMS/",
             "Occupational Activity")
             substring(eventfile[i],nchar(eventfile[i])-21)
            
  
}

#Identifying the sec-by-sec data from Erik's processing code 
secfile<- list.files(pattern = "_1secepoch.csv", recursive = T)
secfile<- secfile[-1] #removing an archival file

#I copied files - keeping an original in their home, and a copy of all
# all participants in a new folder for this new processing 
for(i in 1:length(secfile)){
  
  temp.events <- read.csv(
    paste0("U:/ResearchData/rdss_kawhitaker/Pregnancy247/ActivPAL Processing/",
           secfile[i]))
  
  file.copy(
    from = 
      paste0("U:/ResearchData/rdss_kawhitaker/Pregnancy247/ActivPAL Processing/",
             secfile[i]),
    to = 
      paste0("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/1s epochs/",
             substring(secfile[i],nchar(secfile[i])-21)
      )
  )
}








#load in list of IDs and trimester that have been processed
IDs<- substring(list.files("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/Event Files/"), 1,7)
Trimester <- substring(list.files("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/Event Files/"), 8,8)

#setting working directory to where I will be working with my copied data 
setwd("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/")





i<- 968

#loop to process each one 
for(i in 968:length(IDs)){
#read in event and sec_by_sec data that was made when first processed 
events <- read.csv(paste0("Event Files/",
  list.files("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/Event Files/")[i]))
sec_by_sec <- read.csv(paste0("1s epochs/",
  list.files("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/1s epochs/")[i]))

#removing non-work hours, which leaves an event file and sec_by_sec that only 
# contain work hours and days 
events.work <- events[events$work_loop == 1,]
sec.work <- sec_by_sec[sec_by_sec$work_loop == 1,]

#reformat dates from character to POSIXCT 
# depending on code, Erik's newer codes use POSIXCT and older versions just 
# used characters, so this might not be needed depending on code 
# POSIXCT is a touchy function/class, so if there are  weird errors, 
# I usually check with that 
events.work$time <- as.POSIXct(events.work$time, format = "%Y-%m-%dT%H:%M:%S")
sec.work$time <- as.POSIXct(sec.work$time, format = "%Y-%m-%dT%H:%M:%S")
 

#days to work with 
days <- unique(events.work$wear_day)
  #days.sec <-unique(sec.work$wear_day)

#fooling the code to process these data frames because Erik made it look for 
#a set structure 

class(events.work) <- c("merge.data.events", "data.frame")
class(sec.work) <- c("one.epoch.data", "data.frame")

if(length(days) != 0){
  summ_daily <- tryCatch({summarize_daily(
  subject = IDs[i], trimester = Trimester[i],
  data_events = events.work, 
  data_1s_epoch = sec.work,
  all_days = days, good_days = days
  )
  }, error = function(e) {
    cat("\n Error: ", IDs[i], Trimester[i])
  })
  
}else{summ_daily$daily <- data.frame("no work days reported...")
  
}
cat("\n", IDs[i]," ", Trimester[i], " = ",ifelse(ncol(summ_daily$daily) < 2, "Check", "Good"))
write.csv(summ_daily$daily,
          file = paste0("U:/ResearchData/rdss_kawhitaker/Pregnancy247/Data/Occupational Activity/OPA Daily Summaries/",
                        IDs[i],Trimester[i],"_OPA_daily.csv")
)

}

## WORKING DIRECTORY
#setwd("your working directory")

## Accessing googlesheets
library(googlesheets4) 
gs4_auth()
## Read ICs for Exp4 on GDrive
ICs_Exp4 = 
  read_sheet(ss="https://docs.google.com/spreadsheets/d/1BVLSJDeLPvl2mo_Ors_GTreCRsDxGYUziDjrIDKwrh0/edit#gid=544141775")

# Behavioral Box Daily Checklist Form - Water1/Water2 & Food1/Food2 daily
{
  #https://docs.google.com/spreadsheets/d/1ZcAycD_tzYTNN4xpyttKNKAJNyeDC0bLP0VAGc9Ox8E/edit#gid=364190545
  # Column 1: Timestamp, Col2: Box-Mouse ID
  # Col21: Sucrose Water Location [1,2], Col24: Fatty Food Location [1,2]
  DailyChecklist = 
    read_sheet(ss="https://docs.google.com/spreadsheets/d/1ZcAycD_tzYTNN4xpyttKNKAJNyeDC0bLP0VAGc9Ox8E/edit#gid=364190545")
  #DailyChecklist <- read.csv("C:/Users/simeonem/Documents/Brendon/Deniz Behavioral Box Daily Checklist Form (Responses) - Form Responses 1.csv")
  
  DailyChecklist = as.data.frame(
    cbind(DailyChecklist[, 1],
          DailyChecklist[, 2],
          DailyChecklist[, "Sucrose Water Needle and Beambreak location at end of check  (after any swaps)  ?"],
          DailyChecklist[, "Fatty Food Pellet location at end of check (after any swaps):"],
          DailyChecklist[, "Regular Water Level upon Check [mL]"],
          DailyChecklist[, "Sucrose Water Level upon Check [mL]"]))
  
  colnames(DailyChecklist) = c("TimeStamp", 
                               "ID", 
                               "SucroseLocation", 
                               "FattyLocation",
                               "Regular Water Level", 
                               "Sucrose Water Level")
  
  # Delete the first row, because it's off
  DailyChecklist = DailyChecklist[-1, ]
  
  ## Example: DailyChecklist_lite_1$TimeStamp is already "POSIXct" "POSIXt",
  ## so no need to convert in Day and Hr anymore
  #class(DailyChecklist_lite_1$TimeStamp) # "POSIXct" "POSIXt" 
  Exp4_Initial_Day = "2022-05-10 12:00:00"
  Exp4_DailyChecklist =
    DailyChecklist[DailyChecklist$TimeStamp > Exp4_Initial_Day, ]
  for (i in 1:dim(Exp4_DailyChecklist)[1]) {
    Exp4_DailyChecklist$SucroseLocation[i] = 
      strsplit(Exp4_DailyChecklist$SucroseLocation[i], " ")[[1]][2]
    Exp4_DailyChecklist$FattyLocation[i] = 
      strsplit(Exp4_DailyChecklist$FattyLocation[i]," ")[[1]][2]
  }
}
Exp4_DailyChecklist[, 2] = as.integer(Exp4_DailyChecklist[, 2])

## Loading Entry Logs (Lab, ULAM)
{
  EntryLog_Exp4 = 
    read_sheet(ss="https://docs.google.com/spreadsheets/d/1WT8rbnkABzOSPttD6ktHF7b8dzokdTJJ1fwdtt1wFQU/edit#gid=0")
  EntryLog_Exp4$`Date In` = format(EntryLog_Exp4$`Date In`, "%Y-%m-%d")
  EntryLog_Exp4$`Date out` = format(EntryLog_Exp4$`Date out`, "%Y-%m-%d")
  EntryLog_Exp4$`Time in` =  format(EntryLog_Exp4$`Time in`, "%H:%M:%S")
  EntryLog_Exp4$`Time out` =  format(EntryLog_Exp4$`Time out`, "%H:%M:%S")
  format = "%Y-%m-%d %H:%M:%S"
  # combining date and time into single object
  EntryLog_Exp4$TimeIN =
    as.POSIXct(paste(EntryLog_Exp4$`Date In`, EntryLog_Exp4$`Time in`), format=format)
  EntryLog_Exp4$TimeOUT =
    as.POSIXct(paste(EntryLog_Exp4$`Date out`, EntryLog_Exp4$`Time out`), format=format)

  #Exp4_Days = EntryLog_Exp4$`Date In`[EntryLog_Exp4$`Date In` > Exp4_Initial_Day]
  EntryLog_Exp4 = EntryLog_Exp4[EntryLog_Exp4$`Date In` > Exp4_Initial_Day, ]
  
  EntryLog_ULAM = 
    read_sheet(ss="https://docs.google.com/spreadsheets/d/1WT8rbnkABzOSPttD6ktHF7b8dzokdTJJ1fwdtt1wFQU/edit#gid=0",
               sheet="ULAM Entry")
  EntryLog_ULAM$`Date in` = format(EntryLog_ULAM$`Date in`, "%Y-%m-%d")
  EntryLog_ULAM$`Date out` = format(EntryLog_ULAM$`Date out`, "%Y-%m-%d")
  EntryLog_ULAM$`Time in` =  format(EntryLog_ULAM$`Time in`, "%H:%M:%S")
  EntryLog_ULAM$`Time out` =  format(EntryLog_ULAM$`Time out`, "%H:%M:%S")
  colnames(EntryLog_ULAM) = c("Date in","Time in","Date out","Time out","Notes")
  dim(EntryLog_ULAM)
  #EntryLog_ULAM <-EntryLog_ULAM[,-6]
  EntryLog_ULAM$TimeIN =
    as.POSIXct(paste(EntryLog_ULAM$`Date in`, EntryLog_ULAM$`Time in`), format=format)
  EntryLog_ULAM$TimeOUT =
    as.POSIXct(paste(EntryLog_ULAM$`Date out`, EntryLog_ULAM$`Time out`), format=format)
  
  EntryLog_Mop = EntryLog_ULAM[grep("mop", EntryLog_ULAM$Notes), ]
  EntryLog_ULAM$Notes = NULL
}

EntryLog_ULAM = EntryLog_ULAM[-(1:which(EntryLog_ULAM$TimeIN > Exp4_days[1])[1]), ]
EntryLog_Mop = EntryLog_Mop[-(1:which(EntryLog_Mop$TimeIN > Exp4_days[1])[1]), ]

## LOADING THE ARDUINO FILES
## HERE FILL WITH THE DIRECTORY WHERE YOU PLACE THE TEST DATA
current_directory = "./DigitalCages"

setwd(current_directory)
Exp4_days = seq(from=as.POSIXct("2022-5-12 00:00:00", format=format), 
                to=as.POSIXct("2022-7-08 00:00:00", format=format),
                by="day")

## DIGITAL FILES
## FIRST LOOP THROUGH THE MICE LIST
for (p in 1:16) {
  #mouse <- "BB01"
  mouse = paste0("BB0", p)
  ## SECOND LOOP THROUGH ALL THE DAYS OF THE EXPERIMENT
  for (q in 2:length(Exp4_days)) {
    starting_day = Exp4_days[q-1]
    ending_day = Exp4_days[q]
  
    #starting_day <- "2022-5-15 00:00:00"
    #ending_day <- "2022-5-17 00:00:00"
    current_mouse_path = paste0("./",mouse,"/")
    {
      {
        files_analog = list.files(path=current_mouse_path, pattern="out_file_analog_s")
        finfo_analog = file.info(paste0(current_mouse_path, files_analog))
        #files_digital = grep(list.files(current_mouse_path),pattern="analog", invert=TRUE, value=TRUE)
        files_digital = list.files(path=current_mouse_path, pattern="out_file_s")
        finfo_digital = file.info(paste0(current_mouse_path, files_digital))
        #finfo_digital$mtime
        # Isolate only the Arduino csv files matching the starting_day and ending_day
        #files_to_convert_index_digital =
        #  which(finfo_digital$mtime >= starting_day & finfo_digital$mtime <= ending_day)
        files_to_convert_digital = 
          files_digital[finfo_digital$mtime >= starting_day & 
                          finfo_digital$mtime <= ending_day]
        #finfo_digital[files_to_convert_index_digital, c(1,2,4)]
        
        # Extract info on analog files only
        #files_to_convert_index_analog <- 
        #  which(finfo_analog$mtime >= starting_day & finfo_analog$mtime <= ending_day)
        files_to_convert_analog = 
          files_analog[finfo_analog$mtime >= starting_day & 
                         finfo_analog$mtime <= ending_day]
        #finfo_analog[files_to_convert_index_analog,c(1,2,4)]
        
        ## Ingest the files
        ## Digital files
        ## Merge into a single file/object all the arduino files
        ## with timestamps within a day
        library(tidyverse)
        A_digital = paste0(current_mouse_path,files_to_convert_digital) %>%
          map_df(~read_csv(.))
        dim(A_digital)
        
        ## Set the general header labels
        header_temp = c("_Water1_Beambreak",
                        "_Water2_Beambreak",
                        "_Food1_Beambreak",
                        "_Food2_Beambreak",
                        "_Water1_Dispense",
                        "_Water2_Dispense",
                        "_Food1_Dispense",
                        "_Food2_Dispense")
        
        ## Set the header labels specific for Exp4 (based on the logs in ICs_Exp4)
        colnames(A_digital) = 
          c(names(A_digital)[1],
            paste0(ICs_Exp4[which(sub("BB0", "", mouse)==ICs_Exp4$ID), -1], 
                   header_temp))
        names(A_digital) = str_replace_all(names(A_digital), "Water1", "Water")
        names(A_digital) = str_replace_all(names(A_digital), "Water2", "Water")
        
        ## Here I convert the computerTime into date/hr, 
        ## including milliseconds [for ePhis data, when time comes]
        library(dplyr)
        A_digital$time = format(as.POSIXct(A_digital$computerTime / 1000, 
                                           origin = "1970-01-01", 
                                           tz = "America/New_York"),
                                "%Y-%m-%d %H:%M:%OS3")
        
        #A_digital_NEW = A_digital
        
        ## Here I convert the "0101" into "2000" to identify the beambreak event
        ## and then into "1000"             
        for (i in 2:5){
          l_string_clean_list = unlist(
            strsplit(gsub("0101", "2000", 
                          paste(unlist(A_digital[, i]), collapse="")), 
                     ""))
          A_digital[, i] = ifelse(l_string_clean_list == "2", 1 , 0)
        }
        
        ## For the dispense event, the occurrence is labeled as "0".
        ## Thus, since all the values are 1 by default in the arduino file,
        ## I first subtract 1 to all,
        ## then take the abs to convert the -1 into 1. These are the events
        A_digital[, 6:9] = abs(A_digital[, 6:9] - 1)
        
        ## Setting flags for differenty entry logs [0=none, 1=Lab, 2=ULAM, 3=janitor-mop]
        A_digital$entry_flag = 0
        ## Flag=1 for Lab members entering
        for (i in 1:dim(EntryLog_Exp4)[1]) {
          u1 = which(A_digital$time > EntryLog_Exp4$TimeIN[i])[1]
          u2 = tail(which(A_digital$time < EntryLog_Exp4$TimeOUT[i]),1)
          if (isTRUE(u1 <= u2)) {
            A_digital$entry_flag[u1:u2] = 1
          } else {
            next
          }
        }
      }
        ## Add here the janitor log --> EntryLog_ULAM and EntryLog_Mop
        ## Set flag to 2 for ULAM members entering the room
      for (i in 1:dim(EntryLog_ULAM)[1]) {
        u1 = which(A_digital$time > EntryLog_ULAM$TimeIN[i])[1]
        u2 = tail(which(A_digital$time < EntryLog_ULAM$TimeOUT[i]), 1)
        if (isTRUE(u1 <= u2)) {
            A_digital$entry_flag[u1:u2] = 2
        } else {
          next
        }
      }
      ## Set flag to 3 for janitors mopping
      for (i in 1:dim(EntryLog_Mop)[1]) {
        u1 = which(A_digital$time > EntryLog_Mop$TimeIN[i])[1]
        u2 = tail(which(A_digital$time < EntryLog_Mop$TimeOUT[i]), 1)
        if (isTRUE(u1 <= u2)) {
          A_digital_NEW$entry_flag[u1:u2] = 3
        } else {
          next
        }
      }
    }
      
    ## SWAP Food column data [tricky]
    ## Mouse by mouse:
    ## STEP 1: Select the mouse ID from the DailyCheck [s0]
    ## STEP 2: Select the days when Food data must be swapped [days_to_swap]
    ## STEP 3: Select the columns with Food in the header to be swapped
    ## STEP 4: Select the data chunks to be swapped 
    {
      s0 = which(Exp4_DailyChecklist$ID==p)
      data_temp = A_digital
      #eval(parse(text=paste0("data_temp <-",mouse[s],"_digital_stripped_new")))
      days_to_swap = c()
      for (i in 2:length(s0)) {
        if (isTRUE(Exp4_DailyChecklist$FattyLocation[s0[i]] != Exp4_DailyChecklist$FattyLocation[s0[i-1]])) {
          days_to_swap = c(days_to_swap, s0[i])
        }
      }
      ## Days when the food dispenser was swapped
      s2 = Exp4_DailyChecklist[days_to_swap,]
      ## days when the arduino data must be swapped
      days_tags = which(1:length(days_to_swap)%%2 != 0)
      
      for (j in days_tags) {
        if (isTRUE(ending_day > s2$TimeStamp[j])) {
          rows_to_swap = which(data_temp$time >= s2$TimeStamp[j])
          Food1_Beam = data_temp[rows_to_swap, 4]
          Food2_Beam = data_temp[rows_to_swap, 5]
          Food1_Disp = data_temp[rows_to_swap, 8]
          Food2_Disp = data_temp[rows_to_swap, 9]
          data_temp[rows_to_swap, 4] = Food2_Beam
          data_temp[rows_to_swap, 5] = Food1_Beam
          data_temp[rows_to_swap, 8] = Food2_Disp
          data_temp[rows_to_swap, 9] = Food1_Disp
        }
      }
      
      ## Here I renamed all the features stripping 1 and 2 from Food
      names(data_temp) = str_replace_all(names(data_temp), "Food1", "Food")
      names(data_temp) = str_replace_all(names(data_temp), "Food2", "Food")
      
      ## Here I save the final digital file into a specific labeled object 
      day_label_temp = gsub("-", "_", strsplit(as.character(starting_day), " ")[[1]][1])
      eval(parse(text=paste0(mouse,"_digital_FINAL_",day_label_temp," = data_temp")))
      #eval(parse(text=paste0(mouse[s],"_digital_stripped_FINAL <- data_temp")))
    }
    
    ## This sets the object name as the file name and saves the csv file with the data
    eval(parse(text=paste0(mouse, "_digital_FINAL_", day_label_temp)))
    deparse(substitute(paste0(mouse, "_digital_FINAL_", day_label_temp)))
    filename = paste0(mouse, "_digital_FINAL_", day_label_temp)
    write.csv(data_temp,
              file=paste0(current_mouse_path,filename,".csv"),
              row.names=FALSE)
    cat("File ", filename, " written successfully !!")
  }
    
    
}

## ANALOG FILES
mice_temp <- 1:length(mice_list)
#mice_temp <- mice_temp[-2]
{
  ## FIRST LOOP THROUGH THE MICE LIST
  for (p in 1:length(mice_list))
  {
    mouse <- mice_list[p]
    start_time <- Sys.time()
    
    ## SECOND LOOP THROUGH THE EXP4 DAYS TO RETRIEVE THE CORRESPONDENT
    ## ARDUINO FILES
    for (q in 2:length(Exp4_days))
    {
      print(q)
      mouse <- mice_list[p]
      starting_day <- Exp4_days[q-1]
      ending_day <- Exp4_days[q]
      
      #starting_day <- "2022-5-15 00:00:00"
      #ending_day <- "2022-5-17 00:00:00"
      current_directory <- "/analysis/Arduino-Exp4/"
      #current_directory <- getwd()
      current_mouse_path <- paste0(current_directory,"/",mouse,"/")
      files_analog = list.files(path = current_mouse_path,pattern="analog_s4")
      finfo_analog = file.info(paste0(current_mouse_path,files_analog))
      # Isolate only the Arduino csv files matching the starting_day and ending_day
      # Extract info on analog files only
      files_to_convert_index_analog <- 
        which(finfo_analog$mtime >= starting_day & finfo_analog$mtime <= ending_day)
      files_to_convert_analog <- files_analog[files_to_convert_index_analog]
      finfo_analog[files_to_convert_index_analog,c(1,2,4)]
      
      ## Ingest the files
      library(tidyverse)
      A_analog <- NULL
      A_analog <- paste0(current_mouse_path,files_to_convert_analog) %>%
        map_df(~read_csv(.))
      
      ## Analog files
      names(A_analog) <- c("ComputerTime","Wheel_Beambreak")
      
      ## Convert the computer timestamp down to milliseconds
      A_analog$time <- format(as.POSIXct(A_analog$ComputerTime / 1000, 
                                         origin = "1970-01-01", tz = "America/New_York"),
                              "%Y-%m-%d %H:%M")#:%OS3")
      ## this sets beginning of scientific notation options(scipen = 14)
      ## Unit for computerTime in milliseconds
      dim(A_analog)
      #print(q)
      if (dim(A_analog)[1]<2){
        cat("The Arduino Analog data file has only 1 measure !!")
        next
        } else 
          {
      ## Function that converts the raw frequencies into "relative"
      ## values --> contiguous absolute differences abs(t - (t+1))
      ## Check if the abs(t - (t+1)) > 3.0, then use the following:
      ## new frequency <- 5 - max(t,t+1) + min(t,t+1)
      conversion_relative_frequency <- 
        function(frequency1 = NULL , frequency2 = NULL , 
                 frequency_max = 5 , frequency_threshold = 3)
        {
          if(abs(frequency1-frequency2) > frequency_threshold){
            relative_frequency <- 
              frequency_max - max(frequency1,frequency2) + 
              min(frequency1,frequency2)} else {
                relative_frequency <- abs(frequency1 - frequency2)
              }
          return(relative_frequency)
        }

      start_time <- Sys.time()
      a <- 0 #A_analog$relative_frequency
      b <- A_analog$Wheel_Beambreak
      for (j in 2:length(b)[1])
      {
        a[j] <- 
          conversion_relative_frequency(b[j-1],
                                        b[j])
      }
      end_time <- Sys.time()
      #print(end_time-start_time)
      A_analog$relative_frequency <- a
      
      ## Function that generates Distance from frequency to cm
      conversion_frequency_distance_cm <- 
        function(radius_inch = 3, correction_cm = 1.5 ,
                 max_analog_frequency = 5, analog_frequency=NULL)
        {
          conversion_inch_cm <- 2.54 # 1 inch = 2.54 cm
          radius_cm <- radius_inch*conversion_inch_cm
          radius_cm_corrected <- radius_cm - correction_cm
          circumference_cm <- 2*pi*radius_cm_corrected
          ## proportion
          ## circumference_cm/circumference_freq=analog_distance_cm/analog_frequency
          ## analog_distance_cm = analog_frequency*(circumference_cm/circumference_freq)
          ## analog_distance_cm = analog_frequency*(circumference_cm/5)
          analog_distance_cm <- (circumference_cm*analog_frequency)/5
          return(analog_distance_cm)
        }
      
      # A_analog$Wheel_Beambreak_CM <- 
      #   conversion_frequency_distance_cm(analog_frequency=A_analog$Wheel_Beambreak)
      A_analog$Wheel_Beambreak_CM <- 
        conversion_frequency_distance_cm(analog_frequency=A_analog$relative_frequency)
      sum(A_analog$Wheel_Beambreak_CM)/100/1000
      A_analog$Wheel_Beambreak_CM[which(A_analog$Wheel_Beambreak>5)] <- 0
      sum(A_analog$Wheel_Beambreak_CM)/100/1000
      ## Setting flags for different entry logs [0=none, 1=Lab, 2=ULAM, 3=janitor-mop,
      ##  4=wheel is off [frequency=10]]
      ## NEW FLAGGING CODE
      
      start_time=Sys.time()
      ## Create a data frame that contains all TimeIN and TimeOUT from EntryLog_*
      EntryLog = data.frame("TimeIn" = numeric(),
                            "TimeOut" = numeric(),
                            "flag" = integer())
      for (i in 1:nrow(EntryLog_Exp4)){
        EntryLog[nrow(EntryLog) + 1,] <- 
          c(as.integer(EntryLog_Exp4$TimeIN[i]),
            as.integer(EntryLog_Exp4$TimeOUT[i]),1)
      }
      for (i in 1:nrow(EntryLog_ULAM)){
        EntryLog[nrow(EntryLog) + 1,] <-
          c(as.integer(EntryLog_ULAM$TimeIN[i]),
            as.integer(EntryLog_ULAM$TimeOUT[i]), 2)
      }
      for (i in 1:nrow(EntryLog_Mop)){
        EntryLog[nrow(EntryLog) + 1,] <-
          c(as.integer(EntryLog_Mop$TimeIN[i]),
        as.integer(EntryLog_Mop$TimeOUT[i]), 3)
      }
      EntryLog$TimeIn = as.double(EntryLog$TimeIn)
      EntryLog$TimeOut = as.double(EntryLog$TimeOut)
      a1 <- which(is.na(EntryLog$TimeIn)==T)
      if(length(a1)>0) {EntryLog <- EntryLog[-a1,]}
      a2 <- which(is.na(EntryLog$TimeOut)==T)
      if(length(a2)>0) {EntryLog <- EntryLog[-a2,]}
      #EntryLog$TimeOut[125] = 1658000000 # replace N/A with a reasonable time number
      
      ## Sort the data frame based on TimeIN
      EntryLog = EntryLog[order(EntryLog$TimeIn),]
      
      # The origin EntryLog information, since in the below loop,
      # we discard information in EntryLog that we do not use
      # EntryLog <- EntryLog_origin
      # EntryLog_origin = EntryLog
      
      A_analog$entry_flag = 0
      i = 1
      while (i <= nrow(A_analog) && dim(EntryLog)[1]>0){
        t = A_analog$ComputerTime[i]/1000
        if (t >= EntryLog$TimeIn[1] && t <= EntryLog$TimeOut[1]){ 
          #cat(i)
          A_analog$entry_flag[i] = EntryLog$flag[1]
        } 
        else if(t > EntryLog$TimeOut[1] && dim(EntryLog)[1]>0){ # look into next row of EntryLog
          #i = i - 1
          EntryLog = EntryLog[-c(1),]
        }
        i = i + 1
      }
      
      end_time=Sys.time()
      end_time-start_time
      
      ## This sets the object name as the file name and saves the csv file with the data
      #eval(parse(text=paste0(mouse[s],"_analog_FINAL_",day_label_temp)))
      #deparse(substitute(paste0(mouse[s],"_analog_FINAL_",day_label_temp)))
      day_label_temp <- gsub("-","_",strsplit(starting_day," ")[[1]][1])
      filename <- paste0(current_mouse_path,mouse,"_analog_FINAL_",day_label_temp,".csv")
      #paste0(mouse,"_analog_FINAL_",day_label_temp)
      write.csv(A_analog,file=filename,row.names = F)
      cat("File ",filename," written successfully !!")
      
 # {     ## BINNING
 #      
 #      ## Here I bin the analog data into day/hr/minute/second bins, within the time
 #      ## frame set by starting_day and ending_day
 #      # starting_day <- "2022-5-13 00:00:00.000"
 #      # ending_day <- "2022-5-14 00:00:00.000"
 #      {
 #        starting_day_time <- as.POSIXct(starting_day,origin = "1970-01-01", tz = "America/New_York")
 #        starting_day_CPUtime <- unclass(starting_day_time)*1000
 #        ending_date_time <- as.POSIXct(ending_day,origin = "1970-01-01", tz = "America/New_York")
 #        ending_date_CPUtime <- unclass(ending_date_time)*1000
 #        
 #        Day_data_day_array_cpuTime <- 
 #          seq(starting_day_CPUtime,ending_date_CPUtime,(60*60*1000*24))
 #        Day_data_hr_array_cpuTime <- 
 #          seq(starting_day_CPUtime,ending_date_CPUtime,(60*60*1000))
 #        Day_data_min_array_cpuTime <- 
 #          seq(starting_day_CPUtime,ending_date_CPUtime,(60*1000))
 #        Day_data_seconds_array_cpuTime <- 
 #          seq(starting_day_CPUtime,ending_date_CPUtime,(1000))
 #        
 #        Day_data_hr_array <- format(as.POSIXct(Day_data_hr_array_cpuTime / 1000,
 #                                               origin = "1970-01-01", tz = "America/New_York"),
 #                                    "%Y-%m-%d %H:%M:%OS3") # 24 elements
 #        Day_data_min_array <- format(as.POSIXct(Day_data_min_array_cpuTime / 1000, 
 #                                                origin = "1970-01-01", tz = "America/New_York"),
 #                                     "%Y-%m-%d %H:%M:%OS3") # 1401 elements
 #        # Day_data_seconds_array <- format(as.POSIXct(Day_data_seconds_array_cpuTime / 1000, 
 #        #                                             origin = "1970-01-01", tz = "America/New_York"),
 #        #                                  "%Y-%m-%d %H:%M:%OS3") # 86401 elements
 #        data_temp_analog <- A_analog  
 #        ## BINNING BY HRS
 #        {  
 #          data_temp_hr <- NULL
 #          for (j in 2:length(Day_data_hr_array))
 #          {
 #            #print(j)
 #            bin_hr <- NULL
 #            bin_hr <- which(data_temp_analog$time < Day_data_hr_array[j] & 
 #                              data_temp_analog$time >  Day_data_hr_array[j-1])
 #            if (identical(integer(0),bin_hr))
 #            {next}
 #            data_temp_hr <- 
 #              rbind(data_temp_hr,
 #                    c(Day_data_hr_array[j],sum(data_temp_analog$Wheel_Beambreak_CM[bin_hr]),
 #                      max(data_temp_analog$entry_flag[bin_hr])))
 #          }
 #          dim(data_temp_hr)
 #          colnames(data_temp_hr) <- c("time","Wheel_Beambreak_CM","entry_flag")
 #          data_temp_hr <- as.data.frame(data_temp_hr)
 #          ## Here I save the final analog file
 #          day_label_temp <- gsub("-","_",strsplit(starting_day," ")[[1]][1])
 #          eval(parse(text=paste0(mouse,"_analog_FINAL_binned_HRs_",day_label_temp,"<- data_temp_hr")))
 #          #deparse(substitute(paste0(mouse[s],"_digital_FINAL_binned_HRs_",day_label_temp)))
 #          filename <- paste0(mouse,"_analog_FINAL_binned_HR_",day_label_temp)
 #          write.csv(data_temp_hr,file=paste0(current_mouse_path,filename,".csv"),
 #                    row.names = F)
 #          cat("ANALOG BINNED HRs File ",filename," written successfully !!")
 #        }
 #        
 #        ## Here I take the hourly binned data and standardize the data
 #        data_temp_hr_std <- data_temp_hr
 #          data_temp_hr_std$Wheel_Beambreak_CM <- 
 #            scale(as.numeric(unlist(data_temp_hr$Wheel_Beambreak_CM)),
 #                  center=T,scale=T)
 #          filename <- paste0(mouse,"_analog_FINAL_binned_HR_STD_",day_label_temp)
 #          write.csv(data_temp_hr_std,
 #                  file=paste0(current_mouse_path,filename,".csv"),
 #                  row.names = F)
 #        cat("BINNED HRs File ",filename," STD written successfully !!")
 #        
 #        ## BINNING BY MINUTES
 #        # {  data_temp_min <- NULL
 #        #   for (j in 2:length(Day_data_min_array))
 #        #   {
 #        #     #print(j)
 #        #     bin_min <- NULL
 #        #     bin_min <- which(data_temp_analog$time < Day_data_min_array[j] & 
 #        #                        data_temp_analog$time >  Day_data_min_array[j-1])
 #        #     if (identical(integer(0),bin_min))
 #        #     {next}
 #        #     data_temp_min <- 
 #        #       rbind(data_temp_min,
 #        #             c(Day_data_min_array[j],
 #        #               sum(data_temp_analog$Wheel_Beambreak_CM[bin_min]),
 #        #               max(data_temp_analog$entry_flag[bin_min])))
 #        #   }
 #        #   dim(data_temp_min)
 #        #   colnames(data_temp_min) <- c("time","Wheel_Beambreak_CM","entry_flag")
 #        #   data_temp_min <- as.data.frame(data_temp_min)
 #        #   
 #        #   eval(parse(text=paste0(mouse[s],"_analog_FINAL_binned_MINUTES_",day_label_temp," <- data_temp_min")))
 #        #   filename <- paste0(mouse[s],"_analog_FINAL_binned_MINUTES_",day_label_temp)
 #        #   write.csv(data_temp_min,file=paste0(current_mouse_path,filename,".csv"),
 #        #             row.names = F)
 #        #   cat("ANALOG BINNED MINUTES File ",filename," written successfully !!")
 #        # }
 #        # ## BINNING BY SECONDS
 #        # {  data_temp_seconds <- NULL
 #        #     for (j in 2:length(Day_data_seconds_array))
 #        #     {
 #        #       print(j)
 #        #       bin_seconds <- NULL
 #        #       bin_seconds <- which(data_temp_analog$time < Day_data_seconds_array[j] & 
 #        #                          data_temp_analog$time >  Day_data_seconds_array[j-1])
 #        #       if (identical(integer(0),bin_seconds))
 #        #       {next}
 #        #       data_temp_seconds <- rbind(data_temp_seconds,
 #        #                              c(Day_data_seconds_array[j],colSums(data_temp_analog[bin_seconds,2:9]),
 #        #                                max(data_temp_analog$entry_flag[bin_seconds])))
 #        #     }
 #        #     dim(data_temp_seconds)
 #        #     colnames(data_temp_seconds) <- c("time",names(data_temp_analog)[2:9],"entry_flag")
 #        #     data_temp_seconds <- as.data.frame(data_temp_seconds)
 #        #     
 #        #     eval(parse(text=paste0(mouse[s],"_digital_FINAL_binned_SECONDS_",day_label_temp," <- data_temp_min")))
 #        #     filename <- paste0(mouse[s],"_digital_FINAL_binned_SECONDS_",day_label_temp)
 #        #     write.csv(data_temp_min,file=paste0(current_mouse_path,filename,".csv"),
 #        #               row.names = F)
 #        #     cat("BINNED SECONDS File ",filename," written successfully !!")
 #        #   }
 #      }
 #      }
        }
    }
    end_time <- Sys.time()
    print(end_time-start_time)
  }
}


rm(list = ls())

library(data.table)
library(zoo)
library(caTools)
library(ggplot2)
library(ggpubr)
library(Rmixmod)
library(tidyverse)
library(lubridate)
library(viridis)
library(plyr)
library(smoother)
library(ggsci)


################### ################### ###################
################### ################### ###################
##################### parameters
################### ################### ###################
################### ################### ###################

thr_pitch<- 60 #threshold for Pitch detecting land walking during foraging trips (default = 60)
thr_pitch_low<- 20 #mean lower threshold for Pitch detecting land walking during foraging trips (default= 20)
thr_temp<-0 #threshold for temperature detecting land walking during foraging trips (default = 0)
thr_time<-750 #threshold for time required for detecting land walking during foraging trips (default = 750)
thr_pitch_lie<- -35 #threshold for Pitch detecting when back in water (default = -12.5)
thr_depth<-2 #threshold for surface not surface variable (default = 2)

# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################### ################### ###################
################### ################### ###################
##################### read files
################### ################### ###################
################### ################### ###################


filenamesTrips <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/DataAdelie_Trips_2018-2019",
    pattern = "*.csv",
    full.names = TRUE
  )


filenamesEM <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019",
    pattern = "*.csv",
    full.names = TRUE
  )


#### read file
#accData<-fread("/Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/G3002MP19_S1Cut.csv",header=TRUE)
TripID<-39
filenamesTrips[TripID]
accData <- fread(filenamesTrips[TripID], header = TRUE)

accData<-accData[,-c("V1")]
names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")  
names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_") 

options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")

# #### calculate depth
# result <- getmode(accData$Pressure[which(!is.na(accData$Pressure))])
# accData$depth<-(accData$Pressure-result)/100
# depth<-accData$depth[-which(is.na(accData$depth))]
# depth25Hz<-rep(depth,each=25)
# accData$depth25Hz<-depth25Hz[1:nrow(accData)]
# accData$depth25Hz<-accData$depth25Hz-min(summary(accData$depth25Hz))
 summary(accData$depth25Hz)

# 
# #### calculate change in depth
# depth <- accData$depth25Hz
# myFreq <- 25
# idx <- seq(from = 1, to = nrow(accData), by = myFreq)
# depth <- depth[idx]
# depth1 <- c(depth[2:length(depth)], depth[length(depth)])
# changeDepth <- depth1 - depth
# changeDepth25Hz<-rep(changeDepth,each=25)
# accData$changeDepth25Hz<-changeDepth25Hz[1:nrow(accData)]
# 
# 
# # calculation of Roll
# accData$Roll <-
#   atan2((accData$static_LA),
#         sqrt(
#           accData$static_BF * accData$static_BF + accData$static_DV * accData$static_DV
#         )
#   ) * 180 / pi
# 
# accData$SD_Roll <-
#   runsd(accData$Roll, k = 751) #SD over running mean over 30 sec, sampling 25Hz, if even number k, it will be centered
# 
# 
# accData$SD_Pitch <-
#   runsd(accData$Pitch, k = 1501) #SD over running mean over 1min, sampling 25Hz
# 
# #VeDBA
# accData$VeDBA <- sqrt((accData$Dynamic_DorsoVentral^2)+(accData$Dynamic_Lateral^2)+(accData$Dynamic_BackForward^2))
# 
# #SD SD_VeDBA
# accData$SD_VeDBA <- runsd(accData$VeDBA, k = 1501)
# 
# 
# Sur - Not Sur variable
#hist(accData$depth25Hz,breaks=50)
Sur_NotSur <- ifelse(accData$depth25Hz <= thr_depth, 0, 1) ##### look at this
accData$Sur_NotSur <- as.factor(Sur_NotSur)

#### #### #### #### #### #### #### #### #### #### 
# add EM results  
#### #### #### #### #### #### #### #### #### #### 

EM_ID<-TripID*4
EM_Partition<- fread(filenamesEM[EM_ID], header = TRUE)
head(EM_Partition)
head(accData)
################### ################### ###################
################### ################### ###################
################### assign states
################### ################### ###################
################### ################### ###################

# 
plotdepth<-ggplot(accData) +
  geom_line(aes( x=DatesPos, y=depth25Hz), size=0.7)

plotPitch<-ggplot(accData) +
  geom_line(aes( x=DatesPos, y=Pitch), size=0.7)+ geom_hline(yintercept = 0)

plotTemp<-ggplot(accData) +
  geom_point(aes( x=DatesPos, y=Temp), size=0.7)+ geom_hline(yintercept = 0)

dev.new()
ggarrange(plotdepth,plotPitch,plotTemp,ncol=1)
# 


accData$States<-EM_Partition$StatesComb_22
accData$States1<-EM_Partition$StatesComb_26
accData$States2<-EM_Partition$StatesComb_6
accData$States3<-EM_Partition$StatesComb_42
accData$States4<-EM_Partition$StatesComb_38
accData$States5<-EM_Partition$StatesComb_27
accData$States6<-EM_Partition$StatesComb_10
accData$States7<-EM_Partition$StatesComb_21
accData$States8<-EM_Partition$StatesComb_18
accData$States9<-EM_Partition$StatesComb_25
accData$States10<-EM_Partition$StatesComb_17

# sort(unique(accData$States1))
# sort(unique(accData$States2))
# sort(unique(accData$States3))
# sort(unique(accData$States4))
# sort(unique(accData$States5))
# sort(unique(accData$States6))
# sort(unique(accData$States7))
# sort(unique(accData$States8))
# sort(unique(accData$States9))
# sort(unique(accData$States10))
################### ################### ###################
################### ################### ###################
############ divide dataset and check results
################### ################### ###################
################### ################### ###################

subDataDive <- accData[which(accData$Sur_NotSur == 1), ]
subDataUW <- accData[which(accData$Sur_NotSur == 0), ]

#### #### #### #### #### #### #### #### #### #### 
#detect ice/land events
#### #### #### #### #### #### #### #### #### #### 

subDataUW$Land_NotLand<-0 #unknown 
#calculate succesive differences between dates
#and identify gaps larger than 0.0500 - watch out for datePosix that is rounding time
subDataUW$gap <- c(0, diff(subDataUW$DatesPos,units = "secs") > 0.0500)
# cumulative sum of 'gap' variable
subDataUW$group <- cumsum(subDataUW$gap) + 1
subDataUWList<-split(subDataUW, subDataUW$group)

#   meanGroup<-aggregate(. ~ subDataUW$group, subDataUW[c("Pitch")], mean)
#   hist(meanGroup$Pitch,breaks=50)


for(nList in 1:length(subDataUWList)){
  
  meanT<-mean(subDataUWList[[nList]]$Temp,na.rm=T)
  timeChunk<-nrow(subDataUWList[[nList]])
  
  if((max(subDataUWList[[nList]]$Pitch)>=thr_pitch & meanT>thr_temp) | (mean(subDataUWList[[nList]]$Pitch)>=thr_pitch_low & timeChunk> thr_time)) {
  
    DataChunk<-subDataUWList[[nList]]
    DataChunk$Land_NotLand<-1 #is on Land
    DataChunk$Land_NotLand2<-NA
    DataChunk$Land_NotLand2[which(DataChunk$Pitch>=thr_pitch)]<-1 #1 is on Land
    DataChunk$gapNA <- NA
    DataChunk$groupNA <- NA
    
    DataChunk_NA<-DataChunk[which(is.na(DataChunk$Land_NotLand2)),]
    DataChunk_NA$gapNA <- c(0, diff(DataChunk_NA$DatesPos,units = "secs") > 0.0500)
    DataChunk_NA$groupNA <- cumsum(DataChunk_NA$gapNA) + 1
    
    if(nrow(DataChunk_NA)>0){
      DataChunk_NA_List<-split(DataChunk_NA, DataChunk_NA$groupNA)
      
      for(nListNA in 1:length(DataChunk_NA_List)){
        
        if( any(DataChunk_NA_List[[nListNA]]$Pitch<thr_pitch_lie)){
          DataChunk_NA_List[[nListNA]]$Land_NotLand<-0 #labelled as UW
        } else{
          DataChunk_NA_List[[nListNA]]$Land_NotLand<-1 #labelled as Land
        }
      }
      
      DataChunk_NA <- ldply(DataChunk_NA_List, data.frame,.id = NULL)
    }
    
    DataChunk_NotNA<-DataChunk[which(!is.na(DataChunk$Land_NotLand2)),]
    
    DataChunk<-rbind(DataChunk_NA,DataChunk_NotNA)
    DataChunk<-DataChunk[order(DataChunk$DatesPos), ]
    
    subDataUWList[[nList]]<-DataChunk
    
  }
}


subDataUW <- ldply(subDataUWList, data.frame,.id = NULL)
subDataUW<-subDataUW[order(subDataUW$DatesPos), ]
dim(subDataUW)
subDataUW$Land_NotLand<-as.factor(subDataUW$Land_NotLand)
LandPar<-length(unique(subDataUW$Land_NotLand))

if(LandPar>1){ #if both water and land/ice events have been detected
  
  subDataUW_Land<-subDataUW[which(subDataUW$Land_NotLand==1),]
  subDataUW_NotLand<-subDataUW[which(subDataUW$Land_NotLand==0),]
  
  }


dim(subDataUW_NotLand)
dim(subDataUW_Land)
dim(subDataDive)

################### ################### ###################
################### ################### ###################
##################### plots Dives
################### ################### ###################
################### ################### ###################
# BIC_Dive<- fread(filenamesEM[1], header = TRUE)
# BIC_Dive<-BIC_Dive[,-c("V1")]
# 
# plot(BIC_Dive~nStatesDive,data=BIC_Dive,type="b",pch=19)

unique(subDataDive$States1)
unique(subDataDive$States3)
unique(subDataDive$States6)
unique(subDataDive$States4)
unique(subDataDive$States8)

#subDataDive$States_lev<-factor(subDataDive$States6, levels = c("1_Dive","2_Dive","3_Dive"))
subDataDive$States_lev<-factor(subDataDive$States, levels = c("1_Dive","2_Dive","3_Dive","4_Dive"))
subDataDive$States_lev<-factor(subDataDive$States3, levels = c("1_Dive","2_Dive","3_Dive","4_Dive","5_Dive"))

# # # # #
start<-150000
  end<-300000

plot1<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Depth (m)")
#plot1

plot3<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("VeDBA")

plot4<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=SD_DV2),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=SD_DV2,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("SD_DV2")

plot5<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Pitch")
dev.new()
ggarrange(plot1,plot3,plot5, ncol=1,common.legend = TRUE)

################### ################### ###################

box1<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=(depth25Hz*-1),fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")+ylab("Depth (m)")

box2<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=changeDepth25Hz,fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")

box3<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=SD_DV2,fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")

box4<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

box5<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

# dev.new()
# ggarrange(box1,box2,box3,box4,box5,box6,ncol=2,nrow=3,common.legend = TRUE)


dev.new()
ggarrange(box1,box2,box3,box4,box5,ncol=3,nrow=2,common.legend = TRUE)


################### ################### ###################
################### create catching DF
################### ################### ###################


# ####### divide Dives in events and extrat time spent in different states.
# subDataDive$gap <- c(0, diff(subDataDive$DatesPos,units = "secs") > 0.0500)
# subDataDive$group <- cumsum(subDataDive$gap) + 1
# #plot(subDataDive$depth25Hz[which(subDataDive$group==200)], type="b")
# 
# CatchingDF<-data.frame(NA,NA,NA,NA,NA,NA)# empty dataframe for storing results
# rowCount<-1
# 
# for (DiveEv in 1:length(unique(subDataDive$group))) {
#   ###### divide each Dives in catching events and extrat time spent in different catching events
#   dive_ev <- subDataDive[which(subDataDive$group == DiveEv &
#                         subDataDive$States_lev == "4_Dive"), ]
#   
#   dive_ev$gapCatch <- c(0, diff(dive_ev$DatesPos, units = "secs") > 0.0500)
#   dive_ev$groupCatch <- cumsum(dive_ev$gapCatch) + 1
#   #unique(dive_ev$groupCatch)
# 
#   if (nrow(dive_ev) > 2) {
#     for (catchEv in 1:length(unique(dive_ev$groupCatch))) {
#       dive_evSub <- dive_ev[which(dive_ev$groupCatch == catchEv), ]
#       
#       if(nrow(dive_evSub)>2){ 
#       CatchingDF[rowCount, 1] <- dive_evSub$Timestamp[1] #start
#       CatchingDF[rowCount, 2] <-
#         dive_evSub$Timestamp[nrow(dive_evSub)] #end
#       
#       duration <-
#         difftime(dive_evSub$DatesPos[nrow(dive_evSub)], dive_evSub$DatesPos[1], units = "secs")
#       CatchingDF[rowCount, 3] <- as.numeric(duration)#duration
#       
#       CatchingDF[rowCount, 4] <- mean(dive_evSub$depth25Hz)#depth
#       CatchingDF[rowCount, 5] <- mean(dive_evSub$VeDBA)#VeDBA
#       CatchingDF[rowCount, 6] <- unique(dive_ev$group)
#       
#       rowCount <- rowCount + 1
#       }
#     }
#   }
# }
# 
# 
# colnames(CatchingDF)<- c("DateTime_start","DateTime_end","duration_sec","depth_m","VeDBA","diveID")
# head(CatchingDF)
# options(digits.secs=9)  
# CatchingDF$Animal_ID<-as.factor(unique(accData$TagID))
# CatchingDF$DatesPos<-as.POSIXct(CatchingDF$DateTime_start, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
# 
# # CatchingDF$diveID<-as.factor(CatchingDF$diveID)
# # 
# #CatchingDF<-CatchingDF[which(CatchingDF$duration_sec>=1),]
# # 
# # hist(CatchingDFLong$VeDBA)
# # 
# # ggplot(CatchingDFLong, aes(x=diveID, y=VeDBA)) + 
# #   geom_boxplot(fill="slateblue", alpha=0.2) + 
# #   xlab("")
# # 
# # plot(CatchingDFLong$duration_sec)
# 
# 
# library(hrbrthemes)
# library(viridis)
# 
# # ViolingP <- ggplot(CatchingDF, aes(x=Animal_ID, y=depth_m)) + 
# #   geom_violin(fill="dodgerblue2")+
# #   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
# # #  scale_fill_viridis(discrete = TRUE) +
# #   theme_bw() +
# #   theme(
# #     legend.position="none",
# #     plot.title = element_text(size=20)
# #   ) +ylab("depth (m)")+xlab("")
# # 
# # ViolingP
# # 
# 
# 
# 
# summary(CatchingDF$depth_m)
# plotCatching<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=depth_m), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(name="depth (m)")
# 
# #plotCatching
# 
# summary(CatchingDF$duration_sec)
# MinVal<-round(min(CatchingDF$duration_sec))
# MaxVal<-round(max(CatchingDF$duration_sec))
# 
# library("scales")
# plotCatchingDur<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=duration_sec), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_gradientn(colours = c("papayawhip","peachpuff3","peachpuff4","salmon3","salmon4"), 
#                          values = rescale(c(MinVal,2.5,5,MaxVal)),
#                          guide = "colorbar", limits=c(0,MaxVal), name= "duration (sec)")
# 
# 
# #plotCatchingDur
# 
# 
# plotCatchingVedBA<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=VeDBA), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(option = "plasma", name= "VeDBA (g)")
# 
# #plotCatchingVedBA
# 
# dev.new()
# ggarrange(plotCatching,plotCatchingDur,plotCatchingVedBA,ncol=1,nrow=3,common.legend = FALSE)
# 


# plotCatchDur<-ggplot() +
#   geom_point(data=CatchingDF,aes( x=duration_sec, y=VeDBA),colour="black", size=2)+ylab("VeDBA (g)")+xlab("depth (m)")
# dev.new()
# plotCatchDur
################### ################### ###################
################### ################### ###################
##################### plots Land
################### ################### ###################
################### ################### ###################

unique(subDataUW_Land$States3)
subDataUW_Land$States_lev<-factor(subDataUW_Land$States8, levels = c("1_UWLand","2_UWLand"))
subDataUW_Land$States_lev<-factor(subDataUW_Land$States, levels = c("1_UWLand","2_UWLand","3_UWLand"))
#subDataUW_Land$States_lev<-factor(subDataUW_Land$States1, levels = c("1_UWLand","2_UWLand","3_UWLand","4_UWLand"))
#subDataUW_Land$States_lev<-subDataUW_Land$States
#subDataUW_Land$States_lev<-factor(subDataUW_Land$States2, levels = c("1_UWLand","2_UWLand","3_UWLand","4_UWLand","5_UWLand"))


box2<-ggplot(subDataUW_Land) +
  geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

box3<-ggplot(subDataUW_Land) +
  geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+ 
  scale_fill_jco() +xlab("")

box4<-ggplot(subDataUW_Land) +
  geom_boxplot(aes( x=States_lev, y=SD_Roll,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

dev.new()
ggarrange(box2,box3,box4,ncol=3,nrow=1,common.legend = TRUE)


dim(subDataUW_Land)
start<-1 
end<-nrow(subDataUW_Land)

plot1<-ggplot() +
#  geom_line(data=accData[300000:500000],aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  geom_point(data=subDataUW_Land[start:end,],aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Pitch (degrees)")
#plot1

plot2<-ggplot(subDataUW_Land[start:end,]) +
#  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("VeDBA")

plot3<-ggplot(subDataUW_Land[start:end,]) +
  #  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Temp,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Temp")

dev.new()
ggarrange(plot1,plot3,ncol=1,common.legend = TRUE)


################### ################### ###################
################### ################### ###################
##################### plots UW swim
################### ################### ###################
################### ################### ###################
#unique(subDataUW_NotLand$States8)
#subDataUW_NotLand$States_lev<-factor(subDataUW_NotLand$States8, levels = c("1_UWNotLand","2_UWNotLand"))

unique(subDataUW_NotLand$States1)
subDataUW_NotLand$States_lev<-factor(subDataUW_NotLand$States, levels = c("1_UWNotLand","2_UWNotLand","3_UWNotLand"))

# unique(subDataUW_NotLand$States5)
# subDataUW_NotLand$States_lev<-factor(subDataUW_NotLand$States5, levels = c("1_UWNotLand","2_UWNotLand","3_UWNotLand","4_UWNotLand"))


myColors <- pal_jama()(4)
names(myColors) <- levels(unique(subDataUW_NotLand$States_lev))
custom_colors <- scale_fill_manual(name = "behavioural states below water surface", values = myColors)#,
                                    # labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))

custom_colors2 <- scale_colour_manual(name = "behavioural states below water surface", values = myColors)#,
                                  # labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))


box2<-ggplot(subDataUW_NotLand) +
  geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("Pitch (degrees)")+theme(text = element_text(size=15))

box3<-ggplot(subDataUW_NotLand) +
  geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("VeDBA (g)")+theme(text = element_text(size=15))

box4<-ggplot(subDataUW_NotLand) +
  geom_boxplot(aes( x=States_lev, y=SD_Roll,fill=States_lev),colour="gray25") +
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("SD Roll (degrees)")+theme(text = element_text(size=15))


dev.new()
ggarrange(box2,box3,box4,ncol=3,nrow=1,common.legend = TRUE)


dim(subDataUW_NotLand)
start<-1
end<-nrow(subDataUW_NotLand)

plot1<-ggplot(subDataUW_NotLand[start:end,]) +
  geom_line(aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  custom_colors2+xlab("")+ylab("Pitch (degrees)")
#plot1

plot2<-ggplot(subDataUW_NotLand[start:end,]) +
  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  custom_colors2+xlab("")+ylab("VeDBA")

plot3<-ggplot(subDataUW_NotLand[start:end,]) +
  #  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Temp,colour=States_lev), size=2)+
  custom_colors2+xlab("")+ylab("Temp")

dev.new()
ggarrange(plot1,plot3,ncol=1,common.legend = TRUE)


#################################################################################
####################################### label states and write file
#################################################################################

head(accData)
accData<-accData[,-c("States1","States10","States2","States3","States4","States5","States6","States7","States","States9","DatesPos")] #choose col States

StatesNames<-rep(NA,nrow(accData))

unique(accData$States8)
StatesID<-accData$States8
StatesNames<-ifelse(StatesID=="4_Dive",StatesNames<-"Descending",
                    ifelse(StatesID=="2_Dive",StatesNames<-"Hunting",
                           ifelse(StatesID=="3_Dive",StatesNames<-"Ascending",
                                  ifelse(StatesID=="1_Dive",StatesNames<-"Swimming",
                                      #  ifelse(StatesID=="5_Dive",StatesNames<-"Swimming_2",
                                         
                                        # ifelse(StatesID=="2_UWLand",StatesNames<-"Walk",
                                             #   ifelse(StatesID=="2_UWLand",StatesNames<-"Stand",
                                                       ifelse(StatesID=="2_UWLand",StatesNames<-"LieDown",
                                                              ifelse(StatesID=="1_UWLand",StatesNames<-"Preen/highFlap_L",
                                                                     
                                                                     #         ifelse(StatesID=="4_UWNotLand",StatesNames<-"Stand",
                                                                     ifelse(StatesID=="3_UWNotLand",StatesNames<-"Rest",
                                                                            ifelse(StatesID=="2_UWNotLand",StatesNames<-"Swim/Porpoise",
                                                                                   "Preen/highFlap_W"))))))))#)#)#)#)

unique(StatesNames)
accData$StatesNames<-StatesNames

accData[1:20,c("States","StatesNames")]
head(accData)


ID_Ind <- basename(filenamesTrips[TripID])
ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
ID_Ind <- paste(ID_Ind, "_EM_Sel", ".csv", sep = "")
pathRes <-
  paste("/Users/mariannachimienti/MarieCurie/Adelie_2018_2019EM/",
        ID_Ind,
        sep = "")

pathRes
write.csv(accData, pathRes, row.names = TRUE)

#################################################################################
####################################### general big plots 
#################################################################################
library("ggsci")
library("wesanderson")
library(lisa) # https://cran.r-project.org/web/packages/lisa/readme/README.html

filenamesTrips <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Paper_I_data&analysis/Adelie_2019_2020EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#### read file
#accData<-fread("/Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/G3002MP19_S1Cut.csv",header=TRUE)
TripID<-20
TripID<-81 #for underneath the ice
filenamesTrips[TripID]
accData <- fread(filenamesTrips[TripID], header = TRUE)

accData<-accData[,-c("V1")]
names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")  
names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_") 

options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
head(accData)

plot1<-ggplot() + theme_bw()+
  geom_line(data=accData,aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)



unique(accData$States1)
unique(accData$States4)
#subDataUW_Land$States_lev<-factor(subDataUW_Land$States, levels = c("1_UWLand","2_UWLand","3_UWLand"))
subDataUW_Land$States_lev<-factor(subDataUW_Land$States1, levels = c("1_UWLand","2_UWLand","3_UWLand","4_UWLand"))
subDataUW_NotLand$States_lev<-factor(subDataUW_NotLand$States1, levels = c("1_UWNotLand","2_UWNotLand","3_UWNotLand"))
subDataDive$States_lev<-factor(subDataDive$States1, levels = c("1_Dive","2_Dive","3_Dive","4_Dive"))


accData$States_lev<-factor(accData$States1, levels = c("1_Dive","2_Dive","3_Dive","4_Dive",
                                                       "1_UWLand","2_UWLand","3_UWLand","4_UWLand",
                                                       "1_UWNotLand","2_UWNotLand","3_UWNotLand"))

accData$States_lev<-factor(accData$States4, levels = c("1_Dive","2_Dive","3_Dive","4_Dive","5_Dive",
                                                       "1_UWLand","2_UWLand","3_UWLand","4_UWLand",
                                                       "1_UWNotLand","2_UWNotLand","3_UWNotLand"))


lineS<-0.7
pointS<-6
textS<-25

subAcc<-accData[790000:860000]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_UWLand"|subAcc$States_lev=="2_UWLand"|subAcc$States_lev=="3_UWLand"|subAcc$States_lev=="4_UWLand"),]
subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))

#define custom color scale
myColors <-  lisa_palette("MarcChagall")[2:5]
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "behavioural states on land/ice", values = myColors,
                                     labels=c("1_UWLand"="Preen/Flap on land", "2_UWLand"="Stand", "3_UWLand"="Lie Down/Toboggan","4_UWLand"="Walk"))

# plot0<-ggplot() + theme_bw()+
#   geom_line(data=subAcc,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   #  geom_point(data=subAccPoint, aes(x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=2)+
#   # scale_colour_igv()+
#   custom_colors+
#   xlab("")+ylab("Depth (m)")+
#   theme(text = element_text(size=15))+ theme(legend.text=element_text(size=15))

plot1<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  custom_colors+ xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))


plot2<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot1,plot2,ncol=1,common.legend = TRUE)


###################### below surface

subAcc<-accData[1293000:1295500]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_UWNotLand"|subAcc$States_lev=="2_UWNotLand"|subAcc$States_lev=="3_UWNotLand"),]
subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))

#define custom color scale
myColors <- pal_jama()(3)
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "behavioural states below water surface", values = myColors,
                                     labels=c("1_UWNotLand"="Slow surface swim", "2_UWNotLand"="Swim/Porpoise", "3_UWNotLand"="Preen/Flap on water"))

plot0<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=lineS)+
#  geom_point(data=subAccPoint, aes(x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=2)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Depth (m)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))


plot1<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  custom_colors+xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot2<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot0,plot1,plot2,ncol=1,common.legend = TRUE)


subAcc<-accData[2370200:2373000]
#subAcc<-accData[1292000:1303000]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_UWNotLand"|subAcc$States_lev=="2_UWNotLand"|subAcc$States_lev=="3_UWNotLand"),]
subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))

#define custom color scale
myColors <- pal_jama()(3)
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "behavioural states below water surface", values = myColors,
                                     labels=c("1_UWNotLand"="Slow surface swim", "2_UWNotLand"="Swim/Porpoise", "3_UWNotLand"="Preen/Flap on water"))


plot1a<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  custom_colors+xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot2a<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot1a,plot2a,ncol=1,common.legend = TRUE)

########################### diving 
#subAcc<-accData[920000:940000] #for shallow diving
subAcc<-accData[1290000:1306000] #for deep dives
subAccPoint<-subAcc[which(subAcc$States_lev=="1_Dive"|subAcc$States_lev=="2_Dive"|subAcc$States_lev=="3_Dive"|subAcc$States_lev=="4_Dive"),]
#subAccPoint<-subAcc[which(subAcc$States_lev=="1_Dive"|subAcc$States_lev=="2_Dive"|subAcc$States_lev=="3_Dive"|subAcc$States_lev=="4_Dive"|subAcc$States_lev=="5_Dive"),]

subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))
#define custom color scale

myColors <-  lisa_palette("KarlZerbe")[c(1,2,3,4)]
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "diving behavioural states", values = myColors,
                                     labels=c("1_Dive"="Hunt", "2_Dive"="Ascend", "3_Dive"="Swim/Cruise while diving", "4_Dive"="Descend"),
                                     breaks = c("4_Dive", "3_Dive","1_Dive", "2_Dive"))

# myColors <-  lisa_palette("KarlZerbe")[c(1,2,3,4,5)]
# names(myColors) <- levels(unique(subAccPoint$States_lev))
# custom_colors <- scale_colour_manual(name = "diving behavioural states", values = myColors,
#                                      labels=c("1_Dive"="Descend", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Swim/Cruise type 2", "4_Dive"="Hunt","5_Dive"="Ascend"))

plot0<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint, aes(x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Depth (m)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot1<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot2<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot0,plot1,plot2,ncol=1,common.legend = TRUE)

#ggsave(path = "/Users/mariannachimienti/Dropbox/MarieCurie/Paper_I/Figures/Figure2HR", width = width, height = height, device='tiff', dpi=300)

subAcc<-accData#[900000:1000000]
#subAccPoint<-subAcc[which(subAcc$States_lev=="1_Dive"|subAcc$States_lev=="2_Dive"|subAcc$States_lev=="3_Dive"|subAcc$States_lev=="4_Dive"),]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_Dive"|subAcc$States_lev=="2_Dive"|subAcc$States_lev=="3_Dive"|subAcc$States_lev=="4_Dive"|subAcc$States_lev=="5_Dive"),]

subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))

myColors <-  lisa_palette("KarlZerbe")[c(4,3,5,1,2)]
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_fill_manual(name = "diving behavioural states", values = myColors,
                                   #labels=c("1_Dive"="Hunt", "2_Dive"="Ascend", "3_Dive"="Swim/Cruise while diving", "4_Dive"="Descend"))
                                     labels=c("1_Dive"="Descend", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Swim/Cruise type 2", "4_Dive"="Hunt","5_Dive"="Ascend"))

plotBox1<-ggplot() + theme_bw()+
  geom_boxplot(data=subAccPoint, aes(x=States_lev, y=(depth25Hz*-1),fill=States_lev),outlier.size = pointS,outlier.shape=1)+
  # scale_colour_igv()+
  custom_colors+
  scale_x_discrete( labels=c("1_Dive"="Descend", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Swim/Cruise type 2", "4_Dive"="Hunt","5_Dive"="Ascend"))+
  xlab("")+ylab("Depth (m)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))


plotBox2<-ggplot() + theme_bw()+
  geom_boxplot(data=subAccPoint, aes(x=States_lev, y=Pitch,fill=States_lev),outlier.size = pointS,outlier.shape=1)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Pitch (degrees)")+
  scale_x_discrete( labels=c("1_Dive"="Descend", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Swim/Cruise type 2", "4_Dive"="Hunt","5_Dive"="Ascend"))+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plotBox3<-ggplot() + theme_bw()+
  geom_boxplot(data=subAccPoint, aes(x=States_lev, y=VeDBA,fill=States_lev),outlier.size = pointS,outlier.shape=1)+
  # scale_colour_igv()+
  custom_colors+
  scale_x_discrete( labels=c("1_Dive"="Descend", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Swim/Cruise type 2", "4_Dive"="Hunt","5_Dive"="Ascend"))+
  xlab("")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))


ggarrange(plotBox1,plotBox2,plotBox3,ncol=1,common.legend = TRUE)















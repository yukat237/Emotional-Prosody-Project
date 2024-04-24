########################################################################
########################################################################

#    Emotional Prosody Project Stats (3rd Edition)       ----4/24/2024

########################################################################
########################################################################

# Goal: to combine Main study Data Analysis script and Acoustic Analysis script, with only necessary info.

######------------   workDir and libraries   -------------######

library("dplyr")
library("tidyr")
library("lme4")
library("ggpubr")
library("ggplot2")

setwd("C:/Users/yuka/iCloudDrive/休学2020/Emotional Prosody Project/")
options(max.print=1000000)


######------------  Main Behavioral Data Analysis  -------------######

#####--- Import and Clean Data ---######

####---Qultrix Behavioral data---####
surveyDat<-read.csv("DATA COLLECTION EXCELS/THE MAIN STUDY_October 12, 2021 Only needed data.csv")
dataDf<-surveyDat[2:69,] #Delete empty rows
dataDf[57,93]<- "5e63f5da8a25da181c011cad" #adding missed ID info

####---Prolific Demographic data---####
#Read all demo data
monoDemoDat<-read.csv("DATA COLLECTION EXCELS/prolific_export_6083b336fdf78028aa588fb4(Monolinguals).csv")
biDemoDat<-read.csv("DATA COLLECTION EXCELS/prolific_export_61585ce629c66bf74deea734 (Bilinguals).csv")
biDemoDat6<-read.csv("DATA COLLECTION EXCELS/prolific_export_615a34dcd82bccfa03177944 (6 additional survey).csv")
biDemoDat2<-read.csv("DATA COLLECTION EXCELS/prolific_export_61621b1307e242571834a449 (2 additional survey).csv")
#Only keep approved participants
monoDemoDat<-subset(monoDemoDat,monoDemoDat[,3]=="APPROVED")
biDemoDat<-subset(biDemoDat,biDemoDat[,3]=="APPROVED")
biDemoDat6<-subset(biDemoDat6,biDemoDat6[,3]=="APPROVED")
biDemoDat2<-subset(biDemoDat2,biDemoDat2[,3]=="APPROVED")
AllbiDemoDat<-rbind(biDemoDat,rbind(biDemoDat2,biDemoDat6))
AllDemoDat<-merge(AllbiDemoDat,monoDemoDat,all=T) #bi-mo Combined demo df

####---Combine Behavioral & Demo data---####
FULLdata<-merge(dataDf,AllDemoDat,by.x = "PROLIFIC_PID", by.y = "participant_id") #leaving only matching id with Prolific 

####---Omit data to drop---####
#monolinguals' data
monoOnlyofFull<-subset(FULLdata,FULLdata[,14]=="English monolingual (Only speaks English, not proficient in any other languages)" )
monoOnlyofFull<-monoOnlyofFull[monoOnlyofFull$Fluent.languages=="English",]
mono_timesorted<-monoOnlyofFull[order(monoOnlyofFull$RecordedDate),]
mono_minus11<-mono_timesorted[1:28,]
mono_sex_minus11<-as.data.frame(mono_minus11[,114])
count(mono_sex_minus11,mono_sex_minus11[,1]=="Female")
mono_finaldf<-mono_minus11
#bilinguals' data
biOnlyofFull<-subset(FULLdata, FULLdata[,14]!="English monolingual (Only speaks English, not proficient in any other languages)")
biOnlyofFull<-biOnlyofFull[biOnlyofFull$Fluent.languages=="English" | biOnlyofFull$Fluent.languages== "Spanish" | biOnlyofFull$Fluent.languages=="English, Spanish",]
bi_sex<-as.data.frame(biOnlyofFull[,114])
#combine bilinguals and monolinguals data again
finalDf<- rbind(mono_finaldf,biOnlyofFull)
#take out attention checkers
finalDf<-subset(finalDf,select=-c(Half,Almost))
rownames(finalDf)<-1:nrow(finalDf)
finalDf #this is the cleaned, original Behavioral data with demographics data attached

#####---making a new DF for analysis---#####
analysisdf<-data.frame(subjID=paste(1:52),
                       biormono= c(paste(rep("mono",28)),paste(rep("bi",24))), 
                       item11=finalDf$X9.1, #happy
                       item25=finalDf$X9.3, 
                       item53=finalDf$X9.5, 
                       item74=finalDf$X9.8, 
                       item81=finalDf$X9.9, 
                       item102=finalDf$X9.11, 
                       item123=finalDf$X9.14, 
                       item144=finalDf$X9.17, 
                       item151=finalDf$X9.18, 
                       item172=finalDf$X9.19, 
                       item193=finalDf$X9.21, 
                       item214=finalDf$X9.23, 
                       item221=finalDf$X9.24, 
                       item235=finalDf$X9.26, 
                       item242=finalDf$X9.27, 
                       item263=finalDf$X9.3.1, 
                       item13=finalDf$X9.2, #sad
                       item34=finalDf$X9.4, 
                       item55=finalDf$X9.6, 
                       item62=finalDf$X9.7, 
                       item83=finalDf$X9.1.1, 
                       item104=finalDf$X9.12, 
                       item111=finalDf$X9.13, 
                       item125=finalDf$X9.15, 
                       item132=finalDf$X9.16,
                       item174=finalDf$X9.2.1, 
                       item195=finalDf$X9.22, 
                       item223=finalDf$X9.25, 
                       item244=finalDf$X9.28, 
                       item251=finalDf$X9.29, 
                       item265=finalDf$X9.31, 
                       item272=finalDf$X9.32, 
                       item71=finalDf$X9.36, #anger
                       item85=finalDf$X9.38,
                       item92=finalDf$X9.39,
                       item113=finalDf$X9.4.1,
                       item141=finalDf$X9.47,
                       item63=finalDf$X9.34,#surp
                       item154=finalDf$X9.43,
                       item231=finalDf$X9.51,
                       item182=finalDf$X9.53,
                       item224=finalDf$X9.56,
                       item115=finalDf$X9.42, #fear
                       item122=finalDf$X9.44,
                       item171=finalDf$X9.48,
                       item192=finalDf$X9.49,
                       item262=finalDf$X9.54,
                       item65=finalDf$X9.35,   #disg
                       item114=finalDf$X9.41,
                       item135=finalDf$X9.46,
                       item254=finalDf$X9.52,
                       item275=finalDf$X9.57,
                       item61=finalDf$X9.33,  #neutral
                       item75=finalDf$X9.37,
                       item131=finalDf$X9.45,
                       item201=finalDf$X9.5.1,
                       item271=finalDf$X9.55
)              #this is the cleaned final dataframe for anlysis, with right column names,with subjID,with mono/bi info, only with answers



#####---Tweaking the DF more for stats---#####

#remake df for lmer (each row: subjID, bi or mono, item#, their resp, correct answer)
dfforlmer<-gather(analysisdf, key="items", value = "answer", -subjID, -biormono)

#add a column for whether if the answer is correct!
dfforlmer$corr_ans<-c(paste(rep("Happiness",832)),paste(rep("Sadness",832)),paste(rep("Anger",260)),paste(rep("Surprise",260)),paste(rep("Fear",260)),paste(rep("Disgust",260)),paste(rep("Neutral",260)))
corr_ornot_df<-data.frame(matrix(ncol=1,nrow=1))
for (n in 1:nrow(dfforlmer)){
  correctness_test<-identical(dfforlmer[n,4],dfforlmer[n,5])
  correctness_status<-ifelse(correctness_test == TRUE, "1", "0")
  corr_ornot_df[n,]<-correctness_status
}
dfforlmer[,6]<-corr_ornot_df
colnames(dfforlmer)[6]<-"corr_ornot"

##Accuracy per item
dfforlmer$corr_ornot<-as.numeric(dfforlmer$corr_ornot)
accperitem<- aggregate(corr_ornot~items+corr_ans, data=dfforlmer, sum)
accperitem$accuracy<-accperitem$corr_ornot/52
colnames(accperitem)[3]<-"howmany_corr"

##add accuracy by Japanese
accperitem$acc_jap<-c(100,100,100,100,100,100,80,80,80,80,70,70,90,80,80,70,60,70,70,60,70,100,60,70,60,70,70,80,60,60,80,100,100,100,100,100,90,100,100,90,100,100,90,100,90,100,100,90,100,100,100,90,100,100,100,90,90)

#Correlation with Japanese accuracy
cor.test(accperitem$accuracy,accperitem$acc_jap)
#results...sig correlation 0.0001 p value, r=0.486
#notes on 4/24-- is this actually picking up the right column?


####----visualization of accuracy by Japanese & accuracy by non-Jap----####
ggplot(accperitem, aes(x=accuracy, y=acc_jap)) +
  geom_point()+ labs(x="accuracy in Main study", y="accuracy in pilot" ) + 
  geom_smooth(method=lm)+ theme_bw()

#if wanna see emo by color
ggplot(accperitem, aes(x=accuracy, y=acc_jap, color=corr_ans)) +
  geom_point(size = 4)+ labs(x="accuracy in Main study", y="accuracy in pilot" ) +
  theme_bw()

#this does not look good at all but is an option?...
plot(accperitem$accuracy,accperitem$acc_jap)
abline(lm(acc_jap ~ accuracy, data = accperitem), col = "blue")


##making df for Accuracy by subj
dfforlmer$corr_ornot<-as.numeric(dfforlmer$corr_ornot)
accpersubj<- aggregate(corr_ornot~subjID+corr_ans, data=dfforlmer, sum)

#keep only Happy and Sad (for subj - Happ or Sad pair, how many correct? and accuracy)
accpersubj<-subset(accpersubj, accpersubj$corr_ans=="Happiness"|accpersubj$corr_ans=="Sadness")
accpersubj$accuracy<-accpersubj$corr_ornot/16
colnames(accpersubj)[3]<-"howmany_corr"

dfforlmer$corr_ornot<-as.numeric(dfforlmer$corr_ornot)
accpersubj2<-aggregate(corr_ornot~subjID+corr_ans+biormono, data=dfforlmer, sum)
accpersubj2<-subset(accpersubj2,accpersubj2$corr_ans=="Sadness"|accpersubj2$corr_ans=="Happiness")
accpersubj2$accuracy<-accpersubj2$corr_ornot/16 #add accuracy
newmainDF<-accpersubj2[,c(1,2,3,5)] #omit correct or not column
colnames(newmainDF)[2]<-"emo" #rewrite corr_ans to emo




#####---Main Behavioral Stats ---#####

####-----NEW-----####

##visualization
ggboxplot(newmainDF, x = "emo", y = "accuracy", 
          fill = "biormono",
          ylab = "Accuracy", xlab = "Valence")+
  scale_fill_manual(values = c("#f57178", "#80d3e8"),name="Language Background",breaks=c("bi","mono"),labels = c("Bilingual", "Monolingual")) +
  scale_x_discrete(labels=c("Positive","Negative"))
#I changed the color to try.., diff from what I initially had on google doc MAIN ARTICLE (if you want the original, just remove the color values)
#another blue ...6CB0D6


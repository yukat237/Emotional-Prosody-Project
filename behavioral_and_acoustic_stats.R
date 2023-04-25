########################################################################

#    Emotional Prosody Project Stats (2nd Edition)       ----11/26/2022

########################################################################

# Note (4/25/2023): some acoustic stats below are not making sense as I did not fully 
#                   understand the LMER at the point of writing (i was a junior)-- so they are to be modified!

# Goal: to combine Main study Data Analysis script and Acoustic Analysis script, with only necessary info.

######---   workDir and librareis    -----#######
library("plyr")
library("dplyr")
library("mosaic")
library("lme4")
library("ggpubr")
library("ggplot2")
library("stringr")
library("data.table")
library("tidyr")
library("boot")
library("car")
library("formattable")
library("clean")
library("gt")
library("cowplot")
library("gridExtra")
library("lmerTest")
library("ez")
library("rstatix")
setwd("C:/Users/tatsu/iCloudDrive/?x?w2020/Emotional Prosody Project/")
options(max.print=1000000)



######--- Main Behavioral Data Analysis -----######

#####--- Read and Clean Data ---######

####---Qultrix Behavioral data---####
surveyDat<-read.csv("DATA COLLECTION EXCELS/THE MAIN STUDY_October 12, 2021 Only needed data.csv")
dataDf<-surveyDat[2:69,] #Delete empty rows
dataDf[57,93]<- "5e63f5da8a25da181c011cad" #adding missed ID info

####---Prolific Demopgraphic data---####
#Read all demodata
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
finalDf #this is a cleaned, original Behavioral data with demographics data attached,

#####---new DF for analysis---#####
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
)              #cleaned final dataframe, with right column names,with subjID,with mono/bi info, only with answers

####----Prepping for stats------------####
#remake df
dfforlmer<-gather(analysisdf, key="items", value = "answer", -subjID, -biormono)
#add a column for whther if the answer is correct
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
#visualize
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
#keep only Happy and Sad
accpersubj<-subset(accpersubj, accpersubj$corr_ans=="Happiness"|accpersubj$corr_ans=="Sadness")
accpersubj$accuracy<-accpersubj$corr_ornot/16
colnames(accpersubj)[3]<-"howmany_corr"
accpersubj
dfforlmer$corr_ornot<-as.numeric(dfforlmer$corr_ornot)
accpersubj2<-aggregate(corr_ornot~subjID+corr_ans+biormono, data=dfforlmer, sum)
accpersubj2<-subset(accpersubj2,accpersubj2$corr_ans=="Sadness"|accpersubj2$corr_ans=="Happiness")
accpersubj2$accuracy<-accpersubj2$corr_ornot/16 #add accuracy
newmainDF<-accpersubj2[,c(1,2,3,5)] #omit correct or not column
colnames(newmainDF)[2]<-"emo" #rewrite corr_ans to emo


#####---Main Stats ---#####

####-----NEW-----####

##visualization
ggboxplot(newmainDF, x = "emo", y = "accuracy", 
          fill = "biormono",
          ylab = "Accuracy", xlab = "Valence")+
  scale_fill_manual(values = c("#f57178", "#80d3e8"),name="Language Background",breaks=c("bi","mono"),labels = c("Bilingual", "Monolingual")) +
  scale_x_discrete(labels=c("Positive","Negative"))
#I changed the color to try.., diff from what I initially had on google doc MAIN ARTICLE (if you want the original, just remove the color values)
#another blue ...6CB0D6

#---Clean the DF for Glmer---#

#origianl DF to use --> dfforlmer

#drop other emotion data
dfforGlmer<-dfforlmer%>%filter(corr_ans=="Happiness"|corr_ans=="Sadness")
#drop the raw response column
dfforGlmer<-dfforGlmer[,c(1:3,5,6)]
#rename colnames#
colnames(dfforGlmer)[which(names(dfforGlmer)=="corr_ans")]<-"valence" #independent variable2
colnames(dfforGlmer)[which(names(dfforGlmer)=="corr_ornot")]<-"accuracy" #independent variable1
colnames(dfforGlmer)[which(names(dfforGlmer)=="biormono")]<-"group" #dependent variable
colnames(dfforGlmer)[which(names(dfforGlmer)=="items")]<-"item"
#---Fix the item coding (same itemNum for same sentence1~20)----#
dfforGlmer$item[dfforGlmer$item=="item11"]<-"6"
dfforGlmer$item[dfforGlmer$item=="item25"] <-"5"
dfforGlmer$item[dfforGlmer$item=="item53" ]<-"8"
dfforGlmer$item[dfforGlmer$item=="item74" ]<-"9"
dfforGlmer$item[dfforGlmer$item=="item81" ]<-"1"
dfforGlmer$item[dfforGlmer$item=="item102"]<-"2"
dfforGlmer$item[dfforGlmer$item=="item123" ]<-"3"
dfforGlmer$item[dfforGlmer$item=="item144" ]<-"14"
dfforGlmer$item[dfforGlmer$item=="item151" ]<-"16"
dfforGlmer$item[dfforGlmer$item=="item172" ]<-"17"
dfforGlmer$item[dfforGlmer$item=="item193" ]<-"18"
dfforGlmer$item[dfforGlmer$item=="item214" ]<-"19"
dfforGlmer$item[dfforGlmer$item=="item221" ]<-"11"
dfforGlmer$item[dfforGlmer$item=="item235" ]<-"20"
dfforGlmer$item[dfforGlmer$item=="item242" ]<-"12"
dfforGlmer$item[dfforGlmer$item=="item263" ]<-"13" #Hap done
dfforGlmer$item[dfforGlmer$item=="item34" ]<-"9" #Sad from here
dfforGlmer$item[dfforGlmer$item=="item55" ]<-"10"
dfforGlmer$item[dfforGlmer$item=="item62" ]<-"2"
dfforGlmer$item[dfforGlmer$item=="item111" ]<-"6"
dfforGlmer$item[dfforGlmer$item=="item125" ]<-"5"
dfforGlmer$item[dfforGlmer$item=="item132" ]<-"7"
dfforGlmer$item[dfforGlmer$item=="item174" ]<-"19"
dfforGlmer$item[dfforGlmer$item=="item223" ]<-"13"
dfforGlmer$item[dfforGlmer$item=="item251" ]<-"16"
dfforGlmer$item[dfforGlmer$item=="item265" ]<-"15"
dfforGlmer$item[dfforGlmer$item=="item13" ]<-"8"
dfforGlmer$item[dfforGlmer$item=="item83" ]<-"3"
dfforGlmer$item[dfforGlmer$item=="item104" ]<-"4"
dfforGlmer$item[dfforGlmer$item=="item195" ]<-"20"
dfforGlmer$item[dfforGlmer$item=="item244" ]<-"14"
dfforGlmer$item[dfforGlmer$item=="item272" ]<-"17"#sad done

#---Turn into factor---#
dfforGlmer$subjID<-as.factor(dfforGlmer$subjID)
dfforGlmer$valence<-as.factor(dfforGlmer$valence)
dfforGlmer$group<-as.factor(dfforGlmer$group)
dfforGlmer$item<-as.factor(dfforGlmer$item)

#---Contrast coding---#
contrasts(dfforGlmer$group)<-contr.Sum(levels(dfforGlmer$group))
contrasts(dfforGlmer$valence)<-contr.Sum(levels(dfforGlmer$valence))

#---Glmer models---#

m_max<-glmer(accuracy~1+group*valence+(1+valence|subjID)+(1+valence|item),
             data=dfforGlmer,family = binomial, 
             control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1e6)),verbose = 1)
#this fits!
m_max_summary<-summary(m_max)

#just in case I am trying more complicated ones...
m_complex1<-glmer(accuracy~1+group*valence+(1+valence|subjID)+(1+group*valence|item),
                  data=dfforGlmer,family = binomial, 
                  control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1e6)),verbose = 1)
#this gave me: boundary (singular) fit: see ?isSingular. so I did isSingular(m_complex1) and it returned TRUE

#Now I try non-interaction one.


m_max2<-glmer(accuracy~1+group+valence+(1+valence|subjID)+(1+valence|item),
              data=dfforGlmer,family = binomial, 
              control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1e6)),verbose = 1)

accuracy~1+group+valence+(1+valence|subjID)+(1+valence|item)

#this fits!!

#Model comparison with anova
m_comp<-anova(m_max,m_max2)
#chi sq = 0.5911, df = 1, p = 0.442
#interaction is NOT making the model any better.



#---Exploring More-than-chance Rate---#
mtcr<-aggregate(accuracy~subjID, data=newmainDF, mean)
summary(mtcr)
sd(mtcr$accuracy)
#this shows all participants accuracy (thruout the experiment) are ALL over chance (1/7=0.14)
#mean 0.5367
#sd 0.1122
#range0.2500-0.7812

####-----OLD-----####

#2 by 2 simple ANOVA
newanova_byacc1<-aov(accuracy~emo*biormono,data=newmainDF)
summary(newanova_byacc1)
TukeyHSD(newanova_byacc1)
#possible issue: size diff, not considering within between

#lmer
newlme1<-lmer(accuracy~emo*biormono+(1|subjID),data=newmainDF)
#is this really right (for my variables)?  How do I see the results? 

#2 by 2 ANOVA (considers within between,type 3 to consider unbalanced)...using library("rstatix")
newanova_byacc2 <- anova_test(data = newmainDF, dv = accuracy, wid = subjID,
                              between = biormono, within = emo)
get_anova_table(newanova_byacc2)

#2 by 2 ANOVA
newanova_byacc3<-ezANOVA(data=newmainDF,dv= .(accuracy),wid = .(subjID), between=.(biormono),within=.(emo))

#2 by 2 ANOVA
newanova_byacc4<- anova_test(accuracy~subjID+emo*biormono, data=newmainDF)

##visualization
ggboxplot(newmainDF, x = "emo", y = "accuracy", 
          fill = "biormono", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Accuracy", xlab = "Emotion")+
  scale_fill_discrete(name="Language Background",labels = c("Bilingual", "Monolingual")) +
  scale_x_discrete(labels=c("Happiness","Sadness"))


#to see if above chance rate
#sad only
accpersubj2SAD<-subset(accpersubj2,accpersubj2$corr_ans=="Sadness")
mean(accpersubj2SAD$accuracy)
#0.7560096
sd(accpersubj2SAD$accuracy)
#0.1542195      
#one-sample t-test

accpersubj2HAP<-subset(accpersubj2,accpersubj2$corr_ans=="Happiness")
mean(accpersubj2HAP$accuracy)
#0.3173077
sd(accpersubj2HAP$accuracy)
#0.1569695



######--- Acoustic Data Analysis -----######

#####----- Read Data ------###

####--- Read Table ----####

acoustics_tbl<-read.table("For acoustic analysis/measuresTable_ver2.txt")
forcolname<-as.character(acoustics_tbl[1,])
acoustics_tbl<-read.table("For acoustic analysis/measuresTable_ver2.txt",skip=1)
colnames(acoustics_tbl)<-forcolname

###----Clean the table-----##

#add emo column
emo_col<-data.frame(matrix(ncol=1,nrow=1))
for (n in 1:nrow(acoustics_tbl)){
  emotest<-grepl("sad", acoustics_tbl[n,1])
  emotest_status<-ifelse(emotest == TRUE, "sad", "happy")
  emo_col[n,]<-emotest_status
  
}
acoustics_tbl[31]<-emo_col
colnames(acoustics_tbl)[31]<-"emotion"

#add item_num
itemnum_col<-c("item55","item221","item242","item263","item223",
               "item144","item244","item265","item151","item251"
               ,"item172","item272","item193","item214","item174",
               "item81","item235","item195","item102","item62","item123","item83",
               "item104","item25","item125","item11","item111","item132","item53","item13","item74","item34")
acoustics_tbl$itemNum<-itemnum_col
actbl<-acoustics_tbl #rename the df name


#####--- Stats ---#####

####----part1.Is sadness sig different from happiness, for each acoustic measure?----####

f0mean_agg<-aggregate(f0Mean~emotion,data= actbl,function(x)c(mean(x),sd(x)))
f0mean_agg<-data.frame(emotion= f0mean_agg$emotion, mean=f0mean_agg$f0Mean[,1], sd=f0mean_agg$f0Mean[,2])
t.test(f0Mean~emotion,data=actbl,paired=T)
#p-value = 0.05763
f0meanaov<-aov(f0Mean~emotion, data=actbl)
TukeyHSD(f0meanaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "f0Mean", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "F0 Mean", xlab = "Emotion")

f0Range_agg<-aggregate(f0Range~emotion,data= actbl, function(x)c(mean(x),sd(x)))
f0Range_agg<-data.frame(emotion= f0Range_agg$emotion, mean=f0Range_agg$f0Range[,1], sd=f0Range_agg$f0Range[,2])
t.test(f0Range~emotion,data=actbl,paired=T)
#p-value = 0.8699
ggboxplot(actbl, x = "emotion", y = "f0Range", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "F0 Range", xlab = "Emotion")

f0SD_agg<-aggregate(f0SD~emotion,data= actbl, function(x)c(mean(x),sd(x)))
f0SD_agg<-data.frame(emotion= f0SD_agg$emotion, mean=f0SD_agg$f0SD[,1], sd=f0SD_agg$f0SD[,2])
t.test(f0SD~emotion,data=actbl,paired=T)
#p-value = 7.167e-08 SIGGGGGGGGG
f0sdaov<-aov(f0SD~emotion, data=actbl)
TukeyHSD(f0sdaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "f0SD", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "f0 SD", xlab = "Emotion")

intensityMean_agg<-aggregate(intensityMean~emotion,data= actbl, function(x)c(mean(x),sd(x)))
intensityMean_agg<-data.frame(emotion= intensityMean_agg$emotion, mean=intensityMean_agg$intensityMean[,1], sd=intensityMean_agg$intensityMean[,2])
t.test(intensityMean~emotion,data=actbl,paired=T)
#p-value = 0.0003285 SIGGGGGGGGG
Intmeanaov<-aov(intensityMean~emotion, data=actbl)
TukeyHSD(Intmeanaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "intensityMean", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Mean Intensity", xlab = "Emotion")

intensitySD_agg<-aggregate(intensitySD~emotion,data= actbl, function(x)c(mean(x),sd(x)))
intensitySD_agg<-data.frame(emotion= intensitySD_agg$emotion, mean=intensitySD_agg$intensitySD[,1], sd=intensitySD_agg$intensitySD[,2])
t.test(intensitySD~emotion,data=actbl,paired=T)
#p-value = 0.001226 SIGGGGGGGGG
Intsdaov<-aov(intensitySD~emotion, data=actbl)
TukeyHSD(Intsdaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "intensitySD", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "SD Intensity", xlab = "Emotion")

intensityRange_agg<-aggregate(intensityRange~emotion,data= actbl, function(x)c(mean(x),sd(x)))
intensityRange_agg<-data.frame(emotion= intensityRange_agg$emotion, mean=intensityRange_agg$intensityRange[,1], sd=intensityRange_agg$intensityRange[,2])
t.test(intensityRange~emotion,data=actbl,paired=T)
#p-value =  0.7031
ggboxplot(actbl, x = "emotion", y = "intensityRange", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Intensity Range", xlab = "Emotion")


duration_agg<-aggregate(duration~emotion,data= actbl, function(x)c(mean(x),sd(x)))
duration_agg<-data.frame(emotion= duration_agg$emotion, mean=duration_agg$duration[,1], sd=duration_agg$duration[,2])
t.test(duration~emotion,data=actbl,paired=T)
#p-value =  0.001271 (sad sig longer)
ggboxplot(actbl, x = "emotion", y = "duration", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Duration", xlab = "Emotion")


HNRmean_agg<-aggregate(HNRmean~emotion,data= actbl, function(x)c(mean(x),sd(x)))
HNRmean_agg<-data.frame(emotion= HNRmean_agg$emotion, mean=HNRmean_agg$HNRmean[,1], sd=HNRmean_agg$HNRmean[,2])
t.test(HNRmean~emotion,data=actbl,paired=T)
#p-value = 0.01353 SIGGGGG
hnrmeanaov<-aov(HNRmean~emotion, data=actbl)
TukeyHSD(hnrmeanaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "HNRmean", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Intensity Range", xlab = "Emotion")  #SAD more turbulent noise
#The HNR is an assessment of the ratio between periodic components and non periodic component
#comprising a segment of voiced speech, as Murphy and Akande [6]. The first component arises from the
#vibration of the vocal cords and the second follows from the glottal noise, expressed in dB. The evaluation
#between the two components reflects the efficiency of speech, i.e., the greater the flow of air expelled from the
#lungs into energy of vibration of vocal cords. In these cases the HNR will be greater. 

#Harmonics-to-noise ratio (HNR) is a measure of the proportion between
#harmonic and nose components in the voice, and is related to excessive airflow, or turbulence, through the glottis during vowel articulation. HNR is
#computed as the ratio of the F0-amplitude (amplitude of the main harmonic)
#over the summed amplitude across the other frequency bands. HNR has been
#associated with perception of voice ?gbreathiness?h (Barsties v. Latoszek, et  al., 2018)


HNRsd_agg<-aggregate(HNRsd~emotion,data= actbl, function(x)c(mean(x),sd(x)))
HNRsd_agg<-data.frame(emotion= HNRsd_agg$emotion, mean=HNRsd_agg$HNRsd[,1], sd=HNRsd_agg$HNRsd[,2])
t.test(HNRsd~emotion,data=actbl,paired=T)
#p-value = 0.9287 
ggboxplot(actbl, x = "emotion", y = "HNRsd", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "HNR SD", xlab = "Emotion")  

SpeechRate_agg<-aggregate(SpeechRate~emotion,data= actbl, function(x)c(mean(x),sd(x)))
SpeechRate_agg<-data.frame(emotion= SpeechRate_agg$emotion, mean=SpeechRate_agg$SpeechRate[,1], sd=SpeechRate_agg$SpeechRate[,2])
t.test(SpeechRate~emotion,data=actbl,paired=T)
#p-value = 0.001271 SIGGGGGGGGGGGGGG
SRaov<-aov(SpeechRate~emotion, data=actbl)
TukeyHSD(SRaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "SpeechRate", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Speech Rate", xlab = "Emotion")


JitterLocal_agg<-aggregate(JitterLocal~emotion,data= actbl, function(x)c(mean(x),sd(x)))
JitterLocal_agg<-data.frame(emotion= JitterLocal_agg$emotion, mean=JitterLocal_agg$JitterLocal[,1], sd=JitterLocal_agg$JitterLocal[,2])
t.test(JitterLocal~emotion,data=actbl,paired=T)
#p-value = 0.009624 SIGGGGGGGGGGGGGG
Jitaov<-aov(JitterLocal~emotion, data=actbl)
TukeyHSD(Jitaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "JitterLocal", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "JitterLocal", xlab = "Emotion")


JitterLocal_agg<-aggregate(JitterLocal~emotion,data= actbl, function(x)c(mean(x),sd(x)))
JitterLocal_agg<-data.frame(emotion= JitterLocal_agg$emotion, mean=JitterLocal_agg$JitterLocal[,1], sd=JitterLocal_agg$JitterLocal[,2])
t.test(JitterLocal~emotion,data=actbl,paired=T)
#p-value = 0.009624 SIGGGGGGGGGGGGGG
ggboxplot(actbl, x = "emotion", y = "JitterLocal", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "JitterLocal", xlab = "Emotion")
#Jitter is a perturbation measure computed as the average window-towindow deviations in consecutive F0 time windows. 
# Presence of jitter is #associated with perceived ?groughness?h of the voice (Barsties v. Latoszek, Maryn, Gerrits, & De Bodt, 2018)


ShimmerLocal_agg<-aggregate(ShimmerLocal~emotion,data= actbl, function(x)c(mean(x),sd(x)))
ShimmerLocal_agg<-data.frame(emotion= ShimmerLocal_agg$emotion, mean=ShimmerLocal_agg$ShimmerLocal[,1], sd=ShimmerLocal_agg$ShimmerLocal[,2])
t.test(ShimmerLocal~emotion,data=actbl,paired=T)
#p-value = 0.8343
ggboxplot(actbl, x = "emotion", y = "ShimmerLocal", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "ShimmerLocal", xlab = "Emotion")
#Shimmer is, like jitter, a perturbation measure that is associated with perceived ?groughness?h.
#Unlike jitter though, which measures pitch-instability, shimmer measures amplitude-instability.
#It is computed in a similar way as jitter but uses the average window-to-window deviations in peak amplitudes of consecutive F0 time windows


AlphaRatio_agg<-aggregate(AlphaRatio~emotion,data= actbl, function(x)c(mean(x),sd(x)))
AlphaRatio_agg<-data.frame(emotion= AlphaRatio_agg$emotion, mean=AlphaRatio_agg$AlphaRatio[,1], sd=AlphaRatio_agg$AlphaRatio[,2])
t.test(AlphaRatio~emotion,data=actbl,paired=T)
#p-value = 0.1625
ggboxplot(actbl, x = "emotion", y = "AlphaRatio", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "AlphaRatio", xlab = "Emotion")
#pectral balance parameters:
#Alpha Ratio, Hammarberg Index, Spectral slope, energy proportion, and
#Harmonic difference are all measures of what is perceived as ?gsoft?h- or
#?gsharpness?h of a voice. These measures are sometimes referred to as
#measures of ?ghigh frequency energy?h because they generally measure the
#proportion of energy below versus above a certain cut-off in the frequency
#spectra. 

colnames(actbl)[30]<-"H1H2"
H1H2_agg<-aggregate(H1H2~emotion,data= actbl, function(x)c(mean(x),sd(x)))
H1H2_agg<-data.frame(emotion= H1H2_agg$emotion, mean=H1H2_agg$H1H2[,1], sd=H1H2_agg$H1H2[,2])
t.test(H1H2~emotion,data=actbl,paired=T)
#p-value =0.7308
ggboxplot(actbl, x = "emotion", y = "H1H2", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "H1-H2", xlab = "Emotion")

SpeechRate_agg<-aggregate(SpeechRate~emotion,data= actbl, function(x)c(mean(x),sd(x)))
SpeechRate_agg<-data.frame(emotion= SpeechRate_agg$emotion, mean=SpeechRate_agg$SpeechRate[,1], sd=SpeechRate_agg$SpeechRate[,2])
t.test(SpeechRate~emotion,data=actbl,paired=T)
#p-value = 0.001271 SIGGGGGGGGGGGGGG
SRaov<-aov(SpeechRate~emotion, data=actbl)
TukeyHSD(SRaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "SpeechRate", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Speech Rate", xlab = "Emotion")

SpectralSlope_agg<-aggregate(SpectralSlope~emotion,data= actbl, function(x)c(mean(x),sd(x)))
SpectralSlope_agg<-data.frame(emotion= SpectralSlope_agg$emotion, mean=SpectralSlope_agg$SpectralSlope[,1], sd=SpectralSlope_agg$SpectralSlope[,2])
t.test(SpectralSlope~emotion,data=actbl,paired=T)
#p-value = 0.001271 SIGGGGGGGGGGGGGG
SSaov<-aov(SpectralSlope~emotion, data=actbl)
TukeyHSD(SSaov,"emotion")
ggboxplot(actbl, x = "emotion", y = "SpectralSlope", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "Spectral Slope", xlab = "Emotion")


acous_desc_results<-data.frame(matrix(ncol=1,nrow=1))
for (n in 1:nrow(acoustics_tbl)){
  emotest<-grepl("sad", acoustics_tbl[n,1])
  emotest_status<-ifelse(emotest == TRUE, "sad", "happy")
  emo_col[n,]<-emotest_status
}





####----part2.which acoustic features are different between the "Hardest"(lowest accuracy)item vs "Easiest" (highest accuracy)item?----####

##Happy item
Happyaccdf<-subset(accperitem, accperitem$corr_ans=="Happiness")
Happyaccdf<-Happyaccdf[order(Happyaccdf$accuracy),]
#item214: 0% accuracy
#item74: 0.69 (more than Jap!60%) accuracy

##Sad item
Sadaccdf<-subset(accperitem, accperitem$corr_ans=="Sadness")
Sadaccdf<-Sadaccdf[order(Sadaccdf$accuracy),]
#item13: 48% accuracy
#item272: 92.3%accuracy

Hard_easy_comp_DF<-subset(actbl,actbl$itemNum== "item214"|actbl$itemNum=="item74"|actbl$itemNum=="item13"|actbl$itemNum=="item272")
#Q?Fhow do I do stats for this...??
#at least, just by looking at the table, f0range, intenstiy SD, intensityRange, SpectalSlope, AlphaRatio, H1H2maybe?


####----part3.Was accuracy correlated with any of the acoustic measures? (looking at bi and mo separately)----####

#---prep---#
#getting accuracy
dfforlmer$corr_ornot<-as.numeric(dfforlmer$corr_ornot)
accperitem2<-aggregate(corr_ornot~items+corr_ans+biormono, data=dfforlmer, sum)
#adding accuracy 
accperitem2$accuracy<-accperitem2$corr_ornot/52
#keep only sad and hap
accperitem2<-subset(accperitem2,accperitem2$corr_ans=="Sadness"|accperitem2$corr_ans=="Happiness")
#attach acoustic measure results to each item
ac_newdf<-merge(accperitem2,actbl,by.x = "items",by.y="itemNum") #this is hap sad only?Abi/mono diff accuracy?Aper-item acoustics measure DF
#separating bi and mono #?????????F???????????Ɉȉ??????��????????????????ˁAmain results null???��??????B
biac_newdf<-ac_newdf[ac_newdf$biormono=="bi",]
monoac_newdf<-ac_newdf[ac_newdf$biormono=="mono",]


#----visualize & stats correlations--#

#f0mean
bif0m<-ggplot(biac_newdf, aes(x=f0Mean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0m<-ggplot(monoac_newdf, aes(x=f0Mean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0m, monof0m, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$f0Mean, biac_newdf$accuracy)
cor.test(monoac_newdf$f0Mean, monoac_newdf$accuracy)
#both nonsig


#f0Range
bif0R<-ggplot(biac_newdf, aes(x=f0Range, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0R<-ggplot(monoac_newdf, aes(x=f0Range, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0R, monof0R, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$f0Range,biac_newdf$accuracy)
cor.test(monoac_newdf$f0Range,monoac_newdf$accuracy)  
#both nonsig

#f0SD
bif0SD<-ggplot(biac_newdf, aes(x=f0SD, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0SD<-ggplot(monoac_newdf, aes(x=f0SD, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0SD, monof0SD, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$f0SD,biac_newdf$accuracy)
cor.test(monoac_newdf$f0SD,monoac_newdf$accuracy)  
#BOTH super sig. interesting distribution


#intensityMean
bif0Intm<-ggplot(biac_newdf, aes(x=intensityMean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0Intm<-ggplot(monoac_newdf, aes(x=intensityMean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0Intm, monof0Intm, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$intensityMean,biac_newdf$accuracy)
cor.test(monoac_newdf$intensityMean,monoac_newdf$accuracy)  
#both super sig correlated

#intensitySD
bif0Intsd<-ggplot(biac_newdf, aes(x=intensitySD, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0Intsd<-ggplot(monoac_newdf, aes(x=intensitySD, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0Intsd, monof0Intsd, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$intensitySD,biac_newdf$accuracy)
cor.test(monoac_newdf$intensitySD,monoac_newdf$accuracy)  
#Both SUPRE SIG

#intensityRange
bif0IntR<-ggplot(biac_newdf, aes(x=intensityRange, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monof0IntR<-ggplot(monoac_newdf, aes(x=intensityRange, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bif0IntR, monof0IntR, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$intensityRange,biac_newdf$accuracy)
cor.test(monoac_newdf$intensityRange,monoac_newdf$accuracy)  
#both not sig

#duration
bidu<-ggplot(biac_newdf, aes(x=duration, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monodu<-ggplot(monoac_newdf, aes(x=duration, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(bidu, monodu, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$duration,biac_newdf$accuracy)
cor.test(monoac_newdf$duration,monoac_newdf$accuracy)  
#interesting distribution, both not sig



#HNRmean
biHNRm<-ggplot(biac_newdf, aes(x=HNRmean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoNHRm<-ggplot(monoac_newdf, aes(x=HNRmean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biHNRm, monoNHRm, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$HNRmean,biac_newdf$accuracy)
cor.test(monoac_newdf$HNRmean,monoac_newdf$accuracy)  
#both not sig

#HNRsd
biHNRsd<-ggplot(biac_newdf, aes(x=HNRmean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoNHRsd<-ggplot(monoac_newdf, aes(x=HNRmean, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biHNRsd, monoNHRsd, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$HNRsd,biac_newdf$accuracy)
cor.test(monoac_newdf$HNRsd,monoac_newdf$accuracy)  
#both nonsig


#SpeechRate
biHNRsd<-ggplot(biac_newdf, aes(x=SpeechRate, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoNHRsd<-ggplot(monoac_newdf, aes(x=SpeechRate, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biHNRsd, monoNHRsd, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$SpeechRate,biac_newdf$accuracy)
cor.test(monoac_newdf$SpeechRate,monoac_newdf$accuracy)  
#both nonsig

# JitterLocal
biJit<-ggplot(biac_newdf, aes(x=JitterLocal, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoJit<-ggplot(monoac_newdf, aes(x=JitterLocal, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biJit, monoJit, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$JitterLocal,biac_newdf$accuracy)
cor.test(monoac_newdf$JitterLocal,monoac_newdf$accuracy)  
#both nonsig

#ShimmerLocal
biShim<-ggplot(biac_newdf, aes(x=ShimmerLocal, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoShim<-ggplot(monoac_newdf, aes(x=ShimmerLocal, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biShim, monoShim, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$ShimmerLocal,biac_newdf$accuracy)
cor.test(monoac_newdf$ShimmerLocal,monoac_newdf$accuracy)  
#Both nonsig

#SpectralSlope
biSS<-ggplot(biac_newdf, aes(x=SpectralSlope, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoSS<-ggplot(monoac_newdf, aes(x=SpectralSlope, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biSS, monoSS, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$SpectralSlope,biac_newdf$accuracy)
cor.test(monoac_newdf$SpectralSlope,monoac_newdf$accuracy)  
#BOTH SIG!!!!!!!


#AlphaRatio
biAR<-ggplot(biac_newdf, aes(x=AlphaRatio, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoAR<-ggplot(monoac_newdf, aes(x=AlphaRatio, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biAR, monoAR, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$AlphaRatio,biac_newdf$accuracy)
cor.test(monoac_newdf$AlphaRatio,monoac_newdf$accuracy)  
#both nonsig


#H1-H2
biH12<-ggplot(biac_newdf, aes(x=H1H2, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
monoH12<-ggplot(monoac_newdf, aes(x=H1H2, y=accuracy, color=corr_ans)) +
  geom_point() + 
  geom_smooth(method=lm,se=F)
plot_grid(biH12, monoH12, labels = c("bilingual","monolingual"))

cor.test(biac_newdf$H1H2,biac_newdf$accuracy)
cor.test(monoac_newdf$H1H2,monoac_newdf$accuracy)  
#non sig









####----part4.Is accuracy affected by any acoustic measures? (sad and hap separate, mono/bi as a factor)-random ---####
#DF to use
ac_newdf
happy_acacDf<-subset(ac_newdf, ac_newdf$corr_ans=="Happiness")
sad_acacDf<-subset(ac_newdf, ac_newdf$corr_ans=="Sadness")
#?????????Fmonobi??factor?Ƃ??Ȃ?ver?ł????Ȃ??????????????Ǝv???B
#Q: overall, i did both anova and lmer but what is best? also even if these are the right ones, are they right? how to look at the results
#Q: in visualizations, why mono is always better? bec more data?

###---No Mo/bi factor(new) ---###

#-working log-#
# 11/28/22 9:14...copied and pasted the old ver.
# 11/28/22 9:29...reran f0mean

#f0mean---#

#lmer
hapf0meanlmer2<-lmer(accuracy~f0Mean+(1|items),happy_acacDf)
summary(hapf0meanlmer2)
anova(hapf0meanlmer2)
#Sig - the lower the f0 mean, the higher the accuracy
sadof0meanlmer2<-lmer(accuracy~f0Mean+(1|items),sad_acacDf)
summary(sadof0meanlmer2)
anova(sadof0meanlmer2)
#Nonsig

#Visual
vishap_f0mean2<-ggplot(happy_acacDf,aes(f0Mean,accuracy))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
vissad_f0mean2<-ggplot(sad_acacDf,aes(f0Mean,accuracy))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(vishap_f0mean2,vissad_f0mean2, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#f0SD------#
hapf0SDlmer<-lmer(accuracy~f0SD*biormono+(1|items),happy_acacDf)
summary(hapf0SDlmer)
anova(hapf0SDlmer)
#non sig

sadf0SDlmer<-lmer(accuracy~f0SD*biormono+(1|items),sad_acacDf)
summary(sadf0SDlmer)
anova(sadf0SDlmer)
#bi-mono sig (mono higher in acc)=>NO SIG!!!!!!!!!!!!

#Visual
v3<-ggplot(happy_acacDf,aes(f0SD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v4<-ggplot(sad_acacDf,aes(f0SD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v3,v4, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#f0Range---------#

hapf0Rangelmer<-lmer(accuracy~f0Range*biormono+(1|items),happy_acacDf)
summary(hapf0Rangelmer)
anova(hapf0Rangelmer)
#non sig

sadf0Rangelmer<-lmer(accuracy~f0Range*biormono+(1|items),sad_acacDf)
summary(sadf0Rangelmer)
anova(sadf0Rangelmer)
#f0range sig, biormono sig.

#Visual
v5<-ggplot(happy_acacDf,aes(f0Range,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v6<- ggplot(sad_acacDf,aes(f0Range,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v5,v6, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensityMean-----------#
hapintMeanlmer<-lmer(accuracy~intensityMean*biormono+(1|items),happy_acacDf)
summary(hapintMeanlmer)
anova(hapintMeanlmer)
#intmean...marginal

sadintMeanlmer<-lmer(accuracy~intensityMean*biormono+(1|items),sad_acacDf)
summary(sadintMeanlmer)
anova(sadintMeanlmer)
#intMean super sig (int higher, acc lower), biormono sig. (mono>bi)=> JUST int MEAN!!!!!!

#Visual
v7<-ggplot(happy_acacDf,aes(intensityMean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v8<-ggplot(sad_acacDf,aes(intensityMean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v7,v8, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensitySD----------#

hapintSDlmer<-lmer(accuracy~intensitySD*biormono+(1|items),happy_acacDf)
summary(hapintSDlmer)
anova(hapintSDlmer)
#intsd...marginal

sadintSDlmer<-lmer(accuracy~intensitySD*biormono+(1|items),sad_acacDf)
summary(sadintSDlmer)
anova(sadintSDlmer)
#intsd super sig (int higher, acc lower), biormono sig. (mono>bi)

#Visual
v8<-ggplot(happy_acacDf,aes(intensitySD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v9<-ggplot(sad_acacDf,aes(intensitySD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v8,v9, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensityRange----------#

#duration-----------#
hapdurlmer<-lmer(accuracy~duration*biormono+(1|items),happy_acacDf)
summary(hapdurlmer)
anova(hapdurlmer)
#dur sig*** (shorter, more accurate)

saddurSDlmer<-lmer(accuracy~duration*biormono+(1|items),sad_acacDf)
summary(saddurSDlmer)
anova(saddurSDlmer)
#bi mono sig* (mono>bi)

#Visual
v10<- ggplot(happy_acacDf,aes(duration,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v11<-ggplot(sad_acacDf,aes(duration,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v10,v11, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#HNRmean--------#
hapHNRmlmer<-lmer(accuracy~HNRmean*biormono+(1|items),happy_acacDf)
summary(hapHNRmlmer)
anova(hapHNRmlmer)
#nonsig

sadHNRmlmer<-lmer(accuracy~HNRmean*biormono+(1|items),sad_acacDf)
summary(sadHNRmlmer)
anova(sadHNRmlmer)
#bimono (mono>bi)=>NO SIG

#Visual
v12<- ggplot(happy_acacDf,aes(HNRmean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v13<-ggplot(sad_acacDf,aes(HNRmean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v12,v13, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#HNRsd------------------#
hapHNRsdlmer<-lmer(accuracy~HNRsd*biormono+(1|items),happy_acacDf)
summary(hapHNRsdlmer)
anova(hapHNRsdlmer)
#nonsig

sadHNRsdlmer<-lmer(accuracy~HNRsd*biormono+(1|items),sad_acacDf)
summary(sadHNRsdlmer)
anova(sadHNRsdlmer)
#bimono (mono>bi)=>NO SIG


#SpeechRate----------------#

hapSRlmer<-lmer(accuracy~SpeechRate*biormono+(1|items),happy_acacDf)
summary(hapSRlmer)
anova(hapSRlmer)
#bi or mono (mono>bi)=>>non. but MAIN YES!!!!!!!!SIG!!!!

sadSRlmer<-lmer(accuracy~SpeechRate*biormono+(1|items),sad_acacDf)
summary(sadSRlmer)
anova(sadSRlmer)
#bi or mono (mono>bi)=> non

#Visual
v14<-ggplot(happy_acacDf,aes(SpeechRate,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v15<-ggplot(sad_acacDf,aes(SpeechRate,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v14,v15, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#JitterLocal

hapJitlmer<-lmer(accuracy~JitterLocal*biormono+(1|items),happy_acacDf)
summary(hapJitlmer)
anova(hapJitlmer)
#non sig

sadJitlmer<-lmer(accuracy~JitterLocal*biormono+(1|items),sad_acacDf)
summary(sadJitlmer)
anova(sadJitlmer)
#bi mono (mono>bi)

#Visual
v16<-ggplot(happy_acacDf,aes(JitterLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v17<- ggplot(sad_acacDf,aes(JitterLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v16,v17, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#ShimmerLocal----------#
hapShimlmer<-lmer(accuracy~ShimmerLocal*biormono+(1|items),happy_acacDf)
summary(hapShimlmer)
anova(hapShimlmer)
#non sig

sadShimlmer<-lmer(accuracy~ShimmerLocal*biormono+(1|items),sad_acacDf)
summary(sadShimlmer)
anova(sadShimlmer)
#bi mono (mono>bi)=> non sig

#Visual
v18<-ggplot(happy_acacDf,aes(ShimmerLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v19<-ggplot(sad_acacDf,aes(ShimmerLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v18,v19, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#SpectralSlope---------------#
hapSSlmer<-lmer(accuracy~SpectralSlope*biormono+(1|items),happy_acacDf)
summary(hapSSlmer)
anova(hapSSlmer)
#non sig

sadSSlmer<-lmer(accuracy~SpectralSlope*biormono+(1|items),sad_acacDf)
summary(sadSSlmer)
anova(sadSSlmer)
#bi mono (mono>bi)=> no bimono. BUT MAIN approached SGIG!!!!!

#Visual
v20<-ggplot(happy_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v21<-ggplot(sad_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v20,v21, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#AlphaRatio

hapARlmer<-lmer(accuracy~AlphaRatio*biormono+(1|items),happy_acacDf)
summary(hapARlmer)
anova(hapARlmer)
#non sig

sadARlmer<-lmer(accuracy~AlphaRatio*biormono+(1|items),sad_acacDf)
summary(sadARlmer)
anova(sadARlmer)
#bi mono (mono>bi)==>> NO SIG

#Visual
v22<-ggplot(happy_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v23<-ggplot(sad_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v22,v23, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#H1H2

hapHHlmer<-lmer(accuracy~H1H2*biormono+(1|items),happy_acacDf)
summary(hapHHlmer)
anova(hapHHlmer)
#non sig

sadHHlmer<-lmer(accuracy~H1H2*biormono+(1|items),sad_acacDf)
summary(sadHHlmer)
anova(sadHHlmer)
#bi mono (mono>bi)

#Visual
v24<-ggplot(happy_acacDf,aes(H1H2,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v25<-ggplot(sad_acacDf,aes(H1H2,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v24,v25, labels = c("happy", "sad"),ncol = 2, nrow = 1)




###---Mo/bi as factor(old)---###


#f0mean---#

#anova
hapf0meananova<-anova_test(data = happy_acacDf, dv = accuracy, wid = items, within = c(f0Mean,biormono))

#lmer
hapf0meanlmer<-lmer(accuracy~f0Mean*biormono+(1|items),happy_acacDf)
summary(hapf0meanlmer)
anova(hapf0meanlmer)
#f0Mean sig (the higher f0, the lower accuracy)
sadof0meanlmer<-lmer(accuracy~f0Mean*biormono+(1|items),sad_acacDf)
summary(sadof0meanlmer)
anova(sadof0meanlmer)
#bi-mono sig (mono higher in acc)=>NO!!!!!!!!!!!!!!!

#Visual
vishap_f0mean<-ggplot(happy_acacDf,aes(f0Mean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
vissad_f0mean<-ggplot(sad_acacDf,aes(f0Mean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(vishap_f0mean,vissad_f0mean, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#f0SD------#
hapf0SDlmer<-lmer(accuracy~f0SD*biormono+(1|items),happy_acacDf)
summary(hapf0SDlmer)
anova(hapf0SDlmer)
#non sig

sadf0SDlmer<-lmer(accuracy~f0SD*biormono+(1|items),sad_acacDf)
summary(sadf0SDlmer)
anova(sadf0SDlmer)
#bi-mono sig (mono higher in acc)=>NO SIG!!!!!!!!!!!!

#Visual
v3<-ggplot(happy_acacDf,aes(f0SD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v4<-ggplot(sad_acacDf,aes(f0SD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v3,v4, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#f0Range---------#

hapf0Rangelmer<-lmer(accuracy~f0Range*biormono+(1|items),happy_acacDf)
summary(hapf0Rangelmer)
anova(hapf0Rangelmer)
#non sig

sadf0Rangelmer<-lmer(accuracy~f0Range*biormono+(1|items),sad_acacDf)
summary(sadf0Rangelmer)
anova(sadf0Rangelmer)
#f0range sig, biormono sig.

#Visual
v5<-ggplot(happy_acacDf,aes(f0Range,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v6<- ggplot(sad_acacDf,aes(f0Range,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v5,v6, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensityMean-----------#
hapintMeanlmer<-lmer(accuracy~intensityMean*biormono+(1|items),happy_acacDf)
summary(hapintMeanlmer)
anova(hapintMeanlmer)
#intmean...marginal

sadintMeanlmer<-lmer(accuracy~intensityMean*biormono+(1|items),sad_acacDf)
summary(sadintMeanlmer)
anova(sadintMeanlmer)
#intMean super sig (int higher, acc lower), biormono sig. (mono>bi)=> JUST int MEAN!!!!!!

#Visual
v7<-ggplot(happy_acacDf,aes(intensityMean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v8<-ggplot(sad_acacDf,aes(intensityMean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v7,v8, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensitySD----------#

hapintSDlmer<-lmer(accuracy~intensitySD*biormono+(1|items),happy_acacDf)
summary(hapintSDlmer)
anova(hapintSDlmer)
#intsd...marginal

sadintSDlmer<-lmer(accuracy~intensitySD*biormono+(1|items),sad_acacDf)
summary(sadintSDlmer)
anova(sadintSDlmer)
#intsd super sig (int higher, acc lower), biormono sig. (mono>bi)

#Visual
v8<-ggplot(happy_acacDf,aes(intensitySD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v9<-ggplot(sad_acacDf,aes(intensitySD,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v8,v9, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#intensityRange----------#

#duration-----------#
hapdurlmer<-lmer(accuracy~duration*biormono+(1|items),happy_acacDf)
summary(hapdurlmer)
anova(hapdurlmer)
#dur sig*** (shorter, more accurate)

saddurSDlmer<-lmer(accuracy~duration*biormono+(1|items),sad_acacDf)
summary(saddurSDlmer)
anova(saddurSDlmer)
#bi mono sig* (mono>bi)

#Visual
v10<- ggplot(happy_acacDf,aes(duration,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v11<-ggplot(sad_acacDf,aes(duration,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v10,v11, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#HNRmean--------#
hapHNRmlmer<-lmer(accuracy~HNRmean*biormono+(1|items),happy_acacDf)
summary(hapHNRmlmer)
anova(hapHNRmlmer)
#nonsig

sadHNRmlmer<-lmer(accuracy~HNRmean*biormono+(1|items),sad_acacDf)
summary(sadHNRmlmer)
anova(sadHNRmlmer)
#bimono (mono>bi)=>NO SIG

#Visual
v12<- ggplot(happy_acacDf,aes(HNRmean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v13<-ggplot(sad_acacDf,aes(HNRmean,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v12,v13, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#HNRsd------------------#
hapHNRsdlmer<-lmer(accuracy~HNRsd*biormono+(1|items),happy_acacDf)
summary(hapHNRsdlmer)
anova(hapHNRsdlmer)
#nonsig

sadHNRsdlmer<-lmer(accuracy~HNRsd*biormono+(1|items),sad_acacDf)
summary(sadHNRsdlmer)
anova(sadHNRsdlmer)
#bimono (mono>bi)=>NO SIG


#SpeechRate----------------#

hapSRlmer<-lmer(accuracy~SpeechRate*biormono+(1|items),happy_acacDf)
summary(hapSRlmer)
anova(hapSRlmer)
#bi or mono (mono>bi)=>>non. but MAIN YES!!!!!!!!SIG!!!!

sadSRlmer<-lmer(accuracy~SpeechRate*biormono+(1|items),sad_acacDf)
summary(sadSRlmer)
anova(sadSRlmer)
#bi or mono (mono>bi)=> non

#Visual
v14<-ggplot(happy_acacDf,aes(SpeechRate,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v15<-ggplot(sad_acacDf,aes(SpeechRate,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v14,v15, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#JitterLocal

hapJitlmer<-lmer(accuracy~JitterLocal*biormono+(1|items),happy_acacDf)
summary(hapJitlmer)
anova(hapJitlmer)
#non sig

sadJitlmer<-lmer(accuracy~JitterLocal*biormono+(1|items),sad_acacDf)
summary(sadJitlmer)
anova(sadJitlmer)
#bi mono (mono>bi)

#Visual
v16<-ggplot(happy_acacDf,aes(JitterLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v17<- ggplot(sad_acacDf,aes(JitterLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v16,v17, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#ShimmerLocal----------#
hapShimlmer<-lmer(accuracy~ShimmerLocal*biormono+(1|items),happy_acacDf)
summary(hapShimlmer)
anova(hapShimlmer)
#non sig

sadShimlmer<-lmer(accuracy~ShimmerLocal*biormono+(1|items),sad_acacDf)
summary(sadShimlmer)
anova(sadShimlmer)
#bi mono (mono>bi)=> non sig

#Visual
v18<-ggplot(happy_acacDf,aes(ShimmerLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v19<-ggplot(sad_acacDf,aes(ShimmerLocal,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v18,v19, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#SpectralSlope---------------#
hapSSlmer<-lmer(accuracy~SpectralSlope*biormono+(1|items),happy_acacDf)
summary(hapSSlmer)
anova(hapSSlmer)
#non sig

sadSSlmer<-lmer(accuracy~SpectralSlope*biormono+(1|items),sad_acacDf)
summary(sadSSlmer)
anova(sadSSlmer)
#bi mono (mono>bi)=> no bimono. BUT MAIN approached SGIG!!!!!

#Visual
v20<-ggplot(happy_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v21<-ggplot(sad_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v20,v21, labels = c("happy", "sad"),ncol = 2, nrow = 1)


#AlphaRatio

hapARlmer<-lmer(accuracy~AlphaRatio*biormono+(1|items),happy_acacDf)
summary(hapARlmer)
anova(hapARlmer)
#non sig

sadARlmer<-lmer(accuracy~AlphaRatio*biormono+(1|items),sad_acacDf)
summary(sadARlmer)
anova(sadARlmer)
#bi mono (mono>bi)==>> NO SIG

#Visual
v22<-ggplot(happy_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v23<-ggplot(sad_acacDf,aes(SpectralSlope,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v22,v23, labels = c("happy", "sad"),ncol = 2, nrow = 1)

#H1H2

hapHHlmer<-lmer(accuracy~H1H2*biormono+(1|items),happy_acacDf)
summary(hapHHlmer)
anova(hapHHlmer)
#non sig

sadHHlmer<-lmer(accuracy~H1H2*biormono+(1|items),sad_acacDf)
summary(sadHHlmer)
anova(sadHHlmer)
#bi mono (mono>bi)

#Visual
v24<-ggplot(happy_acacDf,aes(H1H2,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
v25<-ggplot(sad_acacDf,aes(H1H2,accuracy,colour=biormono))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(-0.1,0.6)
ggarrange(v24,v25, labels = c("happy", "sad"),ncol = 2, nrow = 1)





####----part5.Is accuracy affected by any acoustic measures? JAPANESE ver.(sad and hap separate)----####

#--work log--#
#11/28/22 9:48  realized I have to go back to experimental log to see where the Japnese FULL data is. Load it and do the stats below to see what let them perform lower

#---Prep---#
#line below is the df for raw data from Japanese responders
# accBYresponder_ALLitems (commenting out for usual use)

#Japanese's accuracy for each item
accperitem<- aggregate(corr_ornot~items+corr_ans, data=dfforlmer, sum)
accperitem$accuracy<-accperitem$corr_ornot/52
colnames(accperitem)[3]<-"howmany_corr"
accperitem$acc_jap<-c(100,100,100,100,100,100,80,80,80,80,70,70,90,80,80,70,60,70,70,60,70,100,60,70,60,70,70,80,60,60,80,100,100,100,100,100,90,100,100,90,100,100,90,100,90,100,100,90,100,100,100,90,100,100,100,90,90)
acc_wJap<-accperitem
#separating sad and hap
hap_accwJap<-subset(acc_wJap,acc_wJap$corr_ans=="Happiness")
sad_accwJap<-subset(acc_wJap,acc_wJap$corr_ans=="Sadness")
#combine acoustics data
hap_jap_acdf<-merge(hap_accwJap,actbl, by.x = "items",by.y = "itemNum")
sad_jap_acdf<-merge(sad_accwJap,actbl,  by.x = "items",by.y = "itemNum")


#---stats and visualization---#

#f0mean-----------#
hap_jap_acdf$items<-as.factor(hap_jap_acdf$items)
japhapf0meananova<-summary(aov(acc_jap~f0Mean+Error(items/f0Mean),hap_jap_acdf))
#nonsig
japsadof0meananova<-summary(aov(acc_jap~f0Mean+Error(items/f0Mean),sad_jap_acdf))
#nonsig

#Visual
japvishap_f0mean<-ggplot(hap_jap_acdf,aes(f0Mean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_f0mean<-ggplot(sad_jap_acdf,aes(f0Mean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#f0SD------#
japhapf0SDanova<-summary(aov(acc_jap~f0SD+Error(items/f0SD),hap_jap_acdf))
#nonsig
japsadof0SDanova<-summary(aov(acc_jap~f0SD+Error(items/f0SD),sad_jap_acdf))
#nonsig

#Visual
japvishap_f0SD<-ggplot(hap_jap_acdf,aes(f0SD,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_f0SD<-ggplot(sad_jap_acdf,aes(f0SD,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#f0Range---------#
japhapf0Rangeanova<-summary(aov(acc_jap~f0Range+Error(items/f0Range),hap_jap_acdf))
#nonsig(0,07, marginal?)
japsadof0Rangeanova<-summary(aov(acc_jap~f0Range+Error(items/f0Range),sad_jap_acdf))
#nonsig

#Visual
japvishap_f0range<-ggplot(hap_jap_acdf,aes(f0Range,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_f0range<-ggplot(sad_jap_acdf,aes(f0Range,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#intensityMean-----------#
japhapintensityMeananova<-summary(aov(acc_jap~intensityMean+Error(items/intensityMean),hap_jap_acdf))
#nonsig
japsadointensityMeananova<-summary(aov(acc_jap~intensityMean+Error(items/intensityMean),sad_jap_acdf))
#nonsig

#Visual
japvishap_intensityMean<-ggplot(hap_jap_acdf,aes(intensityMean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_intensityMean<-ggplot(sad_jap_acdf,aes(intensityMean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#intensitySD----------#
japhapintensitySDanova<-summary(aov(acc_jap~intensitySD+Error(items/intensitySD),hap_jap_acdf))
#nonsig
japsadointensitySDanova<-summary(aov(acc_jap~intensitySD+Error(items/intensitySD),sad_jap_acdf))
#nonsig

#Visual
japvishap_intensitySD<-ggplot(hap_jap_acdf,aes(intensitySD,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_intensitySD<-ggplot(sad_jap_acdf,aes(intensitySD,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#intensityRange----------#

#duration-----------#
japhapdurationanova<-summary(aov(acc_jap~duration+Error(items/duration),hap_jap_acdf))
#nonsig
japsaddurationanova<-summary(aov(acc_jap~duration+Error(items/duration),sad_jap_acdf))
#nonsig

#Visual
japvishap_duration<-ggplot(hap_jap_acdf,aes(duration,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_duration<-ggplot(sad_jap_acdf,aes(duration,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)


#HNRmean--------#
japhapHNRmeananova<-summary(aov(acc_jap~HNRmean+Error(items/HNRmean),hap_jap_acdf))
#nonsig
japsadHNRmeananova<-summary(aov(acc_jap~HNRmean+Error(items/HNRmean),sad_jap_acdf))
#nonsig

#Visual
japvishap_duration<-ggplot(hap_jap_acdf,aes(HNRmean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_duration<-ggplot(sad_jap_acdf,aes(HNRmean,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)


#HNRsd------------------#
japhapHNRsdanova<-summary(aov(acc_jap~HNRsd+Error(items/HNRsd),hap_jap_acdf))
#nonsig
japsadHNRsdanova<-summary(aov(acc_jap~HNRsd+Error(items/HNRsd),sad_jap_acdf))
#nonsig

#Visual
japvishap_HNRsd<-ggplot(hap_jap_acdf,aes(HNRsd,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_HNRsd<-ggplot(sad_jap_acdf,aes(HNRsd,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)



#SpeechRate----------------#
japhapSpeechRateanova<-summary(aov(acc_jap~SpeechRate+Error(items/SpeechRate),hap_jap_acdf))
#nonsig
japsadSpeechRateanova<-summary(aov(acc_jap~SpeechRate+Error(items/SpeechRate),sad_jap_acdf))
#nonsig

#Visual
japvishap_SpeechRate<-ggplot(hap_jap_acdf,aes(SpeechRate,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_SpeechRate<-ggplot(sad_jap_acdf,aes(SpeechRate,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)



#JitterLocal--------------------#

japhapJitterLocalanova<-summary(aov(acc_jap~JitterLocal,hap_jap_acdf))
#Marginal! 0.6
japsadJitterLocalanova<-summary(aov(acc_jap~JitterLocal,sad_jap_acdf))
#SIGGGGGG  0.00476

#Visual
japvishap_JitterLocal<-ggplot(hap_jap_acdf,aes(JitterLocal,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(30,110)
japvissad_JitterLocal<-ggplot(sad_jap_acdf,aes(JitterLocal,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)+ylim(30,110)
ggarrange(japvishap_JitterLocal,japvissad_JitterLocal, labels = c("happy", "sad"),ncol = 2, nrow = 1)



#ShimmerLocal----------#
japhapShimmerLocalanova<-summary(aov(acc_jap~ShimmerLocal,hap_jap_acdf))
#nonsig
japsadShimmerLocalanova<-summary(aov(acc_jap~ShimmerLocal,sad_jap_acdf))
#marginal 0.07

#Visual
japvishap_ShimmerLocal<-ggplot(hap_jap_acdf,aes(ShimmerLocal,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_ShimmerLocal<-ggplot(sad_jap_acdf,aes(ShimmerLocal,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#SpectralSlope---------------#
japhapSpectralSlopeanova<-summary(aov(acc_jap~SpectralSlope+Error(items/SpectralSlope),hap_jap_acdf))
#nonsig
japsadSpectralSlopeanova<-summary(aov(acc_jap~SpectralSlope+Error(items/SpectralSlope),sad_jap_acdf))
#nonsig

#Visual
japvishap_SpectralSlope<-ggplot(hap_jap_acdf,aes(SpectralSlope,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_SpectralSlope<-ggplot(sad_jap_acdf,aes(SpectralSlope,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)


#AlphaRatio---------------#

japhapAlphaRatioanova<-summary(aov(acc_jap~AlphaRatio+Error(items/AlphaRatio),hap_jap_acdf))
#nonsig
japsadAlphaRatioanova<-summary(aov(acc_jap~AlphaRatio+Error(items/AlphaRatio),sad_jap_acdf))
#nonsig

#Visual
japvishap_AlphaRatio<-ggplot(hap_jap_acdf,aes(AlphaRatio,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_AlphaRatio<-ggplot(sad_jap_acdf,aes(AlphaRatio,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)


#H1H2----------------------#

japhapH1H2anova<-summary(aov(acc_jap~H1H2+Error(items/H1H2),hap_jap_acdf))
#nonsig
japsadH1H2anova<-summary(aov(acc_jap~H1H2+Error(items/H1H2),sad_jap_acdf))
#nonsig

#Visual
japvishap_H1H2<-ggplot(hap_jap_acdf,aes(H1H2,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)
japvissad_H1H2<-ggplot(sad_jap_acdf,aes(H1H2,acc_jap))+
  geom_point()+ geom_smooth(method = "lm", se = T)

#------------------------------------------updates on 11/26-----------------------------------------------------###      
#????????
#all the stats above is analyzing only the 90% 100% accuracy data by Jap ppl ...of cousre no sig diff for most of them
#if need to ???{?l?̂??????Ȃ??Amain study?Ɏg???Ȃ??��??????܂߂Ē??ׂȂ??Ƃł????ˁB



########################################################################
########################################################################

#    Emotional Prosody Project Stats (3rd Edition)       ----4/24/2024

########################################################################
########################################################################

# Goal: to combine Main study Data Analysis script and Acoustic Analysis script, with only necessary info.

######------------   OVERVIEW of this script   -------------######

# working directory and libraries
# Main Behavioral Data Analysis
    # Import and Clean data
      # Qultrix Behavioral data
      # Prolific Demographic data
      # Combine the 2 data above
      # Drop unnecesarry data
      # Make "analysisdf" --- 
    # Tweaking the DF more for stats
      # "dfforlmer"
      # add a column for correct of not
      # add a column for accuracy
      # add accuracy by Japanese pilot study
      # correlation - Jap accuracy X nonJap accuracy
    # visualization of Jap accuracy X nonJap accuracy
    # making df for Accuracy by subj
      # accpersubj
      # keeping only happy and sad
# Main Behavioral Stats
    # boxplot visualization
    # cleaning dfforlmer again for Glmer
    # contrast coding
    # Glmer modeling
      # m_max ... fits the best, with interaction
      # m_max2 ... fits the best, without interaction
      # comparing m_max and m_max2 with anova
    # More-than-chance Rate
# Acoustic Data Analysis
    # import data
    # clean data



######------------   workDir and libraries   -------------######

library("dplyr")
library("tidyr")
library("lme4")
library("ggpubr")
library("ggplot2")
library("memisc") #for contr.sum?
library("stats")

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
  #Anger - 5, Disgust - 5, Fear - 5, Happy - 16, neutral - 5, sadness - 16, Surprise - 5

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


###----making df for Accuracy by subj----
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

####---visualization---####
ggboxplot(newmainDF, x = "emo", y = "accuracy", 
          fill = "biormono",
          ylab = "Accuracy", xlab = "Valence")+
  scale_fill_manual(values = c("#f57178", "#80d3e8"),name="Language Background",breaks=c("bi","mono"),labels = c("Bilingual", "Monolingual")) +
  scale_x_discrete(labels=c("Positive","Negative"))
#I changed the color to try.., diff from what I initially had on google doc MAIN ARTICLE (if you want the original, just remove the color values)
#another blue ...6CB0D6

####---Cleaning the DF again for Glmer---####

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
contrasts(dfforGlmer$group)<-contr.sum(levels(dfforGlmer$group))
contrasts(dfforGlmer$valence)<-contr.sum(levels(dfforGlmer$valence))
#this used to be contr.Sum (with the uppercase S), but did not run. changed to small s. (4/25/2024)

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

#accuracy~1+group+valence+(1+valence|subjID)+(1+valence|item)

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






######--- Acoustic Data Analysis -----######

###----- Import Data ------###

acoustics_tbl_sent<-read.table("For acoustic analysis 2/outputTableSentence.txt", header = T)
acoustics_tbl_vowel<-read.table("For acoustic analysis 2/outputTableVowel.txt", header = T)
  #Make sure you take  out "Sound " from the second column
#forcolname<-as.character(acoustics_tbl[1,])
#colnames(acoustics_tbl)<-forcolname

###----Clean the table-----###

##----Sent Table----

#add emo column to Sent table
emo_col<-data.frame(matrix(ncol=1,nrow=1))
for (n in 1:nrow(acoustics_tbl_sent)){
  emotest<-grepl("sad", acoustics_tbl_sent[n,1])
  emotest_status<-ifelse(emotest == TRUE, "sad", "happy")
  emo_col[n,]<-emotest_status
}

acoustics_tbl_sent[13]<-emo_col
colnames(acoustics_tbl_sent)[13]<-"emotion"

#add item_num column to Sent table (so that can be used to match)
#Note: 4/24 -- checked. this is correct.
itemnum_col<-c("item55","item221","item242","item263","item223",
               "item144","item244","item265","item151","item251"
               ,"item172","item272","item193","item214","item174",
               "item81","item235","item195","item102","item62","item123","item83",
               "item104","item25","item125","item11","item111","item132","item53","item13","item74","item34")
acoustics_tbl_sent$itemNum<-itemnum_col
acSent<-acoustics_tbl_sent #rename the df name

##----Vowel Table----

#I need to add emo and itemNum
#adding emotion column
emo_col<-data.frame(matrix(ncol=1,nrow=1))
for (n in 1:nrow(acoustics_tbl_vowel)){
  emotest<-grepl("sad", acoustics_tbl_vowel[n,1])
  emotest_status<-ifelse(emotest == TRUE, "sad", "happy")
  emo_col[n,]<-emotest_status
}
acoustics_tbl_vowel[27]<-emo_col
colnames(acoustics_tbl_vowel)[27]<-"emotion"

#adding itemNum column
acoustics_tbl_vowel[28] <- gsub("^(.*?)_.*", "\\1", acoustics_tbl_vowel$filename)
colnames(acoustics_tbl_vowel)[28]<-"filenum"
ref_chart<-read.table("For acoustic analysis 2/itemnum_filenamenum_chart.txt", header = T)
acoustics_tbl_vowel$comb<-paste0(acoustics_tbl_vowel$emotion,acoustics_tbl_vowel$filenum)
acoustics_tbl_vowel<- merge(acoustics_tbl_vowel,ref_chart, by.x = "comb", by.y = "filenum", all.x = T)
acoustics_tbl_vowel$itemNum <- paste("item", acoustics_tbl_vowel$itemNum, sep = "")
acVowel <- acoustics_tbl_vowel[, !names(acoustics_tbl_vowel) == "comb"]

#----Averaging some data?----#

#SENTENCE TABLE#
#itemNum/filename = ID
#f0mean/max/range/SD
#intMean/SD/1to3kHz
#duration/speechRate

#VOWEL TABLE#
#vowelNum = ID
#vowel = category
#f0...can average by item
#f1/2/3/4 mean, center, width...ave by vowel
#hnrMean/SD...can average by item
#H1, H1H2, H1A1, H1A2, H1A3, jitterV, shimmerV...can average by item


#####--- Acoustics Stats ---#####
#Main thing I want to know: what are they listening to? how are they different from Japanese people?

# Q1. Is sadness sig different from happiness, for each acoustic measure? (kinda descriptive)

ggboxplot(actbl, x = "emotion", y = "f0Mean", 
          fill = "emotion", palette = c("#00AFBB", "#FF7F7F"),
          ylab = "F0 Mean", xlab = "Emotion")

# Q2. which acoustic features are different between the "Hardest"(lowest accuracy)item vs "Easiest" (highest accuracy)item?


# Q3. Was accuracy correlated with any of the acoustic measures? (looking at bi and mo separately)<- I think we can look at together, as main results were nonsig


# Q4. Is accuracy affected by any acoustic measures? (sad and hap separate, mono/bi as a factor)-random 


# Q5. Is accuracy affected by any acoustic measures? JAPANESE ver.(sad and hap separate) <- any effect of background?




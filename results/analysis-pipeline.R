#####################################################################
###                                                               ###
###             Palatine                                          ###
###               Judgement + Chunked SPR analysis pipeline       ###
###             Christopher Hammerly                              ###
###             UMass Amherst - July 2017                         ###
###                                                               ###
#####################################################################

#   Key to the experimental conditions:
#
#   Cond      Inter    Gramm   Num 
#
#   cond-A    I           G       Sg
#   cond-B    I           G       Pl
#   cond-C    I           U       Sg
#   cond-D    I           U       Pl
#   cond-E    NI          G       Sg
#   cond-F    NI          G       Pl
#   cond-G    NI          U       Sg
#   cond-H    NI          U       Pl
#
#
#   was/were = 
#   is/are =  
#   has/have = 
#

#   Packages necessary for analysis
library(plyr)
library(dplyr)
library(tidyr)
library(yarrr)
library(ggplot2)
library(ggmap)
library(ez)
library(lme4)
library(grid)
library(gridExtra)
library(car)
library(BayesFactor)

#   A function that gets around the issue of turning factors numeric

as.numeric.factor <- function(x){as.numeric(levels(x))[x]}

#   Create column names for the data file.

mycols <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "Question", "Response","null","RT","null")

#   Read in data.

data.raw <- read.csv('/Users/chrishammerly/palatine/results/results.csv',
                     header = 0, 
                     sep = ",",
                     quote = "",
                     comment.char = "#",
                     col.names=mycols,
                     fill = TRUE)

data.raw$Subject <- as.factor(data.raw$Subject)

print(data.raw %>% summarise(number = n_distinct(Subject)))



#   Remove subjects who were not paid on MTurk due to violating exclusion criteria (determined in
#   exclusion-pipeline.R)

excluded.subjects <- c("1501080670","1501084248","1501091688","1501101831")
data.raw <- droplevels(subset(data.raw, !(Subject %in% excluded.subjects)))

#   Create frame for demographic information

data.demo <- droplevels(subset(data.raw, Experiment == 'background' | Experiment == "exit"))
data.demo <- data.demo %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()

#   Add columns for factors and levels

exp.data <- data.raw

exp.data$Intervener <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-B' | exp.data$Experiment=='cond-C' | exp.data$Experiment=='cond-D', 'int', 'nonint')
exp.data$Grammaticality <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-B' | exp.data$Experiment=='cond-E' | exp.data$Experiment=='cond-F', 'grammatical', 'ungrammatical')
exp.data$Attractor <- ifelse(exp.data$Experiment=='cond-A' | exp.data$Experiment=='cond-C' | exp.data$Experiment=='cond-E' | exp.data$Experiment=='cond-G', 'SG', 'PL')

exp.data$Intervener <- as.factor(exp.data$Intervener)
exp.data$Grammaticality <- as.factor(exp.data$Grammaticality)
exp.data$Attractor <- as.factor(exp.data$Attractor)

#   Add column for verb type

#exp.data$Item <- as.numeric.factor(exp.data$Item)

# exp.data$Verb <- ifelse(exp.data$Item >=1 & exp.data$Item <= 14, "was/were", 
#                         ifelse(exp.data$Item >=15 & exp.data$Item <= 30, "lexical",
#                                ifelse(exp.data$Item >=31 & exp.data$Item <= 40, "is/are",
#                                       ifelse(exp.data$Item >=40 & exp.data$Item <= 48,"has/have","filler"))))
data.raw <- exp.data

#   Calculate z-scores for judgments over all items including fillers and plot them

data.all.judgments <- subset(data.raw , Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H','filler') & TrialType == 'Question')
data.all.judgments$Response <- as.numeric(as.character(data.all.judgments$Response))
data.all.judgments$RT <- as.numeric(as.character(data.all.judgments$RT))

data.all.judgments$z <- ave(data.all.judgments$Response, data.all.judgments$Subject, FUN = scale)

ggplot(data.all.judgments,aes(x=z))+
  geom_histogram(binwidth=.1)+
  ggtitle("JUDGMENT Z-SCORE DISTRIBUTION\n FOR ALL ITEMS")

#   Separate and remove fillers from acceptability data

data.acceptability <- subset(data.all.judgments, Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H'))


#   Separate SPR data and rename column headings

data.spr <- subset(exp.data, Experiment %in% c('cond-A', 'cond-B', 'cond-C','cond-D','cond-E','cond-F','cond-G','cond-H') & TrialType == 'DashedSentence')
names(data.spr) <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "region", "fragment","RT","null","sentence","Intervener","Grammaticality","Attractor")

data.spr$RT <- as.numeric(as.character(data.spr$RT))

fillers.spr <-subset(exp.data, Experiment %in% c('filler') & TrialType == 'DashedSentence')
names(fillers.spr) <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "region", "fragment","RT","null","sentence","Intervener","Grammaticality","Attractor")

fillers.spr$RT <- as.numeric(as.character(fillers.spr$RT))

#   Exclude trials where a chunk is read slower than 3 SD above the average and faster than 100ms

bad.trials <- data.spr %>%
  group_by(Experiment, region) %>%
  filter(RT < 100 | RT > 6000) 

bad.trials$ID <- paste(bad.trials$Subject,bad.trials$Item)


data.acceptability$ID <- paste(data.acceptability$Subject,data.acceptability$Item)
data.spr$ID <- paste(data.spr$Subject,data.spr$Item)

num.bad.trials <- n_distinct(bad.trials$ID)
num.trials.total <- n_distinct(data.spr$ID)
percent.trials.lost <- num.bad.trials/num.trials.total

data.acceptability <- droplevels((subset(data.acceptability, !(ID %in% bad.trials$ID))))
data.spr <- droplevels((subset(data.spr, !(ID %in% bad.trials$ID))))

print(data.acceptability %>% summarise(number = n_distinct(Subject)))
print(data.spr %>% summarise(number = n_distinct(Subject)))

### Get demographic infor

data.demo$age <- as.numeric(as.character(data.demo$age))

mean(data.demo$age)
length(grep("female", data.demo$gender))


#################################################
###                                           ###
###             Judgment Analysis             ###
###                                           ###
#################################################

#theme_set(theme_gray(base_size = 18))

pirateplot(Response~Attractor+Grammaticality+Intervener,
           data=data.acceptability,
           jitter.val = .15,
           inf.method = "se",
           inf.within = Subject,
           bar.f.o = .2,
           inf.f.o = .5,
           yaxt = "n",
           gl.lty = 0,
           cex.lab = .7)
par(cex=.7)
axis(side = 2, at = c(1,2,3,4,5,6,7),pos = -5.6, tick = F)
axis(side = 2, at = c(1,2,3,4,5,6,7),pos = .4, tick = F)


data.acceptability$Condition <- revalue(data.acceptability$Experiment, c("cond-A"="SSS","cond-B"="SPS","cond-C"="*SSP","cond-D"="*SPP","cond-E"="SSS","cond-F"="SPS","cond-G"="*SSP","cond-H"="*SPP"))
data.acceptability$Condition <- factor(data.acceptability$Condition, levels = c("SPS","SSS","*SPP","*SSP"))

judgment.hist <- ggplot(data.acceptability,aes(x=Response,fill = Attractor))+
  geom_histogram(binwidth=1)+
  scale_fill_manual(values=c("#499fd1","#e05555"))+
  facet_grid(Modifier~Condition)+ 
  ylab("Count")+
  guides(fill=FALSE)


#   Create a data frame with the mean response for each participant on each condition

subj.by.cond <- data.acceptability %>% 
  group_by(Subject, Intervener, Grammaticality, Attractor) %>% 
  summarise(mean.resp = mean(Response))

#   Create a data frame with the mean of the subject means and the SEM for each condition

cond.summ <- subj.by.cond %>%
  group_by(Intervener, Grammaticality, Attractor) %>%
  summarise(mean_cond = mean(mean.resp),
            SEM = sd(mean.resp)/sqrt(n_distinct(Subject)))

dodge <- position_dodge(width=0.9)

mandible.means.sem <- ggplot(cond.summ,aes(x=Grammaticality,y=mean_cond,fill=Attractor))+
  geom_bar(position = 'dodge',stat = "identity")+
  geom_linerange(stat='identity',position = position_dodge(width = 0.9),mapping=aes(ymax = mean_cond+SEM,ymin=mean_cond-SEM)) +
  scale_fill_grey()+
  #scale_fill_manual(values=c("#499fd1","#e05555"))+
  ylab("Rating")+
  facet_grid(.~Intervener)

#################################################
###                                           ###
###                 SPR Analysis              ###
###                                           ###
#################################################

#   Raw SPR RT distribution for all items and regions

raw.spr.dist <- ggplot(data.spr,aes(x=RT))+
  geom_histogram(binwidth=50)+
  xlim(0,7000)+
  ggtitle("RAW SPR RT DISTRIBUTION")

#   Add log transformed RT as a column in data.spr. This will be used for most analyses

data.spr$logRT <- log(data.spr$RT)


#   Log RT distribution across experimental conditions

log.spr.dist <- ggplot(data.spr,aes(x=logRT))+
  geom_histogram(binwidth=.1)+
  xlim(4,9)+
  ggtitle("SPR LOG(RT) DISTRIBUTION")

grid.arrange(raw.spr.dist,log.spr.dist,ncol = 2)

#   Split the data from region 3 and 4

region4.spr <- droplevels(subset(data.spr, region == 4))
region5.spr <- droplevels(subset(data.spr, region == 5))


pirateplot(logRT~Grammaticality+Attractor+Intervener,
           data=region4.spr,
           jitter.val = .05,
           inf.method = "se",
           inf.within = Subject,
           cex.lab = .7,
           cex.axis = .7)
pirateplot(logRT~Grammaticality+Attractor+Intervener,
           data=region5.spr,
           jitter.val = .05,
           inf.method = "se",
           inf.within = Subject,
           cex.lab = .7,
           cex.axis = .7)

#   Condition summaries for raw RT: Gives mean and SEM by region. This is plotted later

RT.subj.by.cond <- data.spr %>%
  group_by(Subject, Experiment, Intervener, Grammaticality, Attractor, region) %>%
  summarise(average = mean(RT))

RT.cond.summ <- RT.subj.by.cond %>%
  group_by(Experiment, Intervener, Grammaticality, Attractor, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))


#   Summaries by condition and region for log transformed RT

log.RT.subj.by.cond <- data.spr %>%
  group_by(Subject, Experiment, Intervener, Grammaticality, Attractor, region) %>%
  summarise(average = mean(logRT))

log.RT.cond.summ <- log.RT.subj.by.cond %>%
  group_by(Experiment, Intervener, Grammaticality, Attractor, region) %>%
  summarise(mean = mean(average),
            SEM = sd(average)/sqrt(n_distinct(Subject)))


#### Plotting raw RT data


#### Plotting log transformed RT data

log.RT.cond.summ$Condition <- revalue(log.RT.cond.summ$Experiment, c("cond-A"="SSS","cond-B"="SPS","cond-C"="SSP","cond-D"="SPP","cond-E"="SSS","cond-F"="SPS","cond-G"="SSP","cond-H"="SPP"))

log.RT.cond.summ$region2 <- revalue(log.RT.cond.summ$region, c("1"="The truck","2"="(that parked) beside the motorcycle","3"="apparently was refurbushed","4"="by the collector","5"="over the summer"))

log.spr.plot <- ggplot(data=log.RT.cond.summ, 
                       aes(x=region, y=mean, 
                           group=Condition,
                           shape=Attractor,
                           linetype = Grammaticality
                           #color = Attractor
                       )) + 
  geom_point(stat = "identity",size=3.5)+
  geom_errorbar(aes(ymax = mean+SEM,ymin=mean-SEM,width=0.05))+
  scale_color_grey()+
  ylab("Log reading time")+
  xlab("Region")+
  #scale_color_manual(values = c("#e05555","#499fd1","#a03b3b","#36769b"))+
  stat_identity(geom="line")+
  theme(legend.position="bottom",legend.direction="vertical")+
  facet_grid(.~Intervener,
             labeller = labeller(Intervener = c("int" = "Intervening",
                                              "nonint" = "Non-intervening")))


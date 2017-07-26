##########################################################################
######              Mechanical Turk Exclusion Pipeline:             ######
######                Ibex Farm edition                             ######
######              Christopher Hammerly & Brian Dillon             ######
######              Cognitive Science of Language Lab               ######
######              UMass Amherst                                   ######
######              Created: August 2016                            ######
######              Last Modified: August 2016                      ######
##########################################################################

#
#   TO DO:
#     - Add variable for catch fillers in preamble
#     - Make insturction questions easy to modify
#

#####################################################################
###                         Introduction                          ###
#####################################################################

#   The purpose of this script is to make it as easy as possible to see if 
#   an Amazon Mechanical Turk participant has run up against any exclusion criteria
#   on an Ibex Farm experiment.
#
#   This is not a pipleline for excluding particular trials for your analysis. 
#   We assume this can be done in your analysis pipeline. The purpose is 
#   to decide whether or not to pay a given Turker for completing your HIT.
#
#   It is organized by the following criteria:
#
#       - Demographics
#       - Instruction Questions
#       - Catch fillers
#       - Judgment scale usage
#       - Repeat participants
#
#   The goal is for each of these to be malleable enough to easily fit the specifications
#   of your criteria. 
#
#   The list is not exhaustive - add as you see fit and document how to use it!

#####################################################################
###                           Libraries                           ###
#####################################################################

#   This section includes the packages necessary to execute the pipeline.
#
#   If you do not have a given package, you can install it with the following command:
#   install.packages("PACKAGE.NAME"). Remember to use quotations around the name.

library(tidyr)
library(dplyr)
library(ez)

#####################################################################
###                           Data Prep                           ###
#####################################################################

#   Create column names for the data file. These labels are geared towards acceptablity judgments.
#   Modifying these column names will have effects downstream, so do so at your own risk.

mycols <- c("Subject","MD5","TrialType","Number","Element","Experiment","Item", "Question", "Response","null","RT","null")

#   Read in data. Modify the filepath to indicate where your results are kept.

data.raw <- read.csv('/Users/chrishammerly/palatine/results/results.csv',
                     header = 0, 
                     sep = ",",
                     quote = "",
                     comment.char = "#",
                     col.names=mycols,
                     fill = TRUE)
data.raw$Subject <- as.factor(data.raw$Subject)

#   Segregate the judgment data. These are all labled "Question" in Ibex

data.judge <- droplevels(subset(data.raw, TrialType == 'Question' & Experiment != "Practice"))
data.judge$Response <- as.numeric(as.character(data.judge$Response))

#   Segregate the demographic data and Worker ID. Demographic questions 
#   should be in a form with a uniquely identifying label to allow them 
#   to be pulled out -- in this case, the label "background".

data.demo <- droplevels(subset(data.raw, Experiment == 'background' | Experiment == "exit"))
data.demo <- data.demo %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()

#   Segregate the instruction questions. Again, the form for these questions
#   should be unique to allow easy identification -- in this case, they are
#   labeled "intro".

data.instructions <- droplevels(subset(data.raw, Experiment == 'intro'))
data.instructions <- data.instructions %>%
  select(Subject,Question,Response) %>%
  spread(Question,Response) %>%
  droplevels()

#####################################################################
###                           Demographics                        ###
#####################################################################

#   A regular expression to identify those who reported English as their native language 
data.demo$English <- grepl('[E|e]nglish|ENGLISH',data.demo$natlang)

#   Convert booleans to 1 (true) and 0 (false)
data.demo$English <- as.numeric(data.demo$English)

#   Add subjects that did not self-report as English speakers to bad subjects list
for (i in 1:length(levels(data.demo$Subject))) {
  cur.subj <- levels(data.demo$Subject)[i]
  if (data.demo$English[i] == 0) {
    cat(paste0(cur.subj," was excluded due to non-English native language\n"), file = "bad.subjects.txt", sep = " ", append = TRUE)
  }
}


#####################################################################
###                         Instuctions                           ###
#####################################################################

#   Score questions. This can be modified so that the answers to your questions and the names
#   you chose replace those shown below.
correct.answers = c("space","yes","silent","all","no","1","maximized")
names(correct.answers) = c("advancekey","hands","read","scale","screen","unacceptableresponse","window")
all.answers = as.matrix(data.instructions [,c("advancekey","hands","read","scale","screen","unacceptableresponse","window")])
data.instructions$accurate.answers = apply(all.answers, 1, identical, correct.answers)

#   Convert boolean values to numeric
data.instructions$accurate.answers <- as.numeric(data.instructions$accurate.answers)

#   Add subjects who did not meet criteria to bad subjects list
for (i in 1:length(levels(data.instructions$Subject))) {
  cur.subj <- levels(data.instructions$Subject)[i]
  if (data.instructions$accurate.answers[i] == 0) {
    cat(paste0(cur.subj," was excluded based on instruction questions\n"), file = "bad.subjects.txt", sep = " ", append = TRUE)
  }
}

######################################################################
###                       Catch fillers                            ###
######################################################################

#   Grammatical: Items 57-60
#   Ungrammatical: Items 93-96

#   Segregate catch fillers. This can be modified so the item numbers of your catch
#   fillers replace those shown below.

grammatical.catch <- droplevels(subset(data.judge, Item == 57 | Item == 58 | Item == 59 | Item == 60 ))

ungrammatical.catch <- droplevels(subset(data.judge, Item == 93 | Item == 94 | Item == 95 | Item == 96 ))

#   Calculate means on catch fillers for each subject and put values as columns in "catch"

catch <- grammatical.catch %>%
  group_by(Subject) %>%
  summarise(mean.grammatical = mean(Response))

catch <- ungrammatical.catch %>%
  group_by(Subject) %>%
  summarise(mean.ungrammatical = mean(Response)) %>%
  right_join(catch, by = "Subject")

#   Add subjects whose mean rating on the grammatical catch fillers is less than their
#   mean rating on the ungrammatical catch fillers to bad.subjects list

for (i in 1:length(levels(catch$Subject))) {
  cur.subj <- levels(catch$Subject)[i]
  if (catch$mean.ungrammatical[i] > catch$mean.grammatical[i]) {
    cat(paste0(cur.subj," was excluded based on catch fillers\n"), file = "bad.subjects.txt", sep = " ", append = TRUE)
  }
}


#####################################################################
###                         Scale usage                           ###
#####################################################################

#   Add subjects who used only one or two values of the scale to bad subjects list

for (i in 1:length(levels(data.judge$Subject))) {
  cur.subj <- levels(data.judge$Subject)[i]
  cur.data.judge <- subset(data.judge, Subject == cur.subj)
  cur.table <- table(cur.data.judge$Response)
  if (length(cur.table) == 1 | length(cur.table) == 2) {
    cat(paste0(cur.subj," was excluded based on scale usage\n"), file = "bad.subjects.txt", sep = " ", append = TRUE)
  }
}

#####################################################################
###                             Repeats                           ###
#####################################################################

#   Add repeat workers to the bad subjects list
#
#   NOTE: This implicates both the original and the repeat. Workers should still be paid for their
#   first time through, but not for the repeat.

duplicates <- droplevels(data.demo$worker_id[duplicated(data.demo$worker_id)])

for (i in 1:length(levels(data.demo$Subject))) {
  cur.subj <- levels(data.demo$Subject)[i]
  if (data.demo$worker_id[i] %in% duplicates) {
    cat(paste0(cur.subj," was excluded based on repeat worker ID\n"), file = "bad.subjects.txt", sep = " ", append = TRUE) 
  }
}


data.judge.trimmed <- subset(data.judge, Subject != "1473195073" & Subject != "1473339213" & Subject != "1473340057" & Subject != "1473356919" & Subject != "1473208710" & Subject != "1473265118" & Subject != "1473375948" & Subject != "1473432095" & Subject != "1473432829")

xtabs(~ Item + Experiment, data = data.judge.trimmed)

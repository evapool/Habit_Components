####################################################################################################
#                                                                                                  #
#                                                                                                  #                      
#         Habits and stress in problematic reward seeking behaviors: 
#              a French validation of the Creature of Habit Scale                                  # 
#                                                                                                  #
#                                                                                                  #
#                    Tamara Corino                                                                 #
#                    Maelys Denis-Bonnin                                                           #    
#                    Eva R Pool                                                                    # 
#                                                                                                  #
# Created by E.R.P. on  September 2020                                                             #
####################################################################################################

# load libraries necessary for the analyses
library(car)
library(afex)
library(doBy)
library(ggplot2)
library(ggExtra)
library(sjstats)
library(jtools)
library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(devtools)
library(GPArotation)
library(nFactors)
library(RGenData)
library(lavaan)
library(semPlot)

#---------------------------------------------------------------------------
#                    PRELIMINARY STUFF 
#---------------------------------------------------------------------------

# Set path
full_path       <- dirname(rstudioapi::getActiveDocumentContext()$path) # this gets the path were we are in right noe
pos             <- regexpr("French_Habit", full_path) # we want the path to the root folder
home_path       <- substr(full_path, 1, pos+11)
figures_path    <- file.path(home_path, 'analysis/figures') # here we will save the pdf of our figures
#utilities_path  <- file.path(home_path, 'analysis/R') # here we will put any homemade function we might beed
setwd (home_path)

#------------------------------ load the databases -------------------------

# get databases from B1 classes
B1 <- read.csv(file.path(home_path,'analysis/data/B1_compiled.csv'), check.names = F) # read in dataset

# get databases from B2 classes
B2 <- read.csv(file.path(home_path,'analysis/data/B2_compiled.csv'), check.names = F) # read in dataset

# get databases from B2 classes second round
B3 <- read.csv(file.path(home_path,'analysis/data/B3_compiled.csv'), check.names = F) # read in dataset


# MERGE TWO DATASETS
# create an dataset id
B2$dataset = 'B2'
B1$dataset = 'B1'
B3$dataset = 'B3'

# create unique id for each dataset
B2$sub = B2$sub+ 200
B3$sub = B3$sub+ 300 

# join databases
TMP  <- join (B1, B2, type = 'full')
QUEST <- join (TMP, B3, type = 'full')

#------------------------- REMOVE PARTICIPANTS ---------------------------------------------

# remove for missing data
QUEST <- subset(QUEST, sub != 'NA') # remove missing lines 
QUEST <- subset(QUEST, sub != '83') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions 
QUEST <- subset(QUEST, sub != '497') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions 
QUEST <- subset(QUEST, sub != '499') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions

QUEST <- subset(QUEST, sub != '501') # emtpy
QUEST <- subset(QUEST, sub != '502') # emtpy
QUEST <- subset(QUEST, sub != '503') # emtpy

# ----------------------------------------------------------------------------------------------------
#                                 DESCRIPTIVE ANALYSIS
# ----------------------------------------------------------------------------------------------------


# ------------------------- mean n and sd -------------------------------------------------------------
var.subscales <- c("CESD_total","COHS_automaticity", "COHS_routine","EAT26_total",
                   "IAT_total","PCLS_total","PMPUQSV_total", "PSS_total", "QABB_total",
                   "OCIR_total","STAIS_total","STAIT_total")

db.subscale <- QUEST[var.subscales]
describe(db.subscale)


B23 = subset(QUEST, dataset != 'B1')
B1 = subset(QUEST, dataset == 'B1')


var.B1.subscale <- c("UPPS_negative_urgency","UPPS_positive_urgency",
                     "UPPS_premeditation", "UPPS_sensation", "UPPS_perseverance")
db.subscale.B1 <- B1[var.B1.subscale]
describe(db.subscale.B1)

var.B23.subscale <- c("UPPS_urgency","UPPS_premeditation", 
                      "UPPS_sensation", "UPPS_perseverance")

db.subscale.B23 <- B23[var.B23.subscale]
describe(db.subscale.B23)


#----------------------------------- alphas-----------------------------------------------

# COHS
list_COHS_routine = c(15, 27, 13, 6, 24, 2, 12, 17, 7, 10, 20, 1, 22, 18, 4, 14)
col_COHS_routine = paste("COHS[",list_COHS_routine ,"]", sep="")

list_COHS_automaticity = c(11, 25, 3, 23, 19, 26, 8, 9, 16, 5, 21)
col_COHS_automaticity = paste("COHS[",list_COHS_automaticity ,"]", sep="")

# CES-d
col_CESD = paste("CESD[",1:20,"]", sep="") 

# EAT
col_EAT26 = paste("EAT26[",1:26,"]", sep="")

# IAT
col_IAT = paste("IAT[",1:20,"]", sep="")

# PCLS
col_PCLS_total = paste("PCLS[",1:17,"]", sep="")

# PMPUBQSV
col_PMPUBQSV = paste("PMPUQSV[",1:15,"]", sep="")

# PSS
col_PSS = paste("PSS[",1:10,"]", sep="")

# QABB
col_QABB = paste("QABB[",1:19,"]", sep="")

# OCI-R
col_OCIR = paste("OCIR[",1:18,"]", sep="")

#STAIS
col_STAIS = paste("STAIS[",1:20,"]", sep="") 

# STAIT
col_STAIT = paste("STAIT[",1:20,"]", sep="") 

# create a list with the 2 subscales to see the alpha and correlations
ALL <- list(CEDS = c(col_CESD),
            COHS_routine = c(col_COHS_routine), 
            COHS_automaticity = c(col_COHS_automaticity),
            EAT26 = c(col_EAT26),
            IAT = c(col_IAT),
            PCLS = c(col_PCLS_total),
            PMPUBQSV = c(col_PMPUBQSV),
            PSS = c(col_PSS),
            QABB = c (col_QABB),
            OCIR = c(col_OCIR),
            STAIS = c(col_STAIS),
            STAIT = c(col_STAIT)
)

my.scales <- scoreItems(ALL,QUEST) 
my.scales$alpha

B23 = subset(QUEST, dataset != 'B1')
B1 = subset(QUEST, dataset == 'B1')

# short version
# negative negative_urgency
list_negative_urgency = c(4, 7, 12, 17)
col_urgneg = paste("UPPS[",list_negative_urgency ,"]", sep="")

# positive urgency 
list_positive_urgency = c(2, 10, 15, 20)
col_urgpos = paste("UPPS[",list_positive_urgency,"]", sep="")

# lack premeditation
list_premeditation = c(1, 6, 13, 19)
col_lackprem = paste("UPPS[",list_premeditation ,"]", sep="")

# lack perseverance
list_perseverance = c(5, 8, 11, 16)
col_lackpers = paste("UPPS[",list_perseverance ,"]", sep="")

# sensation seeking
list_sensation  = c(3, 9, 14, 18)
col_sensation = paste("UPPS[",list_sensation ,"]", sep="")

# alpha long
SHORT <- list(URGPOS = c(col_urgpos),
              URGNEG = c(col_urgneg),
              SEN = c(col_lackprem), 
              PERS = c(col_lackpers),
              PREM = c(col_sensation)
)

my.scales <- scoreItems(SHORT,B1) 
my.scales$alpha

# long version

# urgency
list_urgency = c(2, 6, 10, 14, 18, 24, 28, 32, 36, 41, 43, 45)
col_urgency = paste("UPPS[",list_urgency ,"]", sep="")

# lack of premeditation
list_premeditation = c(1, 5, 9, 13, 17, 23, 27, 31, 35, 39, 40)
col_lackprem = paste("UPPS[",list_premeditation ,"]", sep="")

# lack of perseverance
list_perseverance = c(4, 8, 12, 16, 20, 22, 26, 30, 34, 38)
col_lackpers = paste("UPPS[",list_perseverance ,"]", sep="")

# sensation seeking
list_sensation  = c(3, 7, 11, 15, 19, 21, 25, 29, 33, 37, 42, 44)
col_sensation = paste("UPPS[",list_sensation ,"]", sep="")

# alpha long
LONG <- list(URG = c(col_urgency),
             SEN = c(col_lackprem), 
             PERS = c(col_lackpers),
             PREM = c(col_sensation)
)

my.scales <- scoreItems(LONG,B23) 

my.scales$alpha


# --------------------- MERGE LONG AND SHORT VERSION OF THE UPPS

# let's keep only the negative urgency of the short version for consistency with the long version
B1$UPPS_urgency <- B1$UPPS_negative_urgency

# scale and then merge
# set version on the same scale
B1$UPPS_urgency <- scale(B1$UPPS_urgency)
B2$UPPS_urgency <- scale(B2$UPPS_urgency)
B3$UPPS_urgency <- scale(B3$UPPS_urgency)

B1$UPPS_premeditation <- scale(B1$UPPS_premeditation)
B2$UPPS_premeditation <- scale(B2$UPPS_premeditation)
B3$UPPS_premeditation <- scale(B3$UPPS_premeditation)

B1$UPPS_perseverance <- scale(B1$UPPS_perseverance )
B2$UPPS_perseverance  <- scale(B2$UPPS_perseverance )
B3$UPPS_perseverance  <- scale(B3$UPPS_perseverance )

B1$UPPS_sensation     <- scale(B1$UPPS_sensation )
B2$UPPS_sensation     <- scale(B2$UPPS_sensation )
B3$UPPS_sensation     <- scale(B3$UPPS_sensation )


# merge
TMP   <- join (B1, B2, type = 'full')
QUEST <- join (TMP, B3, type = 'full')

# remmove again
QUEST <- subset(QUEST, sub != 'NA') # remove missing lines 
QUEST <- subset(QUEST, sub != '83') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions 
QUEST <- subset(QUEST, sub != '497') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions 
QUEST <- subset(QUEST, sub != '499') # participant only used the extreme values on each scale and the time to complete the questionnaire is too short to be reading the questions


#-------------------------------- Figure 1-----------------------------------------------
var.subscales <- c("CESD_total","COHS_automaticity", "COHS_routine","EAT26_total",
                   "IAT_total","PMPUQSV_total", "PSS_total","PCLS_total",
                   "OCIR_total","STAIS_total","STAIT_total", "UPPS_urgency",
                   "UPPS_premeditation","UPPS_perseverance","UPPS_sensation","sub")

db.plot <- QUEST[var.subscales]


# now we need to have that in a long format because it's easy for the plots
db_long <- gather(db.plot, questionnaire , score, CESD_total:UPPS_sensation, factor_key=TRUE)

# labels
labels <- c(CESD_total = "CES-D", COHS_automaticity = "COHS automaticity", COHS_routine = "COHS routine",
            EAT26_total = "EAT-26", IAT_total = "IAT", PMPUQSV_total = "PMPUQ-SV", PSS_total = "PSS",
            PSS_total = "PSS", PCLS_total = "PCL-S", OCIR_total = "OCI-R", STAIS_total = "STAI-S",
            STAIT_total = "STAI-T", UPPS_urgency = "UPPS urg.", UPPS_sensation = "UPPS sens. seek.", 
            UPPS_perseverance = "UPPS lack of pers.", UPPS_premeditation = "UPPS lack of perm.")


# let's plot distribution
pp = ggplot(data = db_long, aes (x = score, fill = questionnaire)) +
  facet_wrap(~questionnaire, scales = "free", labeller = labeller(questionnaire = labels) ) +
  geom_histogram(aes(y=..density..),alpha=0.6) +
  geom_density(aes(color = questionnaire), alpha = 0.3) +
  theme_bw()+
  labs(
    title = '',
    x = 'Questionnaires',
    y = "Scores"
  ) 

# let's make the plot nice looking
ppp <-   pp + theme_bw(base_size = 10, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 10, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.position = "none") 

# let's print the plot in a pdf document
pdf(file.path(figures_path,'Questionnaires_distribution.pdf'))
print(ppp)
dev.off()



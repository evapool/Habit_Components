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
library(data.table)
library(reshape2)


# for network analysis
library(corrplot)
library(RColorBrewer)
library(bootnet) 
library(networktools)
library(NetworkComparisonTest)
library(qgraph)

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



# ----------------------------------------------------------------------------------------------------
#                                 DESCRIPTIVE ANALYSIS
# ----------------------------------------------------------------------------------------------------

# ---------------------------------- mean n and sd -------------------------------------------------------------
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



#----------------------------------- Distributions: Figure 1-----------------------------------------------
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



# ---------------------------------- Demografic  ------------------------------------------------------

# count participants
count(QUEST, c("sub")) # how many participants

# Get only the demografic info from the questionnaire
dem.var <- c("Age", "Genre","Langue[1]","Langue[2]","Langue[3]", "lateralite[1]","lateralite[2]","lateralite[3]", "dataset") 
db.demografics <- QUEST[dem.var]

# rename colons so that we know what they mean
colnames(db.demografics)[colnames(db.demografics) == "Langue[1]"] <- "French_dominant"
colnames(db.demografics)[colnames(db.demografics) == "Langue[2]"] <- "French_good"
colnames(db.demografics)[colnames(db.demografics) == "Langue[3]"] <- "French_medium"

colnames(db.demografics)[colnames(db.demografics) == "lateralite[1]"] <- "right_hand"
colnames(db.demografics)[colnames(db.demografics) == "lateralite[2]"] <- "left_hand"
colnames(db.demografics)[colnames(db.demografics) == "lateralite[3]"] <- "ambidextrous"

# define factors
db.demografics$Genre = factor(db.demografics$Genre)
db.demografics$French_dominant = factor(db.demografics$French_dominant)
db.demografics$French_good = factor(db.demografics$French_good)
db.demografics$French_medium = factor(db.demografics$French_medium)

db.demografics$right_hand = factor(db.demografics$right_hand)
db.demografics$left_hand = factor(db.demografics$left_hand)
db.demografics$ambidextrous = factor(db.demografics$ambidextrous)

db.demografics$dataset = factor(db.demografics$dataset)

# describe
summary(db.demografics)
describe(db.demografics)



# ----------------------------------------------------------------------------------------------------
#                                 COHS FRENCH VERSION VALIDATION
# ----------------------------------------------------------------------------------------------------

# prepare dataset of interest for EFA
col = paste("COHS[",1:27,"]", sep="")
db.COHS <- QUEST[col]
db.COHS <- na.omit(db.COHS) # remove NAN lines

#----------------------------------- Postulates -------------------------------------------------------

# add formal tests for the variance covariance assumption
KMO(db.COHS)
#0.00 to 0.49 unacceptable.
#0.50 to 0.59 miserable.
#0.60 to 0.69 mediocre.
#0.70 to 0.79 middling.
#0.80 to 0.89 meritorious.
#0.90 to 1.00 marvelous.

cor_matrix <- cor(db.COHS)
cortest.bartlett(cor_matrix, n = nrow(db.COHS)) 

# check distributions before proceeding with FA
describe (db.COHS)
pairs.panels(na.omit(db.COHS))

# correlation between factors
corrf = corr.test(QUEST$COHS_automaticity, QUEST$COHS_routine)


# ---------------------------------- EFA Determine N factors --------------------------------------------------
# method 1 parallel analysis
nFact  <- fa.parallel(db.COHS, fm = "ml") # suggests 4 with FA and 2 with PA (eigen value suggest 2 (Kaiser criteria of eigen value larger than 1), catell's suggest 2)
# method 2 minimum average partial procedure
nFact  <- vss(db.COHS) # minimum average partial procedure (MAP) suggests 2
# method 3 optimnal coordinates and acceleration factor
nFact  <- nScree(x = db.COHS, model = "factors") # accelleration factor suggest 2 and optimal coordinantes suggest 2
summary(nFact)
plotnScree(nFact) # here we have a very clear scree plot
# method 4 "comparasion data"
nFact <- EFACompData(db.COHS, 6, n.pop = 10000, n.samples = 500, alpha = .30, graph = T,
                     corr.type = "pearson") # again 2 factors

#---------------------------------- apply EFA with oblimin --------------------------------------------------
quest.1.efa <- fa(r = db.COHS, nfactors = 2, rotate = "oblimin", fm = "ml")

print(quest.1.efa$loadings,cutoff = 0.0) # print loadings for table 2

#---------------------------------------- CFA -----------------------------------------------------

# rename colons so than laavan can read them
db.COHS.cfa <- db.COHS
col_old = paste("COHS[",1:27,"]", sep="")
col_new = paste("item",1:27,"", sep="")
colnames(db.COHS.cfa)[colnames(db.COHS.cfa) == col_old] <- col_new

# define model
cohs.model <- "Automaticity =~ item3 + item5 + item8 + item9 + item11 + item16 + item19 + item21 + item23 + item25 + item26
Routine =~  item1 + item2 + item4 + item6 + item7 + item10 + item12 + item13 + item14 + item15 + item17 + item18 + item20 + item22 + item24 + item27"

# fit the model
fit1 <- lavaan::cfa(cohs.model, data=db.COHS.cfa,std.lv=TRUE)
summary(fit1, fit.measures=T,standardized=T)

# plot
lbls = c("i03", "i05", "i08","i09","i11","i16","i19","i21","i23", "i25", "i26",
         "i01",  "i02", "i04", "i06",  "i07", "i10",  "i12", "i13",  "i14",  "i15",  "i17",  "i18",  "i20", "i22",  "i24", "i27",
         "A", "R")
semPaths(fit1,residuals=F,sizeMan=5,"std",
         #posCol=c("skyblue4", "red"),
         nodeLabels=lbls,
         edge.color="skyblue4",
         edge.label.cex=0.75,layout="circle")

dev.print(pdf, file.path(figures_path,'Figure_CFA.pdf'))
dev.off()


# ----------------------------------------------------------------------------------------------------
#                                 NETWORK ANALYSIS
# ----------------------------------------------------------------------------------------------------


# ------------------------------- data reduction  EFA ------------------------------------------------------
# select subscales
var.subscales <- c("OCIR_Washing","OCIR_checking","OCIR_ordering","OCIR_obsessing","OCIR_hoarding","OCIR_neutralising",
                   "EAT26_oral_control","EAT26_dieting","EAT26_bulimia","IAT_salience","IAT_excessive_use","IAT_neglect_work",
                   "IAT_anticipation","IAT_lack_control","IAT_neglect_social_life","PMPUQSV_dangerous","PMPUQSV_prohibited",
                   "PMPUQSV_dependant", "STAIT_total","STAIS_total", "PSS_total","CESD_total","UPPS_urgency",
                   "UPPS_premeditation", "UPPS_sensation", "UPPS_perseverance")

db.subscale <- QUEST[var.subscales]
describe(db.subscale)

# verify postulates
KMO(db.subscale)

cor_matrix <- cor(db.subscale, use = "complete.obs")
cortest.bartlett(cor_matrix, n = nrow(db.COHS))

# remove the two subscales that do not have a satisfactory KMO
var.subscales <- c("OCIR_Washing","OCIR_checking","OCIR_ordering","OCIR_obsessing","OCIR_hoarding","OCIR_neutralising",
                   "EAT26_oral_control","EAT26_dieting","EAT26_bulimia","IAT_salience","IAT_excessive_use","IAT_neglect_work",
                   "IAT_anticipation","IAT_lack_control","IAT_neglect_social_life","PMPUQSV_prohibited",
                   "PMPUQSV_dependant", "STAIT_total","STAIS_total", "PSS_total","CESD_total","UPPS_urgency",
                   "UPPS_premeditation", "UPPS_perseverance")
db.subscale <- QUEST[var.subscales]
describe(db.subscale)

# extract factors

# method 1 parallel analysis
nFact  <- fa.parallel(db.subscale, fm = "ml") # 5

# method 2 minimum average partial procedure
nFact  <- vss(db.subscale) # 5 (MAP)

# method 3 optimnal coordinates and acceleration factor
nFact  <- nScree(x = na.omit(db.subscale), model = "factors") # accelleration factor suggest 5 and optimal coordinantes suggest 5
summary(nFact)
plotnScree(nFact) # 4 or 5 

# method 5 "comparasion data"
nFact <- EFACompData(na.omit(db.subscale), 8, n.pop = 10000, n.samples = 500, alpha = .30, graph = T,
                     corr.type = "pearson") # 5 

# apply EFA with oblimin
quest.1.efa <- fa(r = db.subscale, nfactors =5, rotate = "oblimin", fm = "ml")

print(quest.1.efa$loadings,cutoff = 0.2)

# calculate the factors loadings
s = factor.scores (db.subscale, quest.1.efa) # 
s


# ---------------------------------------- Figure 2 ----------------------------------------

#----------------------------------------- pannel 1
col_old = var.subscales
col_new  <- c("OCI-R: washing","OCI-R: checking","OCI-R: ordering","OCI-R: obsessing","OCI-R: hoarding","OCI-R: neutralising",
                             "EAT-26: oral control","EAT-26: dieting","EAT-26: bulimia","IAT: salience","IAT: excessive_use","IAT: neglect work",
                             "IAT: anticipation","IAT: lack of control","IAT: neglect social life","PMPUQSV: dangerous","PMPUQSV: prohibited",
                             "PMPUQSV: dependant", "STAI trait","STAI state", "PSS","CES-D","UPPS: urgency",
                             "UPPS: lack of premeditation", "UPPS: sensation seeking", "UPPS: lack of perseverance")


col_new  <- c("OCI-R: washing","OCI-R: checking","OCI-R: ordering","OCI-R: obsessing","OCI-R: hoarding","OCI-R: neutralising",
              "EAT-26: oral control","EAT-26: dieting","EAT-26: bulimia","IAT: salience","IAT: excessive use","IAT: neglect work",
              "IAT: anticipation","IAT: lack of control","IAT: neglect social life","PMPUQSV: prohibited",
              "PMPUQSV: dependant", "STAI trait","STAI state", "PSS","CES-D","UPPS: urgency",
              "UPPS: lack of premeditation", "UPPS: lack of perseverance")



colnames(db.subscale)[colnames(db.subscale) == col_old] <- col_new
corrmatrix <- cor(db.subscale, use="complete.obs")
col1 <- colorRampPalette(brewer.pal(9,"BrBG"))
corrplot(corrmatrix,method = "square", tl.col = "black", tl.cex = 0.75, sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100))
dev.print(pdf, file.path(figures_path,'Figure_EFA_subsccales_pannel_1.pdf'))
dev.off()


#----------------------------------------- pannel 2

# get loadings into a dataset
load = quest.1.efa$loadings
load = load[]
load = data.frame(load)
setDT(load,keep.rownames=TRUE)[]
colnames(load)[1] <- "Subscale"

# order factor so that is the same as in correlation plot
Ord <- c(24:1)
load$Subscale <- reorder(load$Subscale, Ord) 

loadings.m <- melt(load, id="Subscale", 
                   measure=c("ML3", "ML1","ML2", "ML4","ML5"), 
                   variable.name="Factor", value.name="Loading")


# name factors
labels <- c(ML3 = "Problematic media", ML1 = "Affective Stress",
            ML2 = "Problematic eating", ML4 = "Compulsivity", ML5 = "Impulsivity")




pp <- ggplot(loadings.m, aes(Subscale, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1, labeller = labeller(Factor = labels) ) + 
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  scale_fill_gradient2(name = "Loading", 
                       high = "#006666", mid = "white", low = "goldenrod4", 
                       midpoint=0, guide=F) +
  ylab("Loading Strength") +
  theme_bw(base_size=10)

# let's make the plot nice looking
ppp <-   pp + theme_bw(base_size = 10, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 8, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.position = "none") 

dev.print(pdf, file.path(figures_path,'Figure_EFA_subsccales_pannel_2.pdf'))
dev.off()


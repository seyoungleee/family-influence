# Independent Variable
# Gender - Gender
# Socioecon - Family socioeconomic status
# Grade - School year
# Race - Race & Ethnicity
# School - Particular school in CMU
# International - International student

# Dependent Variable
# Weighed FI1 - Family Influence 1
# Weighed FI2 - Family Influence 2
# Weighed FI3 - Family Influence 3
# MS Weighed - Major Satisfaction

# FP - Feelings about Parents
# IM - Information Management

# setwd()

data_set <- read.csv('Updated_MANOVA.csv') #read in the file
head(data_set) #display first few lines of data set
attach(data_set)

#yvars <- cbind(FI,MS,FP,IM)
yvars <- cbind(WeighedFI1,WeighedFI2,WeighedFI3,WeighedFI4)
#yvars <- cbind(Feeling.Connected.Weighed.Sum,Feeling.Controlled.Weighed.Sum)

#yvars <- MSsum
GEN_fac <- factor(Gender)
FAM_fac <- factor(Socioecon)
GRA_fac <- factor(Grade)
ETH_fac <- factor(Ethnicity)
SCH_fac <- factor(School)
SONA_fac <- factor(SONA)
MAJ_fac <- factor(ChangedMajor)
PAR_fac <- factor(ParentOccu)
INT_fac <- factor(International)
CAR_fac <- factor(Career_Choice_Pressure)
LIN_fac <- factor(Degree_Link_to_Career)


# Interpreting MANOVA
#mod <- manova(yvars~GEN_fac + FAM_fac + GRA_fac + ETH_fac + SCH_fac + INT_fac, data=data_set)
mod <- aov(yvars~GEN_fac + FAM_fac + GRA_fac + ETH_fac + SCH_fac + SONA_fac + MAJ_fac + PAR_fac + INT_fac + CAR_fac + LIN_fac, data=data_set)
mod

# Wilks Test
summary(mod, test="Wilks", intercept = TRUE)

# Roys Test
summary(mod, test="Roy", intercept = TRUE)

# Hotelling Lawley Test
summary(mod, test="Hotelling-Lawley", intercept = TRUE)

# Pillai Test
summary(mod, test="Pillai", intercept = TRUE)


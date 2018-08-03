
DT <- mm_heatherb_july19_2018

# Log C00, M00, 2T

DT$lgAsC00 <- log(DT$re_AsC00)
DT$lgAsM00 <- log(DT$re_AsM00)
DT$lgAs2T <- log(DT$re_AsM2T)

# Splitting into SGA, AGA, LGA

fivenum(DT$Fenton_Percentile00)
DT$FentonGA <- cut(DT$Fenton_Percentile00, c(0, 10, 90, 100), labels = c(1:3))
summary(DT$FentonGA)

# removing outlier

rmout <- DT[DT$lgAsM00 < "3.44" & DT$re_AsM00 < "10",]
fivenum(rmout$re_AsM00)

# Indexing complete cases

fivenum(DT$lgAsM00)
fivenum(DT$re_AsM00)
log(31.42)

F <- rmout[!is.na(rmout$lgAsM00), ]
F <- rmout[!is.na(rmout$FentonGA),]


# boxplot of lgAsM00 by Birth Weight for Gestational Age

install.packages("ggplot2")

library(ggplot2)
c <- ggplot(F, aes(factor(FentonGA),lgAsM00))+
        geom_boxplot(size=1.0, width=0.55, fatten=1.0)+
        theme_bw()+
        theme(plot.title = element_text(size=15, face="bold"))+
        theme(axis.title.x = element_text(face="bold", size=16))+
        theme(axis.title.y = element_text(face="bold", size=16)) +  
        theme(legend.title = element_text(size=16, face="bold"))+
        ylab("Log-transformed maternal arsenic")+
        xlab("Birth weight-for-gestational age category")+
        scale_x_discrete(breaks=c("1", "2", "3"), labels=c("SGA", "AGA", "LGA"))+
        stat_summary(fun.y=mean, geom="point", shape=5, size=4)+
        theme(text = element_text(size=16, face="bold"))+ 
        annotate("text", x = 3.3, y = 0.2, size =6, label = "P=0.009")
c

# Creating Table 1

install.packages("tableone")
library("tableone")

listVars <- c("mother_age2T", "mother_pre_bmi", "v11400", "gestage_comb00", "Fenton_Z_score00", "SES_3cat2T", "t212T", "sexo_h00", "smoke_inside2T")
catVars <- c("SES_3cat2T", "smoke_inside2T", "t212T", "sexo_h00")

table1 <- CreateTableOne(vars = listVars, data = F, factorVars = catVars)
table1

## Ranges 

fivenum(F$mother_age2T)
fivenum(F$mother_pre_bmi)
fivenum(F$v11400)
fivenum(F$gestage_comb00)
fivenum(F$Fenton_Z_score00)

## Cat Freq

table(F$sexo_h00)
393+337
393/730

table(F$smoke_inside2T)
510+217
217/727

# ANOVA

## lgasm00 and BWTGA 

aggregate(lgAsM00~FentonGA, F, mean)
AsM00FGA <- aov(F$lgAsM00~F$FentonGA)
summary(AsM00FGA)

## lgasm00 and maternal age
### create categories

F$matagecat[F$mother_age2T < 25] <- "1"
F$matagecat[F$mother_age2T >= 25 & F$mother_age2T < 35] <- "2"
F$matagecat[F$mother_age2T >= 35] <- "3"

table(F$matagecat)

### anova
aggregate(lgAsM00~matagecat, F, mean)
aggregate(lgAsM00~matagecat, F, sd)
AsM00MAC <- aov(F$lgAsM00~F$matagecat)
summary(AsM00MAC)

## lgasm00 and prepregnancy BMI
### create categories

F$bmicat[F$mother_pre_bmi < 25] <- "1"
F$bmicat[F$mother_pre_bmi >= 25 & F$mother_pre_bmi < 30] <- "2"
F$bmicat[F$mother_pre_bmi >= 30] <- "3"

table(F$bmicat)

### anova
aggregate(lgAsM00~bmicat, F, mean)
aggregate(lgAsM00~bmicat, F, sd)
AsM00bmic <- aov(F$lgAsM00~F$bmicat)
summary(AsM00bmic)

## lgasm00 and SES

table(F$SES_3cat2T)

### anova
aggregate(lgAsM00~SES_3cat2T, F, mean)
aggregate(lgAsM00~SES_3cat2T, F, sd)
AsM00sesc <- aov(F$lgAsM00~F$SES_3cat2T)
summary(AsM00sesc)

## lgasm00 and household smoke exposure

table(F$smoke_inside2T)

### anova
aggregate(lgAsM00~smoke_inside2T, F, mean)
aggregate(lgAsM00~smoke_inside2T, F, sd)
AsM00hsec <- aov(F$lgAsM00~F$smoke_inside2T)
summary(AsM00hsec)

## lgasm00 and infant sex

table(F$sexo_h00)

### anova
aggregate(lgAsM00~sexo_h00, F, mean)
aggregate(lgAsM00~sexo_h00, F, sd)
AsM00sexc <- aov(F$lgAsM00~F$sexo_h00)
summary(AsM00sexc)

# Determining missing variables
sum(complete.cases(F$mother_age2T))
sum(complete.cases(F$mother_pre_bmi))
sum(complete.cases(F$Fenton_Z_score00))
sum(complete.cases(F$v11400))
sum(complete.cases(F$SES_3cat2T))
sum(complete.cases(F$t212T))
sum(complete.cases(F$sexo_h00))
sum(complete.cases(F$smoke_inside2T))

setwd("C:/Users/Anne Mullin/Documents/Penn/Arsenic/R Studio/paper/FArsenic/anne2")

#testing github


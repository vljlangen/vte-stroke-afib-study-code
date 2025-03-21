###ALL FU max with OAC adjustments####


library(haven)

Ranalyysit.dt <- data.frame(read_sav("VTEincidentcohort.sav"))
head(Ranalyysit.dt)

save(Ranalyysit.dt,file="Ranalyysitdt.RData")

library(Epi)

load(file="Ranalyysitdt.RData")

with(Ranalyysit.dt,range(cal.yr(LAST_FollowUP_date_end_stroke_death,format = "%Y-%m-%d") ))
with(Ranalyysit.dt,range(cal.yr(DeathDate,format = "%Y-%m-%d"),na.rm = TRUE))

apu1<-with(Ranalyysit.dt,factor(rep("SOF",nrow(Ranalyysit.dt)),levels=c("SOF","EOF","IS")))
apu2<-with(Ranalyysit.dt,factor(ifelse(AnyISafterAF==1,"IS","EOF"),levels=c("SOF","EOF","IS")))

table(apu1,useNA = "always")
table(apu2,useNA = "always")

tmp.dt<-Lexis(entry=list(age=AgeJuly1stCohort,
                         fu=0,
                         per=cal.yr(CohortEntryDate,format = "%Y-%m-%d")),
              duration = cal.yr(LAST_FollowUP_date_end_stroke_death,format = "%Y-%m-%d")-
                cal.yr(CohortEntryDate,format = "%Y-%m-%d"),
              entry.status = apu1,
              exit.status = apu2,
              id=SID,
              data=Ranalyysit.dt)

summary(tmp.dt)
timeScales(tmp.dt)

tmp.dt1<-cutLexis(data = tmp.dt,
                  cut=cal.yr(tmp.dt$FirstOACdate,format = "%Y-%m-%d"),
                  timescale = "per",
                  new.state = "AK",
                  new.scale = "AK.Start"
)
summary(tmp.dt1)


tmp.dt2<-cutLexis(data = tmp.dt1,
                  cut=cal.yr(tmp.dt1$OACend,format = "%Y-%m-%d"),
                  timescale = "per",
                  new.state = "AK.quitted",
                  new.scale = "AK.quit"
)

summary(tmp.dt2)

library(cohorttools)

boxesLx(tmp.dt2,show.persons=FALSE)




save(tmp.dt2,file="tmpdt2.RData")

load(file="tmpdt2.RData")

# Split by calendar year (per)
range(tmp.dt2$per)
tmp.dt3<-splitLexis(lex=tmp.dt2,time.scale = "per",breaks = c(2011,2015))


# Year as factor
tmp.dt3$per.c<-timeBand(lex = tmp.dt3,time.scale = "per",type="factor")
# Year numeric
tmp.dt3$per.num<-with(tmp.dt3,per+lex.dur/2)

# Age as factor, age in middle of time slice
apu<-with(tmp.dt3,cut(age+lex.dur/2,c(0,40,50,60,70,80,90,100,Inf)))
tmp.dt3$age.c<-apu
table(apu,tmp.dt3$lex.Xst)

# Factor names
names(tmp.dt3)

# Sex
apu<-factor(unclass(tmp.dt3$SexCategory),levels=0:1,labels =c("female","male"))
tmp.dt3$sex<-apu

tmp.dt3$sex <- relevel(tmp.dt3$sex, ref = "male")

# Other variables
apu<-unclass(tmp.dt3$HyperlipidemiaBOAC);table(apu)
tmp.dt3$Hyperlipidemia<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$HypertensionBOAC);table(apu)
tmp.dt3$Hypertension<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DiabetesBOAC);table(apu)
tmp.dt3$Diabetes<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$CongestiveHeartFailureBOAC);table(apu)
tmp.dt3$HF<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AbnormalLiverFunctionBOAC);table(apu)
tmp.dt3$LiverVT<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AbnormalRenalFunctionBOAC);table(apu)
tmp.dt3$RenalVT<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$BleedingsBOAC);table(apu)
tmp.dt3$Bleeding<-factor(apu,levels=0:1,labels =c("no","yes"))



apu<-unclass(tmp.dt3$AlcoholBOAC);table(apu)
tmp.dt3$ALKO<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AnyVascularDiseaseBOAC);table(apu)
tmp.dt3$anyvasc<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$CancerBeforeOrAtCohort);table(apu)
tmp.dt3$syopa<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DementiaBOAC);table(apu)
tmp.dt3$dementia<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$PsychiatricDiseaseBOAC);table(apu)
tmp.dt3$psykiatria<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$NTulot_1);table(apu)
tmp.dt3$tulot3luokkaa<-factor(apu,levels=1:3,labels =c("low","mid", "high"))

apu<-unclass(tmp.dt3$IschemicStrokeBOAC);table(apu)
tmp.dt3$stroke<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$low2moderate3high);table(apu)
tmp.dt3$low2moderate3high<-factor(apu,levels=1:3,labels =c("low","mid", "high"))

apu<-unclass(tmp.dt3$CoronaryHeartDiseaseBOAC);table(apu)
tmp.dt3$MCC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$MyocardialInfarctionBOAC);table(apu)
tmp.dt3$MI<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$VascularDiseaseBOAC);table(apu)
tmp.dt3$PAD<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$PulmonaryEmbolismBOAC);table(apu)
tmp.dt3$PulmonaryEmbolismBOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DVT_BOAC);table(apu)
tmp.dt3$DVT_BOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$ANY_VTE_BOAC);table(apu)
tmp.dt3$ANY_VTE_BOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

save(tmp.dt3,file="tmpdt3.RData")

#------------------------------
#------------------------------
# Crude rates
#------------------------------
#------------------------------
library(dplyr)

# Select patients based on "yes"/"no" values
pe_patients <- tmp.dt3 %>% filter(PulmonaryEmbolismBOAC == "yes")
dvt_patients <- tmp.dt3 %>% filter(DVT_BOAC == "yes")
vte_patients <- tmp.dt3 %>% filter(ANY_VTE_BOAC == "yes")

NOpe_patients <- tmp.dt3 %>% filter(PulmonaryEmbolismBOAC == "no")
NOdvt_patients <- tmp.dt3 %>% filter(DVT_BOAC == "no")
NOvte_patients <- tmp.dt3 %>% filter(ANY_VTE_BOAC == "no")

# Run mkratetable analyses for each patient group
tmp.rtpe <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = pe_patients, scale = 100, add.RR = TRUE)
tmp.rtdvt <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = dvt_patients, scale = 100, add.RR = TRUE)
tmp.rtvte <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = vte_patients, scale = 100, add.RR = TRUE)
tmp.rtNOPE <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = NOpe_patients, scale = 100, add.RR = TRUE)
tmp.rtNODVT <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = NOdvt_patients, scale = 100, add.RR = TRUE)
tmp.rtNOVTE <- mkratetable(Surv(lex.dur, lex.Xst == "IS") ~ per.c, data = NOvte_patients, scale = 100, add.RR = TRUE)

sink(file="CRUDE_RATES.html")
knitr::kable(tmp.rtpe,caption="Rates PE",format="html",digits=3)
knitr::kable(tmp.rtdvt,caption="Rates DVT",format="html",digits=3)
knitr::kable(tmp.rtvte,caption="Rates VTE",format="html",digits=3)
knitr::kable(tmp.rtNOPE,caption="Rates NO PE",format="html",digits=3)
knitr::kable(tmp.rtNODVT,caption="Rates NO DVT",format="html",digits=3)
knitr::kable(tmp.rtNOVTE,caption="Rates NO VTE",format="html",digits=3)
sink()


#------------------------------
#------------------------------
# Rate table
#------------------------------

tmp.rt<-mkratetable(Surv(lex.dur,lex.Xst=="IS")~per.c+age.c+Relevel(lex.Cst,list(1,2:3,4))+
                      sex+Hyperlipidemia+Hypertension+MI+PulmonaryEmbolismBOAC+DVT_BOAC+ANY_VTE_BOAC+PAD+MCC+Diabetes+psykiatria+dementia+syopa+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+HF,
                    data=tmp.dt3,scale=100,add.RR = TRUE)

sink(file="RateTableSLTPEallfollowup.html")
knitr::kable(tmp.rt,caption="Rate table (1/100 person years)",format="html",digits=3)
sink()

tmp.PE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))+Hypertension+age.c+tulot3luokkaa+
                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                HF+Diabetes+per.c+PulmonaryEmbolismBOAC,
              data=tmp.dt3,family="poisreg")

tmp.SLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))+Hypertension+age.c+tulot3luokkaa+
                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                HF+Diabetes+per.c+DVT_BOAC,
              data=tmp.dt3,family="poisreg")

tmp.PESLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))+Hypertension+age.c+tulot3luokkaa+
                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                HF+Diabetes+per.c+ANY_VTE_BOAC,
              data=tmp.dt3,family="poisreg")

sink(file="AdjustedAnalysisALLfollowupPESLT.html")

knitr::kable(round(ci.exp(tmp.PE),2),caption="Adjusted IRR for PE from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.SLT),2),caption="Adjusted IRR for SLT from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.PESLT),2),caption="Adjusted IRR for combined PE and SLP from Poisson regression model",format="html")

sink()

tmp.dt3$lex.Cst.uusi<-droplevels(tmp.dt3$lex.Cst)

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+PulmonaryEmbolismBOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1PE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ lex.Cst.uusi+Hypertension+age.c+tulot3luokkaa+
                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                HF+Diabetes+per.c+PulmonaryEmbolismBOAC,
              data=tmp.dt3,
              family="poisreg",maxit = 200)

tmp.m2PE <- update(tmp.m1PE, ~ .+PulmonaryEmbolismBOAC:per.c)

PEinteractiom <- anova(tmp.m1PE,tmp.m2PE,test="Chisq")

round(ci.exp(tmp.m2PE),3)


tmp.dfPE<-with(tmp.dt3,
             expand.grid( lex.Cst.uusi=sort(unique(lex.Cst.uusi))[1],
                          
                          Diabetes=sort(unique(Diabetes))[1],
                          age.c=sort(unique(age.c))[1],
                          sex=sort(unique(sex))[1],
                          Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                          tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                          dementia=sort(unique(dementia))[1],
                          HF=sort(unique(HF))[1],
                          Hypertension=sort(unique(Hypertension))[1],
                          ALKO=sort(unique(ALKO))[1],
                          Bleeding=sort(unique(Bleeding))[1],
                          stroke=sort(unique(stroke))[1],
                          LiverVT=sort(unique(LiverVT))[1],
                          RenalVT=sort(unique(RenalVT))[1],
                          anyvasc=sort(unique(anyvasc))[1],
                          per.c=sort(unique(per.c)),
                          PulmonaryEmbolismBOAC=sort(unique(PulmonaryEmbolismBOAC))))

tmp.mtrPE<-model.matrix(~lex.Cst.uusi + Hypertension + age.c +tulot3luokkaa+
                        sex + Hyperlipidemia  + dementia  +
                        anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                        HF + Diabetes + per.c + PulmonaryEmbolismBOAC:per.c,data=tmp.dfPE)


apu.allPE<-as.data.frame(ci.exp(tmp.m2PE,ctr.mat=(tmp.mtrPE[-(1:3),]-tmp.mtrPE[(1:3),])))

names(apu.allPE)<-c("MRR","MRR.lo","MRR.hi");

apu.allPE$per<-c("2007-2010","2011-2014","2015-2018")


pPE <- ggplot(apu.allPE, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("A") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5, 1.7, by = 0.25), oob = scales::squish)



pPE

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+DVT_BOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1SLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ lex.Cst.uusi+Hypertension+age.c+tulot3luokkaa+
                  sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                  HF+Diabetes+per.c+DVT_BOAC,
                data=tmp.dt3,
                family="poisreg",maxit = 200)

tmp.m2SLT <- update(tmp.m1SLT, ~ .+DVT_BOAC:per.c)

SLTinteractiom <- anova(tmp.m1SLT,tmp.m2SLT,test="Chisq")

round(ci.exp(tmp.m2SLT),3)


tmp.dfSLT<-with(tmp.dt3,
               expand.grid( lex.Cst.uusi=sort(unique(lex.Cst.uusi))[1],
                            
                            Diabetes=sort(unique(Diabetes))[1],
                            age.c=sort(unique(age.c))[1],
                            sex=sort(unique(sex))[1],
                            Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                            tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                            dementia=sort(unique(dementia))[1],
                            HF=sort(unique(HF))[1],
                            Hypertension=sort(unique(Hypertension))[1],
                            ALKO=sort(unique(ALKO))[1],
                            Bleeding=sort(unique(Bleeding))[1],
                            stroke=sort(unique(stroke))[1],
                            LiverVT=sort(unique(LiverVT))[1],
                            RenalVT=sort(unique(RenalVT))[1],
                            anyvasc=sort(unique(anyvasc))[1],
                            per.c=sort(unique(per.c)),
                            DVT_BOAC=sort(unique(DVT_BOAC))))

tmp.mtrSLT<-model.matrix(~lex.Cst.uusi + Hypertension + age.c +tulot3luokkaa+
                          sex + Hyperlipidemia  + dementia  +
                          anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                          HF + Diabetes + per.c + DVT_BOAC:per.c,data=tmp.dfSLT)


apu.allSLT<-as.data.frame(ci.exp(tmp.m2SLT,ctr.mat=(tmp.mtrSLT[-(1:3),]-tmp.mtrSLT[(1:3),])))

names(apu.allSLT)<-c("MRR","MRR.lo","MRR.hi");

apu.allSLT$per<-c("2007-2010","2011-2014","2015-2018")


pSLT <- ggplot(apu.allSLT, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("B") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5, 1.7, by = 0.25), oob = scales::squish)



pSLT

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+ANY_VTE_BOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1SLTPE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ lex.Cst.uusi+Hypertension+age.c+tulot3luokkaa+
                   sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                   HF+Diabetes+per.c+ANY_VTE_BOAC,
                 data=tmp.dt3,
                 family="poisreg",maxit = 200)

tmp.m2SLTPE <- update(tmp.m1SLTPE, ~ .+ANY_VTE_BOAC:per.c)

SLTPEinteractiom <- anova(tmp.m1SLTPE,tmp.m2SLTPE,test="Chisq")

round(ci.exp(tmp.m2SLTPE),3)


tmp.dfSLTPE<-with(tmp.dt3,
                expand.grid( lex.Cst.uusi=sort(unique(lex.Cst.uusi))[1],
                             
                             Diabetes=sort(unique(Diabetes))[1],
                             age.c=sort(unique(age.c))[1],
                             sex=sort(unique(sex))[1],
                             Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                             tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                             dementia=sort(unique(dementia))[1],
                             HF=sort(unique(HF))[1],
                             Hypertension=sort(unique(Hypertension))[1],
                             ALKO=sort(unique(ALKO))[1],
                             Bleeding=sort(unique(Bleeding))[1],
                             stroke=sort(unique(stroke))[1],
                             LiverVT=sort(unique(LiverVT))[1],
                             RenalVT=sort(unique(RenalVT))[1],
                             anyvasc=sort(unique(anyvasc))[1],
                             per.c=sort(unique(per.c)),
                             ANY_VTE_BOAC=sort(unique(ANY_VTE_BOAC))))

tmp.mtrSLTPE<-model.matrix(~lex.Cst.uusi + Hypertension + age.c +tulot3luokkaa+
                           sex + Hyperlipidemia  + dementia  +
                           anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                           HF + Diabetes + per.c + ANY_VTE_BOAC:per.c,data=tmp.dfSLTPE)


apu.allSLTPE<-as.data.frame(ci.exp(tmp.m2SLTPE,ctr.mat=(tmp.mtrSLTPE[-(1:3),]-tmp.mtrSLTPE[(1:3),])))

names(apu.allSLTPE)<-c("MRR","MRR.lo","MRR.hi");

apu.allSLTPE$per<-c("2007-2010","2011-2014","2015-2018")


pSLTPE <- ggplot(apu.allSLTPE, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("C") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 1.7), breaks = seq(0.5, 1.7, by = 0.25), oob = scales::squish)



pSLTPE

###### survival curve#####

library(Epi)


library(Epi)
library(cohorttools)
tmp.dtcurve <- data.frame(read_sav("VTEincidentcohort.sav"))

# Create the duration variable
tmp.dtcurve$duration <- with(tmp.dtcurve, 
                        cal.yr(LAST_FollowUP_date_end_stroke_death, format = "%Y-%m-%d") - 
                          cal.yr(CohortEntryDate, format = "%Y-%m-%d"))


# Load necessary libraries
library(survival)
library(survminer)

# Create a survival object (1 - survival function)
surv_object <- with(tmp.dtcurve, Surv(duration, AnyISafterAF == 1))

# Define a new custom broken color palette with distinct blue and red shades
custom_colors <- c("#E31A1C", "#1F77B4")


# Check the structure of the dataset to ensure the conversion
str(tmp.dtcurve)

# Fit a Kaplan-Meier model
km_fit <- survfit(surv_object ~ ANY_VTE_BOAC, data = tmp.dtcurve)

# Plot the cumulative incidence curve (1 - survival function)
plot <- ggsurvplot(
  fit = km_fit,
  data = tmp.dtcurve,
  palette = custom_colors,  # Apply custom color scheme
  fun = "event",  # 1 - survival function
  conf.int = TRUE,  # Include 95% confidence intervals
  risk.table = TRUE,  # Show number of persons at risk
  censor = FALSE,
  pval = FALSE,  # Set TRUE if you want to add a p-value for group comparison
  xlab = "Follow-up time (years)",
  ylab = "Cumulative incidence",
  xlim = c(0, 10),
  ylim = c(0, 0.20),
  break.x.by = 2,  # Set x-axis ticks every 2 years
  ggtheme = theme_minimal(base_size = 14) +  # Increase the base font size for the plot
    theme(
      legend.title = element_text(size = 14),   # Increase legend title size
      legend.text = element_text(size = 14),    # Increase legend label size
      axis.title = element_text(size = 14),     # Increase axis titles size
      axis.text = element_text(size = 13),      # Increase axis labels size
      risk.table.title = "",  # Increase risk table title size
      risk.table.fontsize = 9                    # Increase risk table text size
    ),
  legend.title = "",  # Title for the legend
  legend.labs = c("No VTE", "VTE"),  # Labels for the legend groups
  risk.table.height = 0.25
)
# Print the plot to check
print(plot)



################# low risk####

# Create a subgroup where low2moderate3high is either "low" or "mid"
low_mid_subset <- tmp.dt3[tmp.dt3$low2moderate3high %in% c("low", "mid"), ]


tmp.rt<-mkratetable(Surv(lex.dur,lex.Xst=="IS")~PulmonaryEmbolismBOAC+DVT_BOAC+ANY_VTE_BOAC,
                    data=low_mid_subset,scale=100,add.RR = TRUE)

tmp.PE_lowANDmoderate <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))++Hypertension+age.c+tulot3luokkaa+
                               sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+LiverVT+RenalVT+
                               HF+Diabetes+per.c+PulmonaryEmbolismBOAC,
                             data=low_mid_subset,family="poisreg")

tmp.SLT_lowANDmoderate <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))++Hypertension+age.c+tulot3luokkaa+
                                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+LiverVT+RenalVT+
                                HF+Diabetes+per.c+DVT_BOAC,
                              data=low_mid_subset,family="poisreg")

tmp.PESLT_lowANDmoderate <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Relevel(lex.Cst,list(1,2,3,4))++Hypertension+age.c+tulot3luokkaa+
                                  sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+LiverVT+RenalVT+
                                  HF+Diabetes+per.c+ANY_VTE_BOAC,
                                data=low_mid_subset,family="poisreg")

sink(file="AdjustedAnalysisALLfollowupPESLT_lowOrModeraterisk.html")

knitr::kable(round(ci.exp(tmp.PE_lowANDmoderate),2),caption="Adjusted IRR for PE from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.SLT_lowANDmoderate),2),caption="Adjusted IRR for SLT from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.PESLT_lowANDmoderate),2),caption="Adjusted IRR for combined PE and SLP from Poisson regression model",format="html")

sink()

sink(file="RateTableSLTPEallfollowup_lowANDmoderaterisk.html")
knitr::kable(tmp.rt,caption="Rate table (1/100 person years)",format="html",digits=3)
sink()


##############################
###ALL FU BUT WITHOUT OAC ####
##############################


library(haven)

Ranalyysit.dt <- data.frame(read_sav("VTEincidentcohort.sav"))
head(Ranalyysit.dt)

save(Ranalyysit.dt,file="Ranalyysitdt.RData")

library(Epi)

load(file="Ranalyysitdt.RData")

with(Ranalyysit.dt,range(cal.yr(LAST_FollowUP_WITHOUT_OAC_date_end_stroke_death_OAC,format = "%Y-%m-%d") ))
with(Ranalyysit.dt,range(cal.yr(DeathDate,format = "%Y-%m-%d"),na.rm = TRUE))

apu1<-with(Ranalyysit.dt,factor(rep("SOF",nrow(Ranalyysit.dt)),levels=c("SOF","EOF","IS")))
apu2<-with(Ranalyysit.dt,factor(ifelse(AnyISafterAF_beforeOAC==1,"IS","EOF"),levels=c("SOF","EOF","IS")))

table(apu1,useNA = "always")
table(apu2,useNA = "always")

tmp.dt2<-Lexis(entry=list(age=AgeJuly1stCohort,
                         fu=0,
                         per=cal.yr(CohortEntryDate,format = "%Y-%m-%d")),
              duration = cal.yr(LAST_FollowUP_WITHOUT_OAC_date_end_stroke_death_OAC,format = "%Y-%m-%d")-
                cal.yr(CohortEntryDate,format = "%Y-%m-%d"),
              entry.status = apu1,
              exit.status = apu2,
              id=SID,
              data=Ranalyysit.dt)


summary(tmp.dt2)
timeScales(tmp.dt2)



library(cohorttools)

boxesLx(tmp.dt2,show.persons=FALSE)




save(tmp.dt2,file="tmpdt2.RData")

load(file="tmpdt2.RData")

# Split by calendar year (per)
range(tmp.dt2$per)
tmp.dt3<-splitLexis(lex=tmp.dt2,time.scale = "per",breaks = c(2011,2015))


# Year as factor
tmp.dt3$per.c<-timeBand(lex = tmp.dt3,time.scale = "per",type="factor")
# Year numeric
tmp.dt3$per.num<-with(tmp.dt3,per+lex.dur/2)

# Age as factor, age in middle of time slice
apu<-with(tmp.dt3,cut(age+lex.dur/2,c(0,40,50,60,70,80,90,100,Inf)))
tmp.dt3$age.c<-apu
table(apu,tmp.dt3$lex.Xst)

# Factor names
names(tmp.dt3)

# Sex
apu<-factor(unclass(tmp.dt3$SexCategory),levels=0:1,labels =c("female","male"))
tmp.dt3$sex<-apu

tmp.dt3$sex <- relevel(tmp.dt3$sex, ref = "male")

# Other variables
apu<-unclass(tmp.dt3$HyperlipidemiaBOAC);table(apu)
tmp.dt3$Hyperlipidemia<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$HypertensionBOAC);table(apu)
tmp.dt3$Hypertension<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DiabetesBOAC);table(apu)
tmp.dt3$Diabetes<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$CongestiveHeartFailureBOAC);table(apu)
tmp.dt3$HF<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AbnormalLiverFunctionBOAC);table(apu)
tmp.dt3$LiverVT<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AbnormalRenalFunctionBOAC);table(apu)
tmp.dt3$RenalVT<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$BleedingsBOAC);table(apu)
tmp.dt3$Bleeding<-factor(apu,levels=0:1,labels =c("no","yes"))



apu<-unclass(tmp.dt3$AlcoholBOAC);table(apu)
tmp.dt3$ALKO<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$AnyVascularDiseaseBOAC);table(apu)
tmp.dt3$anyvasc<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$CancerBeforeOrAtCohort);table(apu)
tmp.dt3$syopa<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DementiaBOAC);table(apu)
tmp.dt3$dementia<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$PsychiatricDiseaseBOAC);table(apu)
tmp.dt3$psykiatria<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$NTulot_1);table(apu)
tmp.dt3$tulot3luokkaa<-factor(apu,levels=1:3,labels =c("low","mid", "high"))

apu<-unclass(tmp.dt3$IschemicStrokeBOAC);table(apu)
tmp.dt3$stroke<-factor(apu,levels=0:1,labels =c("no","yes"))

#apu<-unclass(tmp.dt3$Koulutus1perus2toinen3korkeinUusi);table(apu)
#tmp.dt3$koulutus<-factor(apu,levels=1:3,labels =c("low","mid", "high"))

apu<-unclass(tmp.dt3$low2moderate3high);table(apu)
tmp.dt3$low2moderate3high<-factor(apu,levels=1:3,labels =c("low","mid", "high"))

apu<-unclass(tmp.dt3$CoronaryHeartDiseaseBOAC);table(apu)
tmp.dt3$MCC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$MyocardialInfarctionBOAC);table(apu)
tmp.dt3$MI<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$VascularDiseaseBOAC);table(apu)
tmp.dt3$PAD<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$PulmonaryEmbolismBOAC);table(apu)
tmp.dt3$PulmonaryEmbolismBOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$DVT_BOAC);table(apu)
tmp.dt3$DVT_BOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

apu<-unclass(tmp.dt3$ANY_VTE_BOAC);table(apu)
tmp.dt3$ANY_VTE_BOAC<-factor(apu,levels=0:1,labels =c("no","yes"))

save(tmp.dt3,file="tmpdt3.RData")


#------------------------------
#------------------------------
# Rate table
#------------------------------

tmp.rt<-mkratetable(Surv(lex.dur,lex.Xst=="IS")~per.c+age.c+
                      sex+Hyperlipidemia+Hypertension+MI+PulmonaryEmbolismBOAC+DVT_BOAC+ANY_VTE_BOAC+PAD+MCC+Diabetes+psykiatria+dementia+syopa+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+HF,
                    data=tmp.dt3,scale=100,add.RR = TRUE)

sink(file="RateTableSLTPEallfollowup_NO_OAC.html")
knitr::kable(tmp.rt,caption="Rate table (1/100 person years)",format="html",digits=3)
sink()

tmp.PE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                HF+Diabetes+per.c+PulmonaryEmbolismBOAC,
              data=tmp.dt3,family="poisreg")

tmp.SLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                 sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                 HF+Diabetes+per.c+DVT_BOAC,
               data=tmp.dt3,family="poisreg")

tmp.PESLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                   sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                   HF+Diabetes+per.c+ANY_VTE_BOAC,
                 data=tmp.dt3,family="poisreg")

sink(file="AdjustedAnalysisALLfollowupPESLT_NO_OAC.html")

knitr::kable(round(ci.exp(tmp.PE),2),caption="Adjusted IRR for PE from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.SLT),2),caption="Adjusted IRR for SLT from Poisson regression model",format="html")
knitr::kable(round(ci.exp(tmp.PESLT),2),caption="Adjusted IRR for combined PE and SLP from Poisson regression model",format="html")

sink()

tmp.dt3$lex.Cst.uusi<-droplevels(tmp.dt3$lex.Cst)

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+PulmonaryEmbolismBOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1PE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                  sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                  HF+Diabetes+per.c+PulmonaryEmbolismBOAC,
                data=tmp.dt3,
                family="poisreg",maxit = 200)

tmp.m2PE <- update(tmp.m1PE, ~ .+PulmonaryEmbolismBOAC:per.c)

PEinteractiomNO_OAC <- anova(tmp.m1PE,tmp.m2PE,test="Chisq")

round(ci.exp(tmp.m2PE),3)


tmp.dfPE<-with(tmp.dt3,
               expand.grid( 
                            Diabetes=sort(unique(Diabetes))[1],
                            age.c=sort(unique(age.c))[1],
                            sex=sort(unique(sex))[1],
                            Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                            tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                            dementia=sort(unique(dementia))[1],
                            HF=sort(unique(HF))[1],
                            Hypertension=sort(unique(Hypertension))[1],
                            ALKO=sort(unique(ALKO))[1],
                            Bleeding=sort(unique(Bleeding))[1],
                            stroke=sort(unique(stroke))[1],
                            LiverVT=sort(unique(LiverVT))[1],
                            RenalVT=sort(unique(RenalVT))[1],
                            anyvasc=sort(unique(anyvasc))[1],
                            per.c=sort(unique(per.c)),
                            PulmonaryEmbolismBOAC=sort(unique(PulmonaryEmbolismBOAC))))

tmp.mtrPE<-model.matrix(~ Hypertension + age.c +tulot3luokkaa+
                          sex + Hyperlipidemia  + dementia  +
                          anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                          HF + Diabetes + per.c + PulmonaryEmbolismBOAC:per.c,data=tmp.dfPE)


apu.allPE<-as.data.frame(ci.exp(tmp.m2PE,ctr.mat=(tmp.mtrPE[-(1:3),]-tmp.mtrPE[(1:3),])))

names(apu.allPE)<-c("MRR","MRR.lo","MRR.hi");

apu.allPE$per<-c("2007-2010","2011-2014","2015-2018")


pPE_NO_OAC <- ggplot(apu.allPE, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("A") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 2.2), breaks = seq(0.5, 2.0, by = 0.25), oob = scales::squish)



pPE_NO_OAC

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+DVT_BOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1SLT <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                   sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                   HF+Diabetes+per.c+DVT_BOAC,
                 data=tmp.dt3,
                 family="poisreg",maxit = 200)

tmp.m2SLT <- update(tmp.m1SLT, ~ .+DVT_BOAC:per.c)

SLTinteractiomNO_OAC <- anova(tmp.m1SLT,tmp.m2SLT,test="Chisq")

round(ci.exp(tmp.m2SLT),3)


tmp.dfSLT<-with(tmp.dt3,
                expand.grid( 
                             Diabetes=sort(unique(Diabetes))[1],
                             age.c=sort(unique(age.c))[1],
                             sex=sort(unique(sex))[1],
                             Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                             tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                             dementia=sort(unique(dementia))[1],
                             HF=sort(unique(HF))[1],
                             Hypertension=sort(unique(Hypertension))[1],
                             ALKO=sort(unique(ALKO))[1],
                             Bleeding=sort(unique(Bleeding))[1],
                             stroke=sort(unique(stroke))[1],
                             LiverVT=sort(unique(LiverVT))[1],
                             RenalVT=sort(unique(RenalVT))[1],
                             anyvasc=sort(unique(anyvasc))[1],
                             per.c=sort(unique(per.c)),
                             DVT_BOAC=sort(unique(DVT_BOAC))))

tmp.mtrSLT<-model.matrix(~ Hypertension + age.c +tulot3luokkaa+
                           sex + Hyperlipidemia  + dementia  +
                           anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                           HF + Diabetes + per.c + DVT_BOAC:per.c,data=tmp.dfSLT)


apu.allSLT<-as.data.frame(ci.exp(tmp.m2SLT,ctr.mat=(tmp.mtrSLT[-(1:3),]-tmp.mtrSLT[(1:3),])))

names(apu.allSLT)<-c("MRR","MRR.lo","MRR.hi");

apu.allSLT$per<-c("2007-2010","2011-2014","2015-2018")


pSLT_NO_OAC <- ggplot(apu.allSLT, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("B") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 2.2), breaks = seq(0.5, 2.0, by = 0.25), oob = scales::squish)



pSLT_NO_OAC

mkratetable(Surv(lex.dur,lex.Xst=="IS")~ lex.Cst.uusi+ANY_VTE_BOAC+per.c,data=tmp.dt3,scale=100)


tmp.m1SLTPE <- glm(cbind(lex.Xst=="IS",lex.dur) ~ Hypertension+age.c+tulot3luokkaa+
                     sex+Hyperlipidemia+dementia+anyvasc+ALKO+Bleeding+stroke+LiverVT+RenalVT+
                     HF+Diabetes+per.c+ANY_VTE_BOAC,
                   data=tmp.dt3,
                   family="poisreg",maxit = 200)

tmp.m2SLTPE <- update(tmp.m1SLTPE, ~ .+ANY_VTE_BOAC:per.c)

SLTPEinteractiomNO_OAC <- anova(tmp.m1SLTPE,tmp.m2SLTPE,test="Chisq")

round(ci.exp(tmp.m2SLTPE),3)


tmp.dfSLTPE<-with(tmp.dt3,
                  expand.grid(
                               Diabetes=sort(unique(Diabetes))[1],
                               age.c=sort(unique(age.c))[1],
                               sex=sort(unique(sex))[1],
                               Hyperlipidemia=sort(unique(Hyperlipidemia))[1],
                               tulot3luokkaa=sort(unique(tulot3luokkaa))[1],
                               dementia=sort(unique(dementia))[1],
                               HF=sort(unique(HF))[1],
                               Hypertension=sort(unique(Hypertension))[1],
                               ALKO=sort(unique(ALKO))[1],
                               Bleeding=sort(unique(Bleeding))[1],
                               stroke=sort(unique(stroke))[1],
                               LiverVT=sort(unique(LiverVT))[1],
                               RenalVT=sort(unique(RenalVT))[1],
                               anyvasc=sort(unique(anyvasc))[1],
                               per.c=sort(unique(per.c)),
                               ANY_VTE_BOAC=sort(unique(ANY_VTE_BOAC))))

tmp.mtrSLTPE<-model.matrix(~Hypertension + age.c +tulot3luokkaa+
                             sex + Hyperlipidemia  + dementia  +
                             anyvasc + ALKO + Bleeding + stroke + LiverVT + RenalVT +
                             HF + Diabetes + per.c + ANY_VTE_BOAC:per.c,data=tmp.dfSLTPE)


apu.allSLTPE<-as.data.frame(ci.exp(tmp.m2SLTPE,ctr.mat=(tmp.mtrSLTPE[-(1:3),]-tmp.mtrSLTPE[(1:3),])))

names(apu.allSLTPE)<-c("MRR","MRR.lo","MRR.hi");

apu.allSLTPE$per<-c("2007-2010","2011-2014","2015-2018")


pSLTPE_NO_OAC <- ggplot(apu.allSLTPE, aes(x = per, y = MRR, group = 1)) +  # Specify 'group = 1' to connect all points with a single line
  geom_pointrange(aes(ymin = MRR.lo, ymax = MRR.hi), fatten = 2.5, lwd = 1.5, color = "#0D2C54") +
  geom_line(color = "#0D2C54") +  # Add a line connecting the points
  geom_hline(yintercept = 1, linetype = 3, color = "red") +
  labs(y = "Adjusted incidence rate ratio", x = "Calendar year", title = " ") +
  ggtitle("C") + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )+ scale_y_continuous(limits = c(0.5, 2.2), breaks = seq(0.5, 2.0, by = 0.25), oob = scales::squish)



pSLTPE_NO_OAC

library(gridExtra)

allplotsOAC<- grid.arrange(plot(pPE), plot(pSLT), plot(pSLTPE), 
                        
                        ncol = 3)

ggsave(plot = allplotsOAC,file="FIG1PESLTwithOAC.pdf")

library(gridExtra)
allplotsNO_OAC<- grid.arrange(plot(pPE_NO_OAC), plot(pSLT_NO_OAC), plot(pSLTPE_NO_OAC), 
                        
                        ncol = 3)

ggsave(plot = allplotsNO_OAC,file="FIG1PESLT_NO_OAC.pdf")

PEinteractiom
SLTinteractiom
SLTPEinteractiom

PEinteractiomNO_OAC
SLTinteractiomNO_OAC
SLTPEinteractiomNO_OAC



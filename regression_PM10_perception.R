survey_AP <- read.csv("C:/Users/Gianmarco/Desktop/ERASMUS Traineeship/PROJECT/Analysis_STAT/Results/survey_AP.csv")
View(survey_AP)

#valutare n° rispondenti
#table(survey_AP$MUN_ITA) #Molise(14) -> 4  ESCLUDERE?
#table(survey_AP$NUTS2)

hist(survey_AP$lik_airp_rel)
qqnorm(scale(survey_AP$lik_airp_rel))
abline(0,1)

library(dplyr)
survey_AP<-survey_AP %>%
  mutate(PM10_mean_10 = PM10_mean/10)


#CREARE 2 SUBSET ITA e SWEDEN
survey_AP_SWE <- survey_AP[survey_AP$BG_COUNTRY == "0",]
survey_AP_ITA <- survey_AP[survey_AP$BG_COUNTRY == "1",]

survey_AP_ITA<-subset(survey_AP_ITA,survey_AP_ITA$MUN_ITA!="14")#tolgo MOLISE

library("jtools")
library("ggplot2")
install.packages("ggstance")
install.packages("broom.mixed")

#install.packages("officer")
#install.packages("flextable")
#install.packages("huxtable")



#----------REGRESSION ANALYSYS-------------
#model = lm( Y ~ X1 + X2 + Xn , data = dataset) Y= variabile dipendente, X= variabile indipendente
#USARE LM con var RELATIVA - METTERE we - factor() per var CATEGORALI


#Likelihood
model_lik_ITA = lm (lik_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summary(model_lik_ITA)
summ(model_lik_ITA)
coefficients(model_lik_ITA)

model_res_lik = residuals(model_lik_ITA)
qqnorm(scale(model_res_lik))#qqplot RESIDUI
abline(0,1)
hist(model_res_lik)


model_lik_SWE = lm (lik_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summary(model_lik_SWE)
summ(model_lik_SWE)

#Impact on the individual
model_impact_ITA = lm (imp_ind_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summary(model_impact_ITA)
summ(model_impact_ITA)

model_impact_SWE = lm (imp_ind_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summary(model_impact_SWE)
summ(model_impact_SWE)

#----------PLOT--------
#likelihood
plot_lik<- plot_summs(model_lik_ITA, model_lik_SWE,
           coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
           model.names = list("ITALY", "SWEDEN"), scale = TRUE, point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Likelihood") + theme(plot.title = element_text(size = rel(2)))

plot_lik

ggsave(file ="C:/Users/Gianmarco/Desktop/ERASMUS Traineeship/PROJECT/Analysis_STAT/Results/plot_lik.png", plot_lik, width = 300, height = 150, units = "mm", dpi = 300) # ti salva il .png nella working directory


export_summs(model_lik_ITA, model_lik_SWE, 
             coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
             model.names = list("ITALY", "SWEDEN"), scale = TRUE, error_format = "[{conf.low}, {conf.high}]",
             to.file = "HTML", file.name = "likelihood.HTML")


#impact individual
plot_imp<- plot_summs(model_impact_ITA, model_impact_SWE, 
           coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
           model.names = list("ITALY", "SWEDEN"), scale = TRUE, point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Impact on the individual") + theme(plot.title = element_text(size = rel(2)))

plot_imp

ggsave(file ="C:/Users/Gianmarco/Desktop/ERASMUS Traineeship/PROJECT/Analysis_STAT/Results/plot_imp.png", plot_imp, width = 300, height = 150, units = "mm", dpi = 300) # ti salva il .png nella working directory


export_summs(model_impact_ITA, model_impact_SWE, 
             coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
             model.names = list("ITALY", "SWEDEN"), scale = TRUE, error_format = "[{conf.low}, {conf.high}]",
             to.file = "HTML", file.name = "individual_impact.HTML")


library("gridExtra") 
plot_lik_imp <- grid.arrange(plot_lik, plot_imp, ncol = 2)

ggsave(file ="C:/Users/Gianmarco/Desktop/ERASMUS Traineeship/PROJECT/Analysis_STAT/Results/plot_lik&imp.png", plot_lik_imp, width = 300, height = 150, units = "mm", dpi = 300) # ti salva il .png nella working directory


#---------PM10 vs Perception-------------

model_A_ITA <- lm(lik_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summary(model_A_ITA)
summ(model_A_ITA)

model_A_SWE <- lm(lik_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summary(model_A_SWE)
summ(model_A_SWE)


model_B_ITA <- lm(imp_ind_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summary(model_B_ITA)
summ(model_B_ITA)

model_B_SWE <- lm(imp_ind_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summary(model_B_SWE)
summ(model_B_SWE)


plot_PM10<-plot_summs(model_A_ITA, model_A_SWE, model_B_ITA, model_B_SWE,
           model.names = list("LIKELIHOOD - Italy", "LIKELIHOOD - Sweden", "IMPACT INDIVIDUAL - Italy", "IMPACT INDIVIDUAL - Sweden"),coefs = c("PM10 mean" = "PM10_mean_10"), point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) 
#+labs(title = "Likelihood & Impact on the individual") + theme(plot.title = element_text(size = rel(2))) 

plot_PM10

ggsave(file ="C:/Users/Gianmarco/Desktop/ERASMUS Traineeship/PROJECT/Analysis_STAT/Results/PM10_lik&imp.png", plot_PM10, width = 300, height = 150, units = "mm", dpi = 300) # ti salva il .png nella working directory











#---- creazione TABELLA----- 
#install.packages("stargazer")
library(stargazer)
stargazer(model_A_ITA, model_A_SWE, model_B_ITA, model_B_SWE,
          column.labels = c("ITALY","SWEDEN","ITALY","SWEDEN","ITALY","SWEDEN","ITALY","SWEDEN"),
          dep.var.labels=c("Likelihood","Impact on the individual", "Individual preparedness","Individual knowledge"), omit = "Constant",
          covariate.labels=c("PM10 mean"), omit.stat=c("ser","f", "adj.rsq"), ci=TRUE, align=TRUE,
          type = "text",out = "regression.html")#mettere type=html per export

         




#---------------ALTRO---------------

#RIFARE IN RISCHI RELATIVI!!!!!!!
#Individual preparedness
model_prep_ITA = lm (prep_ind_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summary(model_prep_ITA)
summ(model_prep_ITA)

model_prep_SWE = lm (prep_ind_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summary(model_prep_SWE)
summ(model_prep_SWE)

#Individual knowledge
model_know_ITA = lm (know_ind_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summary(model_know_ITA)
summ(model_know_ITA)

model_know_SWE = lm (know_ind_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summary(model_know_SWE)
summ(model_know_SWE)

#Authority preparedness
model_autprep_ITA = lm (prep_aut_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summary(model_autprep_ITA)
summ(model_autprep_ITA)

model_autprep_SWE = lm (prep_aut_airp ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summary(model_autprep_SWE)
summ(model_autprep_SWE)


#Individual preparedness
plot_summs(model_prep_ITA, model_prep_SWE, 
           coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
           model.names = list("ITALY", "SWEDEN"), scale = TRUE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Individual preparedness") + theme(plot.title = element_text(size = rel(2)))


export_summs(model_prep_ITA, model_prep_SWE, 
             coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
             model.names = list("ITALY", "SWEDEN"), scale = TRUE, error_format = "[{conf.low}, {conf.high}]",
             to.file = "HTML", file.name = "individual_prep.HTML")


#individual knowledge
plot_summs(model_know_ITA, model_know_SWE, 
           coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
           model.names = list("ITALY", "SWEDEN"), scale = TRUE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Individual knowledge") + theme(plot.title = element_text(size = rel(2)))


export_summs(model_know_ITA, model_know_SWE, 
             coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
             model.names = list("ITALY", "SWEDEN"), scale = TRUE, error_format = "[{conf.low}, {conf.high}]",
             to.file = "HTML", file.name = "individual_know.HTML")

#Authority preparedness
plot_summs(model_autprep_ITA, model_autprep_SWE, 
           coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
           model.names = list("ITALY", "SWEDEN"), scale = TRUE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Authority preparedness") + theme(plot.title = element_text(size = rel(2)))



#---------PM10 vs perception (ALTRO)------
model_C_ITA <- lm(prep_ind_airp ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summary(model_C_ITA)
summ(model_C_ITA)

model_C_SWE <- lm(prep_ind_airp ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summary(model_C_SWE)
summ(model_C_SWE)


model_D_ITA <- lm(know_ind_airp ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summary(model_D_ITA)
summ(model_D_ITA)

model_D_SWE <- lm(know_ind_airp ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summary(model_D_SWE)
summ(model_D_SWE)


plot_summs(model_C_ITA, model_C_SWE,
           model.names = list("ITALY", "SWEDEN"),coefs = c("PM10 mean" = "PM10_mean_10")) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Individual preparedness") + theme(plot.title = element_text(size = rel(2))) 

plot_summs(model_D_ITA, model_D_SWE,
           model.names = list("ITALY", "SWEDEN"),coefs = c("PM10 mean" = "PM10_mean_10"), 
           scale = TRUE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Individual knowledge") + theme(plot.title = element_text(size = rel(2))) 



export_summs(model_A_ITA, model_A_SWE, model_B_ITA, model_B_SWE,
             coefs = c("PM10 mean" = "PM10_mean_10"), 
             model.names = list("Likelihood ITALY", "SWEDEN"),
             error_format = "[{conf.low}, {conf.high}]")
#to.file = "HTML", file.name = "PM10vsAirP.HTML")

# Import library in R

library("dplyr")
library("jtools")
library("ggplot2")
library("gridExtra")

# Import data 

survey_AP <- read.csv("filelocation/survey_AP.csv")
View(survey_AP)

# Create column with PM10 concentrations of 10 ??g/m3 

survey_AP<-survey_AP %>%
  mutate(PM10_mean_10 = PM10_mean/10)

# Create 2 different dataframe for ITALY and SWEDEN

survey_AP_SWE <- survey_AP[survey_AP$BG_COUNTRY == "0",]

survey_AP_ITA <- survey_AP[survey_AP$BG_COUNTRY == "1",]
survey_AP_ITA<-subset(survey_AP_ITA,survey_AP_ITA$MUN_ITA!="14") # Exclude Molise region (less than 10 respondents)


# ----- Linear regression models -----

# Association between PM10 concentrations and the domains of risk perception

# Likelihood

# ITALY
model_A_ITA <- lm(lik_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summ(model_A_ITA)

# SWEDEN
model_A_SWE <- lm(lik_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summ(model_A_SWE)

# Impact on the individual

# ITALY
model_B_ITA <- lm(imp_ind_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_ITA) 
summ(model_B_ITA)

# SWEDEN
model_B_SWE <- lm(imp_ind_airp_rel ~ PM10_mean_10, weights = we, data = survey_AP_SWE) 
summ(model_B_SWE)

# Create Forest Plot -----

plot_PM10<-plot_summs(model_A_ITA, model_A_SWE, model_B_ITA, model_B_SWE,
                      model.names = list("LIKELIHOOD - Italy", "LIKELIHOOD - Sweden", "IMPACT INDIVIDUAL - Italy", "IMPACT INDIVIDUAL - Sweden"),coefs = c("PM10 mean" = "PM10_mean_10"), point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) 
plot_PM10

# Export the outcome in the working directory

ggsave(file ="filelocation/PM10_lik&imp.png", plot_PM10, width = 300, height = 150, units = "mm", dpi = 300) # ti salva il .png nella working directory


#Individual experience and socio-demographic factors associated with the domains of risk perception

# Likelihood

# ITALY
model_lik_ITA = lm (lik_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summ(model_lik_ITA)

# SWEDEN
model_lik_SWE = lm (lik_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summ(model_lik_SWE)

# Impact on the individual

# ITALY
model_impact_ITA = lm (imp_ind_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_ITA )
summ(model_impact_ITA)

# SWEDEN
model_impact_SWE = lm (imp_ind_airp_rel ~ BG_Gender + factor(BG_Age) + work + education + income + factor(politic) + smoke + exper_airp, weights = we, data = survey_AP_SWE )
summ(model_impact_SWE)

# Create Forest Plots -----

# Likelihood
plot_lik<- plot_summs(model_lik_ITA, model_lik_SWE,
                      coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
                      model.names = list("ITALY", "SWEDEN"), scale = TRUE, point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Likelihood") + theme(plot.title = element_text(size = rel(2)))

plot_lik

# Impact on the individual
plot_imp<- plot_summs(model_impact_ITA, model_impact_SWE, 
                      coefs = c("Males" = "BG_Gender", "Age 40-64" = "`factor(BG_Age)`1", "Age > 65" = "`factor(BG_Age)`2", "Employment"= "work", "University education"= "education","Relative income"=  "income", "Left/center-left"= "`factor(politic)`1", "Right/center-right"= "`factor(politic)`2", "Smokers"="smoke", "Individual experience"="exper_airp"),
                      model.names = list("ITALY", "SWEDEN"), scale = TRUE, point.shape = FALSE) + scale_x_continuous(labels = function(x) paste0(x*100, "%")) +labs(title = "Impact on the individual") + theme(plot.title = element_text(size = rel(2)))

plot_imp

# Combine the 2 forest plots and export the outcome in the working directory
 
plot_lik_imp <- grid.arrange(plot_lik, plot_imp, ncol = 2)

ggsave(file ="filelocation/plot_lik&imp.png", plot_lik_imp, width = 300, height = 150, units = "mm", dpi = 300)

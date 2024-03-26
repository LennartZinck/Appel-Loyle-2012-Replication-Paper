library(tidyverse)
library(stargazer)
library(ggpubr)
library(car)
library(robustbase)
library(haven)


#The Constructed Dataset by Appel and Loyle (2012) from 
#https://www.prio.org/journals/jpr/replicationdata
data_dupl <- read_dta("appelloyle_jprdata.dta")


#Duplication of Appel & Loyle Main Regression Table //appelloyle_jprdofile.do#----

ecoeq <- lm(v3Mdiff ~ truthvictim + fv8 + fv10 + fv11 + fv34 + xratf + labor +
              v64mean, data = data_dupl)
insteq <- lm(v3Mdiff ~ truthvictim + fv27 + polity2 , data = data_dupl) 
confeq <- lm(v3Mdiff ~ truthvictim + damage + cw_duration_lag 
             + peace_agreement_lag + victory_lag, data = data_dupl)

#Final EQ
fineq <- lm(v3Mdiff ~ truthvictim + fv8 + fv10 + fv11 + fv34 + fv27 + victory_lag +
              cw_duration_lag + damage + peace_agreement_lag + coldwar + polity2 +
              xratf + labor + v64mean, data = data_dupl)

#Main Regressiontable from Appel and Loyle (2012: 694) for the Appendix.
stargazer(ecoeq, insteq, confeq, fineq,
          dep.var.labels = c("Net FDI Inflow"),
          covariate.labels = c("Post-conflict justice", 
                                 "Economic Development", 
                                 "Economic Size", 
                                 "Economic Growth",
                                 "KAOPEN Index",
                                 "Exchange Rate",
                                 "Labor Force",
                                 "Life Expectancy Female",
                                 "Political Constraints",
                                 "Democracy Polity",
                                 "Damage",
                                 "Duration",
                                 "Peace Agreement",
                                 "Coldwar",
                                 "Victory",
                                 "Constant"),
            no.space=T,
            star.cutoffs = c(.05, .01, .001),
            star.char = c("*", "**", "***"),
            notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
            notes.append=FALSE, 
            title = "Appel and Loyle (2012) Main Regression Table",
            align = T,
            type = "text"
            )



#The Replication Paper 
#Descriptive Data Analysis#----------------------------------------------------

#Boxplot plotting FDI Inflows for the PCJ- and No PCJ-Group
PCJBox <- ggplot(data = data_dupl, aes(y = v3Mdiff)) +
  geom_boxplot(outlier.color = "Red") +
  stat_boxplot(geom = "errorbar") +
  ggtitle("Boxplot for the FDI and PCJ Variables") +
  facet_wrap( ~ truthvictim, labeller = as_labeller(c('0' = "No PCJ", '1' = "PCJ"))) +
  ylab("FDI net inflows") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
PCJBox

#Checking the reported mean difference between the PCJ and No-PCJ Group reported in Appel & Loyle (2012: 692)
PCJ <- data_dupl %>% filter(truthvictim == 1)
NOPCJ <- data_dupl %>% filter(truthvictim == 0)
mean(PCJ$v3Mdiff)
mean(NOPCJ$v3Mdiff)

#Their reported mean-values are correct, but comparing the medians seems more appropriate due to the rightskewed distribution of the variable
median(PCJ$v3Mdiff)
median(NOPCJ$v3Mdiff)


#Histograms for the DV (FDI Inflow) and Economic Size variable
ecobar1 <- ggplot(data = data_dupl, aes(x = fv10)) +
            geom_histogram()+
            xlab("")+
            ylab("")

ecobar2 <- ggplot(data = data_dupl, aes(x = v3Mdiff))+
            geom_histogram()+
            xlab("")+
            ylab("")

#New logarithm scale for economic size (fv10) & cube root (3rd root) of FDI Inflow + Descriptive Analysis
data_dupl$fv10log <- log10(data_dupl$fv10)
data_dupl$v3Mdiffcubic <- sign(data_dupl$v3Mdiff) * abs(data_dupl$v3Mdiff)^(1/3)

ecobar1log <- ggplot(data = data_dupl, aes(x = fv10log))+
  geom_histogram(binwidth = 0.1)+
  xlab("") +
  ylab("")

ecobar2cube <- ggplot(data = data_dupl, aes(x = v3Mdiffcubic))+
  geom_histogram(binwidth = 1)+
  xlab("")+
  ylab("")

#All Plots in One
transformed_plot <- ggarrange(ecobar2, ecobar2cube, ecobar1, ecobar1log,
                      nrow = 1, 
                      ncol = 2, 
                      label.y = 0.11, 
                      font.label = list(size = 10, 
                                        color = "black", 
                                        face = "italic", 
                                        family = NULL), 
                      labels = c("Net FDI Inflows", "Cube Root Net FDI Inflows", "Economic Size", "Log-normalized Economic Size"))

ggsave("Transformed Dependent Variable Plot.png", 
       plot = transformed_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)


#Outlier & Leverage Detection#-------------------------------------------------------------------------------

#Cook's distance for the main regression model from Appel and Loyle (2012)
influenceIndexPlot(fineq,
                   vars = "Cook",
                   id = list(method="y", 
                             n=1,
                             col="Grey", 
                             location="lr"),
                   main = "")


#Compute DFBETAS for the PCJ Coefficient
DFBETAS <- dfbetas(fineq)
par(mfrow=c(1,1)) 
plot(DFBETAS[, "truthvictim"], 
     ylab="PCJ")
abline(h = c(-0.2, 0.2), lty = 2)
points(x = 24, y = DFBETAS[24, "truthvictim"], pch = 16, col = "grey")
text(x = 24, y = DFBETAS[24, "truthvictim"], labels = "24", pos = 1, col = "grey")



#My Own Regression Models#-------------------------------------------------------------------------------
#Create a Dummy-Variable for the Observation (testnewid_lag = 3090000)
data_dupl <- data_dupl %>% 
              mutate(dum365 = ifelse(testnewid_lag == 3090000, 1, 0))

#The modified regression model with transformed economic variables
finown1 <- lm(v3Mdiffcubic ~ truthvictim + fv8 + fv10log + fv11 + fv34 + 
                fv27 + victory_lag + cw_duration_lag + damage + peace_agreement_lag +
                coldwar + polity2 + xratf + labor + v64mean, data = data_dupl)

#The modified regression models with dummy variable for the outlier
finown2 <- lm(v3Mdiffcubic ~ truthvictim + dum365 + fv8 + fv10log + fv11 + fv34 +
                fv27 + victory_lag + cw_duration_lag + damage + peace_agreement_lag +
                coldwar + polity2 + xratf + labor + v64mean, data = data_dupl)

#Regression model excluding the dummy variable from the analysis
data_dupl_fin <- data_dupl %>% filter(testnewid_lag != 3090000)
finfin <- lm(v3Mdiffcubic ~ truthvictim + fv8 + fv10log + fv11 + fv34 + fv27 + 
               victory_lag + cw_duration_lag + damage + peace_agreement_lag + 
               coldwar + polity2 + xratf + labor + v64mean, data = data_dupl_fin)

#Comparing them to the Main Regression Model by Appel and Loyle (2012)
stargazer(fineq, finown1, finown2, finfin, 
          star.cutoffs = c(.05, .01, .001),
          star.char = c("*", "**", "***"),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append=FALSE, 
          out = "ModelComparison.txt")


#Model Diagnostics#-------------------------------------------------------------------------------

#QQ plot for Main Regression Model 
qqnorm(residuals(fineq), main = "QQ-Plot for Model by Appel & Loyle (2012)")
qqline(residuals(fineq))
#QQ plot for My Model
qqnorm(residuals(finfin), main = "QQ-Plot for my adapted Model")
qqline(residuals(finfin))


#Residuals vs Fitted plot for Main Regression model
plot(fitted(fineq), residuals(fineq),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted for Model \nby Appel & Loyle (2012)")
abline(h = 0, col = "red")

#Residuals vs Fitted plot for My model
plot(fitted(finfin), residuals(finfin),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted for my adapted Model")
abline(h = 0, col = "red")




#Robustness Checks#-------------------------------------------------------------------------------

#Robust Regression of the Main Regression Model from Appel and Loyle (2012)
robreg <- lmrob(v3Mdiff ~ truthvictim + fv8 + fv10 + fv11 + fv34 + fv27 + 
                  victory_lag + cw_duration_lag + damage + peace_agreement_lag +
                  coldwar + polity2 + xratf + labor + v64mean, 
                data = data_dupl,
                setting="KS2014")
summary(robreg)


#Robust Regression with normalized scales for the FDI and Economic Size variable
robregfin <- lmrob(v3Mdiffcubic ~ truthvictim + fv8 + fv10log + fv11 + fv34 + 
                     fv27 + victory_lag + cw_duration_lag + damage + peace_agreement_lag +
                     coldwar + polity2 + xratf + labor + v64mean, 
                   data = data_dupl,
                   setting="KS2014")
summary(robregfin)

stargazer(robreg, robregfin, 
          star.cutoffs = c(.05, .01, .001),
          star.char = c("*", "**", "***"),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append= F, out = "RobRegmodel.txt")


#Dummying out of the outlier from the analysis based on the 
#main regression model proposed by Appel & Loyle (2012)

finincl1 <- lm(v3Mdiff ~ truthvictim + dum365 + fv8 + fv10 + fv11 + fv34 + fv27 +
                 victory_lag + cw_duration_lag + damage + peace_agreement_lag +
                 coldwar + polity2 + xratf + labor + v64mean, data = data_dupl)

#Exclusion of the outlier
finexcl <- lm(v3Mdiff ~ truthvictim + fv8 + fv10 + fv11 + fv34 + fv27 + victory_lag +
               cw_duration_lag + damage + peace_agreement_lag + coldwar + polity2 +
               xratf + labor + v64mean, data = data_dupl_fin)



stargazer(fineq, finincl1, finexcl,
          star.cutoffs = c(.05, .01, .001),
          star.char = c("*", "**", "***"),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append=FALSE, 
          covariate.labels = c("Post-conflict justice", 
                               "Economic Development", 
                               "Economic Size", 
                               "Economic Growth",
                               "KAOPEN Index",
                               "Exchange Rate",
                               "Labor Force",
                               "Life Expectancy Female",
                               "Political Constraints",
                               "Democracy Polity",
                               "Damage",
                               "Duration",
                               "Peace Agreement",
                               "Coldwar",
                               "Victory",
                               "Constant"),
          no.space = F,
          out = "ModelExclRussia.txt")

#After excluding the observation from the main regression model Appel and Loyle (2012)
#the Effect of PCJs is not statistically significant anymore.




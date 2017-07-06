library("sas7bdat")
library("dplyr")
library("plyr")

temp = list.files(pattern = ".sas7bdat")
exp_analysis_elig <- read.sas7bdat(file = "exp_analysis_elig.sas7bdat")
exp_analysis_wide <- read.sas7bdat(file = "exp_analysis_wide.sas7bdat")

save(exp_analysis_elig, file = "exp_analysis_elig.rda")
save(exp_analysis_wide, file = "exp_analysis_wide.rda")

setwd("G:/Air Pollution and Autism in Denmark/Data")
load(file = "exp_analysis_elig.rda")

#Data cleanning
elig = data.frame(exp_analysis_elig)
colnames(elig)
elig <- elig[,c(3:5,7,16:18,21,29,37,45:52,54,61,62,65)]
elig <- subset(elig, is.na(elig$not_eligible))


elig <- mutate(elig,m_age = ifelse(maternal_age <= 18,1,
                                   ifelse(maternal_age %in% 19:25,2,
                                          ifelse(maternal_age %in% 26:30,3,
                                                 ifelse(maternal_age %in% 31:35,4,5
                                                 )))))
elig <- mutate(elig,f_age = ifelse(paternal_age <= 18,1,
                                   ifelse(paternal_age %in% 19:25,2,
                                          ifelse(paternal_age %in% 26:30,3,
                                                 ifelse(paternal_age %in% 31:35,4,5
                                                 )))))
typeof(elig$maternal_smoking)
elig$maternal_smoking <- revalue(elig$maternal_smoking, c("00"="0","99"=NA))
elig$maternal_smoking <- as.numeric(as.character(elig$maternal_smoking))
typeof(elig$maternal_smoking)
elig <- mutate(elig,smoking = ifelse(maternal_smoking>0,1,
                                                      ifelse(maternal_smoking==0,0,NA)))

elig$birth_location_g[elig$birth_location_g <= 3] <- 1
elig$birth_location_g[elig$birth_location_g == 4] <- 2
elig$birth_location_g[elig$birth_location_g == 5] <- 3

elig$paternal_age[elig$paternal_age == -1] <- NA
elig$paternal_age[elig$paternal_age == 0] <- NA
elig$birthyear <- as.factor(elig$birthyear)
elig$m_age <- as.factor(elig$m_age)
elig$f_age <- as.factor(elig$f_age)
elig$birth_location_g <- as.character(as.numeric(elig$birth_location_g))
elig$birth_location_g <- as.factor(elig$birth_location_g)


#Data analysis
adjusted <- function(period,exposure){
  elig_sub <- elig[which(elig$period==period),]
  adj.model <- glm(elig_sub$case ~ elig_sub[[exposure]]+elig_sub$birthyear+elig_sub$gender+elig_sub$maternal_smoking
                          +elig_sub$m_age+elig_sub$f_age+elig_sub$birth_location_g,
                   data = elig_sub, family = binomial(link = logit))
  summary(adj.model)
  coef_exp <- data.frame(coef(adj.model))
  ci_exp <- data.frame(confint.default(adj.model))
  results <- exp(cbind(OR = coef_exp[2,],ci_exp[2,]))
  return(results)
}

exposure <- c("NO2_pregIQR","NOX_pregIQR","O3_pregIQR","CO_pregIQR","SO2_pregIQR",
              "PM10_pregIQR","PM2_5_pregIQR")

adjres_final <- data.frame(matrix(ncol = 3, nrow = 7))
colnames(adjres_final) <- c("OR","Lower_ci","Upper_ci")
rownames(adjres_final) <- c("NO2_pregIQR","NOX_pregIQR","O3_pregIQR","CO_pregIQR","SO2_pregIQR",
                         "PM10_pregIQR","PM2_5_pregIQR")

for(i in 1:length(exposure)){
  adjres_final[i,] <- data.frame(adjusted(period = "preg", exposure = exposure[i]))
}

adjres_final

#Spline


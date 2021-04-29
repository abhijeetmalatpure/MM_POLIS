library(survminer)
library(survival)

setwd("c:/Users/abhmalat/OneDrive - Indiana University/")
mm <- read.csv("cbio_MM/data_clinical_patient_ALL_MM.txt", header=FALSE, sep="\t")
mm <- mm[5:nrow(mm),]
colnames(mm) <- mm[1,]

mm <- mm[2:nrow(mm),]

#os_surv = Surv (as.numeric(mm$OS_MONTHS), mm$vital_status)

mm$survival_days <- as.integer(mm$OS_MONTHS)
mm$vital_status <- ifelse(mm$OS_STATUS=="LIVING", 1, 0)
mm$AGE <- as.integer(mm$AGE)
mm <- mm %>% mutate(age_group = ifelse(AGE <=5.0, "Peds", ifelse(AGE >5.0 & AGE <=18.0,"YoungAdult", "Adult")))

fit <- survfit(Surv(survival_days, vital_status) ~ GENDER, data = mm)
ggsurvplot(fit, data = mm, pval = TRUE, risk.table = TRUE)


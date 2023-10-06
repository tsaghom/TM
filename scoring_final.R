library(lubridate)
library(dplyr)
library(ggplot2)
library(smbinning)
# smbinning, variable expliquer doittre integer, variable explicative doittre "continuous characteristic"
library(woeBinning)
# package caret pour avoir plein de fonctions différentes au sein d'une méme fonction
library(sqldf)
library(MASS)
library(ROCR)
library(e1071)
library(randomForest)


getwd()
#Répertoire
setwd("C:/Users/Lenovo/Desktop/scoring")
# Importation des bases bd_12_2020.txt
base_telecom_2020_12 <- read.table (file = "bd_12_2020.txt",
                                    sep = ";" ,
                                    header = TRUE ,
                                    encoding = "UTF-8",
                                    stringsAsFactors = FALSE
                                   
)

# base_de_donnees_03_2021.txt

base_telecom_2021_03 <- read.table (file = "base_de_donnees_03_2021.txt",
                                    sep = ";" ,
                                    header = TRUE ,
                                    encoding = "UTF-8",
                                    stringsAsFactors = FALSE
)



colnames(base_telecom_2020_12) <- str_to_upper(colnames(base_telecom_2020_12))
colnames(base_telecom_2021_03) <- str_to_upper(colnames(base_telecom_2021_03))
SurT=table(base_telecom_2020_12$FLAG_RESILIATION)
round(100*prop.table(SurT),digits=0)

data2019=base_telecom_2020_12


# Stratification Non
#quilibrage Non

# recodage des variables et création de la variable AGE
data2019$FLAG_RESILIATION <- as.integer(as.character(data2019$FLAG_RESILIATION))
data2019$DATE_NAISSANCE <- dmy(data2019$DATE_NAISSANCE)
data2019$SEXE <- as.factor(data2019$SEXE)
data2019$CSP <- as.factor(data2019$CSP)
data2019$TAILLE_VILLE <- as.numeric(data2019$TAILLE_VILLE)
data2019$TYPE_VILLE <- as.factor(data2019$TYPE_VILLE)
data2019$REVENU_MOYEN_VILLE <- as.numeric(data2019$REVENU_MOYEN_VILLE)
data2019$DATE_ACTIVATION <- dmy(data2019$DATE_ACTIVATION)
data2019$ENSEIGNE <- as.factor(data2019$ENSEIGNE)
data2019$MODE_PAIEMENT <- as.factor(data2019$MODE_PAIEMENT)
data2019$DUREE_OFFRE_INIT <- as.factor(data2019$DUREE_OFFRE_INIT)
data2019$DUREE_OFFRE <- as.factor(data2019$DUREE_OFFRE)
data2019$NB_MIGRATIONS <- as.factor(data2019$NB_MIGRATIONS)
data2019$FLAG_MIGRATION_HAUSSE <- as.factor(data2019$FLAG_MIGRATION_HAUSSE)
data2019$FLAG_MIGRATION_BAISSE <- as.factor(data2019$FLAG_MIGRATION_BAISSE)
data2019$NB_SERVICES <- as.factor(data2019$NB_SERVICES)
data2019$FLAG_PERSONNALISATION_REPONDEUR <- as.factor(data2019$FLAG_PERSONNALISATION_REPONDEUR)
data2019$FLAG_TELECHARGEMENT_SONNERIE <- as.factor(data2019$FLAG_TELECHARGEMENT_SONNERIE)
data2019$TELEPHONE_INIT <- as.factor(data2019$TELEPHONE_INIT)
data2019$TELEPHONE <- as.factor(data2019$TELEPHONE)
data2019$DATE_FIN_ENGAGEMENT <- dmy(data2019$DATE_FIN_ENGAGEMENT)
data2019$NB_REENGAGEMENTS <- as.factor(data2019$NB_REENGAGEMENTS)
data2019$DATE_DERNIER_REENGAGEMENT <- dmy(data2019$DATE_DERNIER_REENGAGEMENT)
data2019$SITUATION_IMPAYES <- as.factor(data2019$SITUATION_IMPAYES)
data2019$FLAG_APPELS_VERS_INTERNATIONAL <- as.factor(data2019$FLAG_APPELS_VERS_INTERNATIONAL)
data2019$FLAG_APPELS_DEPUIS_INTERNATIONAL <- as.factor(data2019$FLAG_APPELS_DEPUIS_INTERNATIONAL)
data2019$FLAG_APPELS_NUMEROS_SPECIAUX <- as.factor(data2019$FLAG_APPELS_NUMEROS_SPECIAUX)
data2019$SEGMENT <- as.factor(data2019$SEGMENT)
data2019$AGE <- 2020 - year(data2019$DATE_NAISSANCE)

# Stratification Non
#quilibrage Non

typeof(data2019$DATE_FIN_ENGAGEMENT)
##################################################################################
# TRAITEMENT : VALEURS MANQUANTES ET ABERRANTES + SEPARATION EN CLASSES
##################################################################################

# FONCTIONS UTILES ###############################################################

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

histo <- function(VAR, axe_x) {
  pourcentage <- round(as.numeric(prop.table(table(data2019[, VAR]))*100), digits = 2)
  proportion <- (prop.table(table(data2019[, VAR], data2019$FLAG_RESILIATION), margin = 1)*100)[,2]
  resilies <- as.numeric(proportion)
  noms <- names(proportion)
  dataTest <- data.frame(noms, pourcentage, resilies)
  dataTest$noms <- factor(dataTest$noms, levels = dataTest$noms[order(dataTest$resilies, decreasing = TRUE)])
  ggplot(dataTest, aes(x = noms, y = resilies)) +
    geom_bar(stat = "identity", fill = "#008B8B") +
    geom_text(aes(label = pourcentage), color = "#468B00", vjust = -0.7, fontface = "bold") +
    xlab(axe_x) +
    ylab("Taux de résiliés") +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 13, color = "black"),
      axis.text.y = element_text(face = "bold", size = 13, color = "black")
    )
}
#col="#4682B4", border="#B47846"
boxpl <- function(VAR, axe_y) {
  mutate(data2019, outlierVAR = ifelse(is_outlier(data2019[, VAR]), data2019[, VAR], as.numeric(NA))) %>%
    ggplot(., aes(x = as.factor(FLAG_RESILIATION), y = data2019[, VAR])) +
    geom_boxplot(outlier.colour = "#0000FF", outlier.size = 1,color="#FF0000",fill="#CCEEFF") +
    xlab("Résiliation") +
    ylab(axe_y) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 13, color = "black"),
      axis.text.y = element_text(face = "bold", size = 13, color = "black")
    ) +
    scale_x_discrete(labels = c("0" = "Non", "1" = "Oui"))
}

# AGE ############################################################################
summary(data2019$AGE) # 44 NA
for (csp in levels(data2019$CSP)) {
  data2019$AGE[is.na(data2019$DATE_NAISSANCE) & data2019$CSP == csp] <-
    mean(data2019$AGE[data2019$CSP == csp], na.rm = TRUE)
}
rm(csp)

boxpl("AGE", "Age") # valeurs extrémes : > 78 ans
table(data2019$AGE[which(data2019$AGE >= 78)])
round(prop.table(table(data2019$AGE[which(data2019$AGE >= 78)],
                       data2019$FLAG_RESILIATION[which(data2019$AGE >= 78)]),
                 margin = 1)*100, digits = 2)
# on supprime >= 84 ans
data2019 <- data2019[-which(data2019$AGE >= 84),]

smbinning(df = data2019, y = "FLAG_RESILIATION", x = "AGE")$ivtable
# coupures 24, 50, 60 (5.15%), du coup on coupe 24 et 50
data2019$AGE <- cut(data2019$AGE, breaks = c(min(data2019$AGE), 24, 50, max(data2019$AGE)), labels = c("<= 24", "25-50", ">= 51"), include.lowest = TRUE)
histo("AGE", "Age")
# SEXE ###########################################################################
summary(data2019$SEXE) # 7 NA
set.seed(1824)
genre <- sample(c("Masculin", "Féminin"), length(which(data2019$SEXE == "")), replace = TRUE, prob = rep(0.5, 2))
data2019$SEXE[which(data2019$SEXE == "")] <- ifelse(genre == "Masculin", "Masculin", "Féminin")
data2019$SEXE <- droplevels(data2019$SEXE)
rm(genre)
histo("SEXE", "Sexe")


# variable inutile

# CSP ############################################################################
summary(data2019$CSP) # 0 NA
histo("CSP", "CSP")
#tudiants, employés/sans emploi, fonctionnaire/cadre/autre, commeréant/prof libérale
data2019$CSP <- as.character(data2019$CSP)
data2019$CSP[which(data2019$CSP %in% c("Employé", "Sans emploi"))] <- "Employé/Sans emploi"
data2019$CSP[which(data2019$CSP %in% c("Fonctionnaire", "Cadre", "Autre"))] <- "Fonc/Cadre/Autre"
data2019$CSP[which(data2019$CSP %in% c("Commerçant", "Profession libérale"))] <- "Commerçant/Libéral"
data2019$CSP <- as.factor(data2019$CSP)
histo("CSP", "CSP")



# DATE_ACTIVATION et DUREE_ACTIVATION ############################################
summary(data2019$DATE_ACTIVATION) # 0 NA
# nouvelle variable DUREE_ACTIVATION
data2019$DUREE_ACTIVATION <- as.numeric(round((ymd("2020-12-31") - data2019$DATE_ACTIVATION)/30, digits = 1))
boxpl("DUREE_ACTIVATION", "Durée d'activation")
# valeurs extrémes : > 86 mois
table(data2019$DUREE_ACTIVATION[which(data2019$DUREE_ACTIVATION >= 86)])
round(prop.table(table(data2019$DUREE_ACTIVATION[which(data2019$DUREE_ACTIVATION >= 86)],
                       data2019$FLAG_RESILIATION[which(data2019$DUREE_ACTIVATION >= 86)]),
                 margin = 1)*100,digits = 2)
# on supprime rien
smbinning(df = data2019, y = "FLAG_RESILIATION", x = "DUREE_ACTIVATION")$ivtable
# coupures 12, 31.5, 38
data2019$DUREE_ACTIVATION <- cut(data2019$DUREE_ACTIVATION, breaks = c(min(data2019$DUREE_ACTIVATION), 12.1, 31.6, 36.4, max(data2019$DUREE_ACTIVATION)), labels = c("<= 12.1", "12.2-31.6", "31.7-43", ">= 43.1"), include.lowest = TRUE)
histo("DUREE_ACTIVATION", "Durée d'activation")


# ENSEIGNE #######################################################################
summary(data2019$ENSEIGNE) # pas de NA
histo("ENSEIGNE", "Enseigne") # variable inutile

# MODE_PAIEMENT ##################################################################
summary(data2019$MODE_PAIEMENT) # pas de NA
histo("MODE_PAIEMENT", "Mode de paiement")
data2019$MODE_PAIEMENT <- as.character(data2019$MODE_PAIEMENT)
data2019$MODE_PAIEMENT[which(data2019$MODE_PAIEMENT %in% c("Chèque", "TIP"))] <- "Autre"
data2019$MODE_PAIEMENT <- as.factor(data2019$MODE_PAIEMENT)
histo("MODE_PAIEMENT", "Mode de paiement") # pas hyper utile non plus


# DUREE_OFFRE_INIT ###############################################################
summary(data2019$DUREE_OFFRE_INIT) # pas de NA
histo("DUREE_OFFRE_INIT", "Durée de l'offre initiale")
data2019$DUREE_OFFRE_INIT <- as.character(data2019$DUREE_OFFRE_INIT)
data2019$DUREE_OFFRE_INIT[which(data2019$DUREE_OFFRE_INIT %in% c("0.5","1","4","3"))] <- "0.5/1/3/4"
data2019$DUREE_OFFRE_INIT[which(data2019$DUREE_OFFRE_INIT %in% c("8","10","6","2"))] <- "2/6/8/10"
data2019$DUREE_OFFRE_INIT <- as.factor(data2019$DUREE_OFFRE_INIT)
histo("DUREE_OFFRE_INIT", "Durée de l'offre initiale")

# DUREE_OFFRE ####################################################################
summary(data2019$DUREE_OFFRE) # pas de NA
histo("DUREE_OFFRE", "Durée de l'offre")
data2019$DUREE_OFFRE <- as.numeric(as.character(data2019$DUREE_OFFRE))
data2019$DUREE_OFFRE <- cut(data2019$DUREE_OFFRE, breaks = c(min(data2019$DUREE_OFFRE), 2, 4, max(data2019$DUREE_OFFRE)), labels = c("<= 2", "3/4", ">= 5"), include.lowest = TRUE)
histo("DUREE_OFFRE", "Durée de l'offre")

# NB_MIGRATIONS ##################################################################
summary(data2019$NB_MIGRATIONS)
data2019$NB_MIGRATIONS <- as.numeric(as.character(data2019$NB_MIGRATIONS))
boxpl("NB_MIGRATIONS", "Nombre de migrations") # valeurs extrémes : >= 9
data2019 <- data2019[-which(data2019$NB_MIGRATIONS >= 9),]
histo("NB_MIGRATIONS", "Nombre de migrations")
data2019$NB_MIGRATIONS <- cut(data2019$NB_MIGRATIONS, breaks = c(min(data2019$NB_MIGRATIONS), 1, 2, max(data2019$NB_MIGRATIONS)), labels = c("<= 1", "2", ">= 3"), include.lowest = TRUE)
histo("NB_MIGRATIONS", "Nombre de migrations")

# FLAG_MIGRATION_HAUSSE ##########################################################
summary(data2019$FLAG_MIGRATION_HAUSSE)
histo("FLAG_MIGRATION_HAUSSE", "Migration hausse")


# FLAG_MIGRATION_BAISSE ##########################################################
summary(data2019$FLAG_MIGRATION_BAISSE)
histo("FLAG_MIGRATION_BAISSE", "Migration baisse")

# NB_SERVICES ####################################################################
summary(data2019$NB_SERVICES)
data2019$NB_SERVICES <- as.numeric(as.character(data2019$NB_SERVICES))
boxpl("NB_SERVICES", "Nombre de services") # valeurs extrémes : >= 11
data2019 <- data2019[-which(data2019$NB_SERVICES >= 11),]
# histo("NB_SERVICES")
data2019$NB_SERVICES <- cut(data2019$NB_SERVICES, breaks = c(min(data2019$NB_SERVICES), 1, 4, max(data2019$NB_SERVICES)), labels = c("<= 1", "2-4", ">= 5"), include.lowest = TRUE)
histo("NB_SERVICES", "Nombre de services")

# FLAG_PERSONNALISATION_REPONDEUR ################################################
summary(data2019$FLAG_PERSONNALISATION_REPONDEUR)
histo("FLAG_PERSONNALISATION_REPONDEUR", "Personnalisation du répondeur")

# FLAG_TELECHARGEMENT_SONNERIE ###################################################
summary(data2019$FLAG_TELECHARGEMENT_SONNERIE)
histo("FLAG_TELECHARGEMENT_SONNERIE", "Téléchargement d'une sonnerie")

# TELEPHONE_INIT #################################################################
summary(data2019$TELEPHONE_INIT)
histo("TELEPHONE_INIT", "Téléphone initial")

# TELEPHONE ######################################################################
summary(data2019$TELEPHONE)
histo("TELEPHONE", "Téléphone")

# DATE_FIN_ENGAGEMENT et DUREE_ENGAGEMENT_RESTANT ####################################
summary(data2019$DATE_FIN_ENGAGEMENT) # 633 NA
summary(as.factor(data2019$FLAG_RESILIATION[which(is.na(data2019$DATE_FIN_ENGAGEMENT))]))
# les 633 sont 0 réengagement, 296 ont résilié
moy_duree_engagement <- mean(data2019$DATE_FIN_ENGAGEMENT[!is.na(data2019$DATE_FIN_ENGAGEMENT) &
                                                            data2019$NB_REENGAGEMENTS == 0]
                             - data2019$DATE_ACTIVATION[!is.na(data2019$DATE_FIN_ENGAGEMENT) &
                                                          data2019$NB_REENGAGEMENTS == 0])
print(moy_duree_engagement)
for (i in 1:nrow(data2019)) {
  if (is.na(data2019$DATE_FIN_ENGAGEMENT[i])) {
    data2019$DATE_FIN_ENGAGEMENT[i] <- data2019$DATE_ACTIVATION[i] + moy_duree_engagement
  }
}
rm(i, moy_duree_engagement)

# création variable Date de fin d'engagement restant DUREE_ENGAGEMENT_RESTANT
data2019$DUREE_ENGAGEMENT_RESTANT <- 
  as.numeric(round((data2019$DATE_FIN_ENGAGEMENT - ymd("2020-12-31"))/30, digits = 1))
summary(data2019$DUREE_ENGAGEMENT_RESTANT) # cohérent

boxpl("DUREE_ENGAGEMENT_RESTANT", "Durée d'engagement restant") # on supprime rien
smbinning(df = data2019, y = "FLAG_RESILIATION", x = "DUREE_ENGAGEMENT_RESTANT")$ivtable
# coupures -9.1, 0, 9
data2019$DUREE_ENGAGEMENT_RESTANT <- cut(data2019$DUREE_ENGAGEMENT_RESTANT, breaks = c(min(data2019$DUREE_ENGAGEMENT_RESTANT), -9.1, 0, 9, max(data2019$DUREE_ENGAGEMENT_RESTANT)), labels = c("<= -9.1", "-9-0", "0.1-9", ">= 9.1"), include.lowest = TRUE)
histo("DUREE_ENGAGEMENT_RESTANT", "Durée d'engagement restant")


# NB_REENGAGEMENTS et FLAG_REENGAGEMENT ##########################################
summary(data2019$NB_REENGAGEMENTS) # 22376 sont 0
histo("NB_REENGAGEMENTS", "Nombre de réengagements") # nouvelle variable FLAG_REENGAGEMENT
data2019$NB_REENGAGEMENTS <- as.numeric(as.character(data2019$NB_REENGAGEMENTS))
data2019$NB_REENGAGEMENTS <- cut(data2019$NB_REENGAGEMENTS, breaks = c(-1, 0, max(data2019$NB_REENGAGEMENTS)), labels = c("0", "1"), include.lowest = TRUE)
colnames(data2019)[which(colnames(data2019) == "NB_REENGAGEMENTS")] <- "FLAG_REENGAGEMENT"

# DATE_DERNIER_REENGAGEMENT et DUREE_DERNIER_REENGAGEMENT ########################
summary(data2019$DATE_DERNIER_REENGAGEMENT) # 22 376 NA, on n'y touche pas (cf. ci-dessus)
data2019$DUREE_DERNIER_REENGAGEMENT <- as.numeric(round((ymd("2020-12-31") - data2019$DATE_DERNIER_REENGAGEMENT)/30, digits = 1))
smbinning(df = data2019, y = "FLAG_RESILIATION", x = "DUREE_DERNIER_REENGAGEMENT")$ivtable
data2019$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2019$DUREE_DERNIER_REENGAGEMENT) & data2019$DUREE_DERNIER_REENGAGEMENT <= 12.2)] <- "<= 12.2"
data2019$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2019$DUREE_DERNIER_REENGAGEMENT) & data2019$DUREE_DERNIER_REENGAGEMENT != "<= 12")] <- ">= 12.3"
data2019$DUREE_DERNIER_REENGAGEMENT[which(is.na(data2019$DUREE_DERNIER_REENGAGEMENT))] <- "Jamais réengagé"
data2019$DUREE_DERNIER_REENGAGEMENT <- as.factor(data2019$DUREE_DERNIER_REENGAGEMENT)
histo("DUREE_DERNIER_REENGAGEMENT", "Durée depuis le dernier réengagement")

# SITUATION_IMPAYES ##############################################################
summary(data2019$SITUATION_IMPAYES) # pas de NA
histo("SITUATION_IMPAYES", "Situation des impayés")

# APPELS et MOY_APPELS_M6 ########################################################
# pas de NA parmi les 6 variables
# moyenne du nombre d'appels sur les 6 derniers mois MOY_APPELS_M6
data2019$MOY_APPELS_M6 <- rowMeans(cbind(data2019$VOL_APPELS_M1,
                                         data2019$VOL_APPELS_M2,
                                         data2019$VOL_APPELS_M3,
                                         data2019$VOL_APPELS_M4,
                                         data2019$VOL_APPELS_M5,
                                         data2019$VOL_APPELS_M6))
boxpl("MOY_APPELS_M6", "Moyenne du volume d'appels au cours des 6 derniers mois")
smbinning(df = data2019, y = "FLAG_RESILIATION", x = "MOY_APPELS_M6")$ivtable
data2019$MOY_APPELS_M6 <- cut(data2019$MOY_APPELS_M6, breaks = c(min(data2019$MOY_APPELS_M6), 9364.5, 16071.3, max(data2019$MOY_APPELS_M6)), labels = c("<= 9364.5", "9364.6-16071.3", ">= 16071.4"), include.lowest = TRUE)
histo("MOY_APPELS_M6", "Moyenne du volume d'appels au cours des 6 derniers mois")


# FLAG_APPELS_VERS_INTERNATIONAL #################################################
summary(data2019$FLAG_APPELS_VERS_INTERNATIONAL) # no na
histo("FLAG_APPELS_VERS_INTERNATIONAL", "Appels vers l'international")


# FLAG_APPELS_DEPUIS_INTERNATIONAL ###############################################
summary(data2019$FLAG_APPELS_DEPUIS_INTERNATIONAL)#no na
histo("FLAG_APPELS_DEPUIS_INTERNATIONAL", "Appels depuis l'international")
# FLAG_APPELS_NUMEROS_SPECIAUX ###################################################
summary(data2019$FLAG_APPELS_NUMEROS_SPECIAUX) #no na
histo("FLAG_APPELS_NUMEROS_SPECIAUX", "Appel de numéros spéciaux")

# SMS et MOY_NB_SMS_M6 ###########################################################
summary(data2019$NB_SMS_M3) # 435 NA
data2019$NB_SMS_M3[is.na(data2019$NB_SMS_M3)] <- 
  as.integer(round(mean(data2019$NB_SMS_M3, na.rm = TRUE), digits = 0))

# moyenne du nombre de SMS (envoyés) sur les 6 derniers mois
data2019$MOY_NB_SMS_M6 <- rowMeans(cbind(data2019$NB_SMS_M1,
                                         data2019$NB_SMS_M2,
                                         data2019$NB_SMS_M3,
                                         data2019$NB_SMS_M4,
                                         data2019$NB_SMS_M5,
                                         data2019$NB_SMS_M6))
boxpl("MOY_NB_SMS_M6", "Moyenne du nombre de SMS envoyés au cours des 6 derniers mois")
smbinning(df = data2019, y = "FLAG_RESILIATION", x = "MOY_NB_SMS_M6")$ivtable
data2019$MOY_NB_SMS_M6 <- cut(data2019$MOY_NB_SMS_M6, breaks = c(min(data2019$MOY_NB_SMS_M6), 8, max(data2019$MOY_NB_SMS_M6)), labels = c("<= 8", ">= 9"), include.lowest = TRUE)
histo("MOY_NB_SMS_M6", "Moyenne du nombre de SMS envoyés au cours des 6 derniers mois")

# SEGMENT ########################################################################
summary(data2019$SEGMENT)
histo("SEGMENT","segent")
data2019$SEGMENT <- as.character(data2019$SEGMENT)
data2019$SEGMENT[which(data2019$SEGMENT %in% c("B", "C"))] <- "B/C"
data2019$SEGMENT <- as.factor(data2019$SEGMENT)
histo("SEGMENT", "Segment")
##################################################################################
# SELECTION DES COLONNES
##################################################################################

data2019$FLAG_RESILIATION <- as.factor(data2019$FLAG_RESILIATION)

VAR_UTILES <- c(
  "FLAG_RESILIATION",
  #   "SEXE",
  "CSP",
  #"TAILLE_VILLE",
  #   "TYPE_VILLE",
  #"REVENU_MOYEN_VILLE",
  "ENSEIGNE",
  #   "MODE_PAIEMENT",
  "DUREE_OFFRE_INIT",
  "DUREE_OFFRE",
  #   "NB_MIGRATIONS",
  "FLAG_MIGRATION_HAUSSE",
  "FLAG_MIGRATION_BAISSE",
  "NB_SERVICES",
  "FLAG_PERSONNALISATION_REPONDEUR",
  "FLAG_TELECHARGEMENT_SONNERIE",
  #   "TELEPHONE_INIT",
  "TELEPHONE",
  "FLAG_REENGAGEMENT",
  "SITUATION_IMPAYES",
  "FLAG_APPELS_VERS_INTERNATIONAL",
  # "FLAG_APPELS_DEPUIS_INTERNATIONAL",
  # "FLAG_APPELS_NUMEROS_SPECIAUX",
  "SEGMENT",
  "AGE",
  "DUREE_ACTIVATION",
  "DUREE_ENGAGEMENT_RESTANT",
  #   "DUREE_DERNIER_REENGAGEMENT",
  "MOY_NB_SMS_M6",
  "MOY_APPELS_M6"
)
data2019Pred <- data2019[, which(colnames(data2019) %in% VAR_UTILES)]
##################################################################################
# TRAINING / TEST 
##################################################################################

set.seed(4231)
train <- sample(1:nrow(data2019Pred), round(nrow(data2019Pred)*0.8))
ech_train <- data2019Pred[train,]
ech_test <- data2019Pred[-train,]




##################################################################################
# CONSTRUCTION DU MODELE
##################################################################################

modele <- glm(FLAG_RESILIATION ~ ., data = ech_train, family = "binomial")
summary(modele)

# stepAIC(modele, direction = "backward", trace = FALSE)


# Calcul des scores
test_proba_step <- cbind(ech_test, predict(modele, newdata = ech_test, type = "response", se = TRUE))

# Courbe ROC
Pred_step <- prediction(test_proba_step$fit, test_proba_step$FLAG_RESILIATION)
Perf_step <- performance(Pred_step, "tpr", "fpr")
plot(Perf_step, main = "Courbe ROC", col = "red", lwd = 3)
abline(0, 1, lwd = 3)

# AUC
Perf_step2 <- performance(Pred_step, "auc")
Perf_step2@y.values[1]

# Matrice de confusion
ech_test_pred <- cbind(test_proba_step, pred.Y = factor(ifelse(test_proba_step$fit > 0.5, 1, 0)))
m.confusion <- as.matrix(table(prediction = ech_test_pred$pred.Y, truth = ech_test_pred$FLAG_RESILIATION))
print(m.confusion)
print(sum(diag(m.confusion))/sum(m.confusion)*100)


##################################################################################
# APPLICATION DU MODELE
##################################################################################

data2020 <- read.table(
  file = "base_telecom_2021_03.txt",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)

colnames(data2020) <- str_to_upper(colnames(data2020))
colnames(data2020) <- str_to_upper(colnames(data2020))

# recodage des variables et création de la variable AGE
data2020$DATE_NAISSANCE <- dmy(data2020$DATE_NAISSANCE)
data2020$SEXE <- as.factor(data2020$SEXE)
data2020$CSP <- as.factor(data2020$CSP)
data2020$TAILLE_VILLE <- as.numeric(data2020$TAILLE_VILLE)
data2020$TYPE_VILLE <- as.factor(data2020$TYPE_VILLE)
data2020$REVENU_MOYEN_VILLE <- as.numeric(data2020$REVENU_MOYEN_VILLE)
data2020$DATE_ACTIVATION <- dmy(data2020$DATE_ACTIVATION)
data2020$ENSEIGNE <- as.factor(data2020$ENSEIGNE)
data2020$MODE_PAIEMENT <- as.factor(data2020$MODE_PAIEMENT)
data2020$DUREE_OFFRE_INIT <- as.factor(data2020$DUREE_OFFRE_INIT)
data2020$DUREE_OFFRE <- as.factor(data2020$DUREE_OFFRE)
data2020$NB_MIGRATIONS <- as.factor(data2020$NB_MIGRATIONS)
data2020$FLAG_MIGRATION_HAUSSE <- as.factor(data2020$FLAG_MIGRATION_HAUSSE)
data2020$FLAG_MIGRATION_BAISSE <- as.factor(data2020$FLAG_MIGRATION_BAISSE)
data2020$NB_SERVICES <- as.factor(data2020$NB_SERVICES)
data2020$FLAG_PERSONNALISATION_REPONDEUR <- as.factor(data2020$FLAG_PERSONNALISATION_REPONDEUR)
data2020$FLAG_TELECHARGEMENT_SONNERIE <- as.factor(data2020$FLAG_TELECHARGEMENT_SONNERIE)
data2020$TELEPHONE_INIT <- as.factor(data2020$TELEPHONE_INIT)
data2020$TELEPHONE <- as.factor(data2020$TELEPHONE)
data2020$DATE_FIN_ENGAGEMENT <- dmy(data2020$DATE_FIN_ENGAGEMENT)
data2020$NB_REENGAGEMENTS <- as.factor(data2020$NB_REENGAGEMENTS)
data2020$DATE_DERNIER_REENGAGEMENT <- dmy(data2020$DATE_DERNIER_REENGAGEMENT)
data2020$SITUATION_IMPAYES <- as.factor(data2020$SITUATION_IMPAYES)
data2020$FLAG_APPELS_VERS_INTERNATIONAL <- as.factor(data2020$FLAG_APPELS_VERS_INTERNATIONAL)
data2020$FLAG_APPELS_DEPUIS_INTERNATIONAL <- as.factor(data2020$FLAG_APPELS_DEPUIS_INTERNATIONAL)
data2020$FLAG_APPELS_NUMEROS_SPECIAUX <- as.factor(data2020$FLAG_APPELS_NUMEROS_SPECIAUX)
data2020$SEGMENT <- as.factor(data2020$SEGMENT)
data2020$AGE <- 2021 - year(data2020$DATE_NAISSANCE)




##################################################################################
# TRAITEMENT DES VALEURS MANQUANTES ET ABERRANTES
##################################################################################




# AGE ############################################################################
for (csp in levels(data2020$CSP)) {
  data2020$AGE[is.na(data2020$DATE_NAISSANCE) & data2020$CSP == csp] <- 
    mean(data2020$AGE[data2020$CSP == csp], na.rm = TRUE)
}
rm(csp)
data2020 <- data2020[-which(data2020$AGE >= 84),]
data2020$AGE <- cut(data2020$AGE, breaks = c(min(data2020$AGE), 24, 50, max(data2020$AGE)), labels = c("<= 24", "25-50", ">= 51"), include.lowest = TRUE)

# SEXE ###########################################################################
set.seed(1824)
genre <- sample(c("Masculin", "Féminin"), length(which(data2020$SEXE == "")), replace = TRUE, prob = rep(0.5, 2))
data2020$SEXE[which(data2020$SEXE == "")] <- ifelse(genre == "Masculin", "Masculin", "Féminin")
data2020$SEXE <- droplevels(data2020$SEXE)
rm(genre)

# CSP ############################################################################
data2020$CSP <- as.character(data2020$CSP)
data2020$CSP[which(data2020$CSP %in% c("Employé", "Sans emploi"))] <- "Employé/Sans emploi"
data2020$CSP[which(data2020$CSP %in% c("Fonctionnaire", "Cadre", "Autre"))] <- "Fonc/Cadre/Autre"
data2020$CSP[which(data2020$CSP %in% c("Commerçant", "Profession libérale"))] <- "Commerçant/Libéral"
data2020$CSP <- as.factor(data2020$CSP)

#base_de_donnees_03_2021.txt

# MODE_PAIEMENT ##################################################################
data2020$MODE_PAIEMENT <- as.character(data2020$MODE_PAIEMENT)
data2020$MODE_PAIEMENT[which(data2020$MODE_PAIEMENT %in% c("Chèque", "TIP"))] <- "Autre"
data2020$MODE_PAIEMENT <- as.factor(data2020$MODE_PAIEMENT)

# DUREE_OFFRE_INIT ###############################################################
data2020$DUREE_OFFRE_INIT <- as.character(data2020$DUREE_OFFRE_INIT)
data2020$DUREE_OFFRE_INIT[which(data2020$DUREE_OFFRE_INIT %in% c("0.5","1","4","3"))] <- "0.5/1/3/4"
data2020$DUREE_OFFRE_INIT[which(data2020$DUREE_OFFRE_INIT %in% c("8","10","6","2"))] <- "2/6/8/10"
data2020$DUREE_OFFRE_INIT <- as.factor(data2020$DUREE_OFFRE_INIT)
# DATE_ACTIVATION et DUREE_ACTIVATION ############################################
data2020$DUREE_ACTIVATION <- as.numeric(round((ymd("2021-12-31") - data2020$DATE_ACTIVATION)/30, digits = 1))
data2020$DUREE_ACTIVATION <- cut(data2020$DUREE_ACTIVATION, breaks = c(min(data2020$DUREE_ACTIVATION), 12.1, 31.6, 36.4, max(data2020$DUREE_ACTIVATION)), labels = c("<= 12.1", "12.2-31.6", "31.7-43", ">= 43.1"), include.lowest = TRUE)
# DUREE_OFFRE ####################################################################
data2020$DUREE_OFFRE <- as.numeric(as.character(data2020$DUREE_OFFRE))
data2020$DUREE_OFFRE <- cut(data2020$DUREE_OFFRE, breaks = c(min(data2020$DUREE_OFFRE), 2, 4, max(data2020$DUREE_OFFRE)), labels = c("<= 2", "3/4", ">= 5"), include.lowest = TRUE)

# NB_MIGRATIONS ##################################################################
data2020$NB_MIGRATIONS <- as.numeric(as.character(data2020$NB_MIGRATIONS))
data2020 <- data2020[-which(data2020$NB_MIGRATIONS >= 9),]
data2020$NB_MIGRATIONS <- cut(data2020$NB_MIGRATIONS, breaks = c(min(data2020$NB_MIGRATIONS), 1, 2, max(data2020$NB_MIGRATIONS)), labels = c("<= 1", "2", ">= 3"), include.lowest = TRUE)

# NB_SERVICES ####################################################################
data2020$NB_SERVICES <- as.numeric(as.character(data2020$NB_SERVICES))
data2020 <- data2020[-which(data2020$NB_SERVICES >= 11),]
data2020$NB_SERVICES <- cut(data2020$NB_SERVICES, breaks = c(min(data2020$NB_SERVICES), 1, 4, max(data2020$NB_SERVICES)), labels = c("<= 1", "2-4", ">= 5"), include.lowest = TRUE)

# DATE_FIN_ENGAGEMENT et DUREE_ENGAGEMENT_RESTANT ####################################
moy_duree_engagement <- mean(data2020$DATE_FIN_ENGAGEMENT[!is.na(data2020$DATE_FIN_ENGAGEMENT) &
                                                            data2020$NB_REENGAGEMENTS == 0]
                             - data2020$DATE_ACTIVATION[!is.na(data2020$DATE_FIN_ENGAGEMENT) &
                                                          data2020$NB_REENGAGEMENTS == 0])
for (i in 1:nrow(data2020)) {
  if (is.na(data2020$DATE_FIN_ENGAGEMENT[i])) {
    data2020$DATE_FIN_ENGAGEMENT[i] <- data2020$DATE_ACTIVATION[i] + moy_duree_engagement
  }
}
rm(i, moy_duree_engagement)

data2020$DUREE_ENGAGEMENT_RESTANT <- 
  as.numeric(round((data2020$DATE_FIN_ENGAGEMENT - ymd("2021-03-31"))/30, digits = 1))

data2020$DUREE_ENGAGEMENT_RESTANT <- cut(data2020$DUREE_ENGAGEMENT_RESTANT, breaks = c(min(data2020$DUREE_ENGAGEMENT_RESTANT), -9.1, 0, 9, max(data2020$DUREE_ENGAGEMENT_RESTANT)), labels = c("<= -9.1", "-9-0", "0.1-9", ">= 9.1"), include.lowest = TRUE)


# NB_REENGAGEMENTS et FLAG_REENGAGEMENT ##########################################
data2020$NB_REENGAGEMENTS <- as.numeric(as.character(data2020$NB_REENGAGEMENTS))
data2020$NB_REENGAGEMENTS <- cut(data2020$NB_REENGAGEMENTS, breaks = c(-1, 0, max(data2020$NB_REENGAGEMENTS)), labels = c("0", "1"), include.lowest = TRUE)
colnames(data2020)[which(colnames(data2020) == "NB_REENGAGEMENTS")] <- "FLAG_REENGAGEMENT"


# DATE_DERNIER_REENGAGEMENT et DUREE_DERNIER_REENGAGEMENT ########################
data2020$DUREE_DERNIER_REENGAGEMENT <- as.numeric(round((ymd("2021-03-31") - data2020$DATE_DERNIER_REENGAGEMENT)/30, digits = 1))
data2020$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2020$DUREE_DERNIER_REENGAGEMENT) & data2020$DUREE_DERNIER_REENGAGEMENT <= 12)] <- "<= 12"
data2020$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2020$DUREE_DERNIER_REENGAGEMENT) & data2020$DUREE_DERNIER_REENGAGEMENT != "<= 12")] <- ">= 12.1"
data2020$DUREE_DERNIER_REENGAGEMENT[which(is.na(data2020$DUREE_DERNIER_REENGAGEMENT))] <- "Jamais réengagé"
data2020$DUREE_DERNIER_REENGAGEMENT <- as.factor(data2020$DUREE_DERNIER_REENGAGEMENT)



# APPELS et MOY_APPELS_M6 ########################################################
data2020$MOY_APPELS_M6 <- rowMeans(cbind(data2020$VOL_APPELS_M1,
                                         data2020$VOL_APPELS_M2,
                                         data2020$VOL_APPELS_M3,
                                         data2020$VOL_APPELS_M4,
                                         data2020$VOL_APPELS_M5,
                                         data2020$VOL_APPELS_M6))
data2020$MOY_APPELS_M6 <- cut(data2020$MOY_APPELS_M6, breaks = c(min(data2020$MOY_APPELS_M6), 9364.5, 16071.3, max(data2020$MOY_APPELS_M6)), labels = c("<= 9364.5", "9364.6-16071.3", ">= 16071.4"), include.lowest = TRUE)


# SMS et MOY_NB_SMS_M6 ###########################################################
data2020$NB_SMS_M3[is.na(data2020$NB_SMS_M3)] <- 
  as.integer(round(mean(data2020$NB_SMS_M3, na.rm = TRUE), digits = 0))

data2020$MOY_NB_SMS_M6 <- rowMeans(cbind(data2020$NB_SMS_M1,
                                         data2020$NB_SMS_M2,
                                         data2020$NB_SMS_M3,
                                         data2020$NB_SMS_M4,
                                         data2020$NB_SMS_M5,
                                         data2020$NB_SMS_M6))
data2020$MOY_NB_SMS_M6 <- cut(data2020$MOY_NB_SMS_M6, breaks = c(min(data2020$MOY_NB_SMS_M6), 8, max(data2020$MOY_NB_SMS_M6)), labels = c("<= 8", ">= 9"), include.lowest = TRUE)

# SEGMENT ########################################################################
data2020$SEGMENT <- as.character(data2020$SEGMENT)
data2020$SEGMENT[which(data2020$SEGMENT %in% c("B", "C"))] <- "B/C"
data2020$SEGMENT <- as.factor(data2020$SEGMENT)


VAR <- c(
  "ID_CLIENT",
  "AGE",
  "CSP",
 # "TAILLE_VILLE",
 # "REVENU_MOYEN_VILLE",
  "DUREE_ACTIVATION",
  "ENSEIGNE",
  "DUREE_OFFRE_INIT",
  "DUREE_OFFRE",
  "FLAG_MIGRATION_HAUSSE",
  "FLAG_MIGRATION_BAISSE",
  "NB_SERVICES",
  "FLAG_PERSONNALISATION_REPONDEUR",
  "FLAG_TELECHARGEMENT_SONNERIE",
  "TELEPHONE",
  "DUREE_ENGAGEMENT_RESTANT",
  "FLAG_REENGAGEMENT",
  "SITUATION_IMPAYES",
  "MOY_APPELS_M6",
  "FLAG_APPELS_VERS_INTERNATIONAL",
  "MOY_NB_SMS_M6",
  "SEGMENT"
)

data2020Pred <- data2020[, which(colnames(data2020) %in% VAR)]

ech_pred <- predict(modele, type = "response", newdata = data2020Pred[, -which(colnames(data2020Pred) == "ID_CLIENT")])
head(ech_pred)

predictions <- cbind(data2020$ID_CLIENT, ech_pred)
predictions <- as.data.frame(predictions)

base_C4 <- predictions[order(predictions$ech_pred, decreasing = TRUE),]
final <- base_C4[1:2000, "V1"]

# write.table(
#   x = final,
#   file = "C4_Mansouri.txt",
#   row.names = FALSE,
#   col.names = FALSE,
#   quote = FALSE
# )


##################################################################################
# Ciblage score V2 ###############################################################
##################################################################################

rm(list=ls())


##################################################################################
# IMPORTS ET PRE-TRAITEMENT DES BASES
##################################################################################


data2020v2 <- read.table(
  file = "base_de_donnees_03_2021.txt",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)



colnames(data2020v2) <- str_to_upper(colnames(data2020v2))

# recodage des variables
data2020v2$DATE_NAISSANCE <- dmy(data2020v2$DATE_NAISSANCE)
data2020v2$SEXE <- as.factor(data2020v2$SEXE)
data2020v2$CSP <- as.factor(data2020v2$CSP)
data2020v2$TAILLE_VILLE <- as.numeric(data2020v2$TAILLE_VILLE)
data2020v2$TYPE_VILLE <- as.factor(data2020v2$TYPE_VILLE)
data2020v2$REVENU_MOYEN_VILLE <- as.numeric(data2020v2$REVENU_MOYEN_VILLE)
data2020v2$DATE_ACTIVATION <- dmy(data2020v2$DATE_ACTIVATION)
data2020v2$ENSEIGNE <- as.factor(data2020v2$ENSEIGNE)
data2020v2$MODE_PAIEMENT <- as.factor(data2020v2$MODE_PAIEMENT)
data2020v2$DUREE_OFFRE_INIT <- as.factor(data2020v2$DUREE_OFFRE_INIT)
data2020v2$DUREE_OFFRE <- as.factor(data2020v2$DUREE_OFFRE)
data2020v2$NB_MIGRATIONS <- as.numeric(data2020v2$NB_MIGRATIONS)
data2020v2$FLAG_MIGRATION_HAUSSE <- as.factor(data2020v2$FLAG_MIGRATION_HAUSSE)
data2020v2$FLAG_MIGRATION_BAISSE <- as.factor(data2020v2$FLAG_MIGRATION_BAISSE)
data2020v2$NB_SERVICES <- as.factor(data2020v2$NB_SERVICES)
data2020v2$FLAG_PERSONNALISATION_REPONDEUR <- as.factor(data2020v2$FLAG_PERSONNALISATION_REPONDEUR)
data2020v2$FLAG_TELECHARGEMENT_SONNERIE <- as.factor(data2020v2$FLAG_TELECHARGEMENT_SONNERIE)
data2020v2$TELEPHONE_INIT <- as.factor(data2020v2$TELEPHONE_INIT)
data2020v2$TELEPHONE <- as.factor(data2020v2$TELEPHONE)
data2020v2$DATE_FIN_ENGAGEMENT <- dmy(data2020v2$DATE_FIN_ENGAGEMENT)
data2020v2$NB_REENGAGEMENTS <- as.factor(data2020v2$NB_REENGAGEMENTS)
data2020v2$DATE_DERNIER_REENGAGEMENT <- dmy(data2020v2$DATE_DERNIER_REENGAGEMENT)
data2020v2$SITUATION_IMPAYES <- as.factor(data2020v2$SITUATION_IMPAYES)
data2020v2$FLAG_APPELS_VERS_INTERNATIONAL <- as.factor(data2020v2$FLAG_APPELS_VERS_INTERNATIONAL)
data2020v2$FLAG_APPELS_DEPUIS_INTERNATIONAL <- as.factor(data2020v2$FLAG_APPELS_DEPUIS_INTERNATIONAL)
data2020v2$FLAG_APPELS_NUMEROS_SPECIAUX <- as.factor(data2020v2$FLAG_APPELS_NUMEROS_SPECIAUX)
data2020v2$SEGMENT <- as.factor(data2020v2$SEGMENT)

data2019v2 <- read.table(
  file = "bd_12_2020.txt",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)

colnames(data2019v2) <- str_to_upper(colnames(data2019v2))

# recodage des variables
data2019v2$FLAG_RESILIATION <- as.factor(data2019v2$FLAG_RESILIATION)
data2019v2$DATE_NAISSANCE <- dmy(data2019v2$DATE_NAISSANCE)
data2019v2$SEXE <- as.factor(data2019v2$SEXE)
data2019v2$CSP <- as.factor(data2019v2$CSP)
data2019v2$TAILLE_VILLE <- as.numeric(data2019v2$TAILLE_VILLE)
data2019v2$TYPE_VILLE <- as.factor(data2019v2$TYPE_VILLE)
data2019v2$REVENU_MOYEN_VILLE <- as.numeric(data2019v2$REVENU_MOYEN_VILLE)
data2019v2$DATE_ACTIVATION <- dmy(data2019v2$DATE_ACTIVATION)
data2019v2$ENSEIGNE <- as.factor(data2019v2$ENSEIGNE)
data2019v2$MODE_PAIEMENT <- as.factor(data2019v2$MODE_PAIEMENT)
data2019v2$DUREE_OFFRE_INIT <- as.factor(data2019v2$DUREE_OFFRE_INIT)
data2019v2$DUREE_OFFRE <- as.factor(data2019v2$DUREE_OFFRE)
data2019v2$NB_MIGRATIONS <- as.numeric(data2019v2$NB_MIGRATIONS)
data2019v2$FLAG_MIGRATION_HAUSSE <- as.factor(data2019v2$FLAG_MIGRATION_HAUSSE)
data2019v2$FLAG_MIGRATION_BAISSE <- as.factor(data2019v2$FLAG_MIGRATION_BAISSE)
data2019v2$NB_SERVICES <- as.factor(data2019v2$NB_SERVICES)
data2019v2$FLAG_PERSONNALISATION_REPONDEUR <- as.factor(data2019v2$FLAG_PERSONNALISATION_REPONDEUR)
data2019v2$FLAG_TELECHARGEMENT_SONNERIE <- as.factor(data2019v2$FLAG_TELECHARGEMENT_SONNERIE)
data2019v2$TELEPHONE_INIT <- as.factor(data2019v2$TELEPHONE_INIT)
data2019v2$TELEPHONE <- as.factor(data2019v2$TELEPHONE)
data2019v2$DATE_FIN_ENGAGEMENT <- dmy(data2019v2$DATE_FIN_ENGAGEMENT)
data2019v2$NB_REENGAGEMENTS <- as.factor(data2019v2$NB_REENGAGEMENTS)
data2019v2$DATE_DERNIER_REENGAGEMENT <- dmy(data2019v2$DATE_DERNIER_REENGAGEMENT)
data2019v2$SITUATION_IMPAYES <- as.factor(data2019v2$SITUATION_IMPAYES)
data2019v2$FLAG_APPELS_VERS_INTERNATIONAL <- as.factor(data2019v2$FLAG_APPELS_VERS_INTERNATIONAL)
data2019v2$FLAG_APPELS_DEPUIS_INTERNATIONAL <- as.factor(data2019v2$FLAG_APPELS_DEPUIS_INTERNATIONAL)
data2019v2$FLAG_APPELS_NUMEROS_SPECIAUX <- as.factor(data2019v2$FLAG_APPELS_NUMEROS_SPECIAUX)
data2019v2$SEGMENT <- as.factor(data2019v2$SEGMENT)



# Stratification Non
#quilibrage Non

##################################################################################
# TRAITEMENT : VALEURS MANQUANTES ET ABERRANTES + SEPARATION EN CLASSES
##################################################################################

# FONCTIONS UTILES ###############################################################

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

histo <- function(VAR, axe_x) {
  pourcentage <- round(as.numeric(prop.table(table(data2019[, VAR]))*100), digits = 2)
  proportion <- (prop.table(table(data2019[, VAR], data2019$FLAG_RESILIATION), margin = 1)*100)[,2]
  resilies <- as.numeric(proportion)
  noms <- names(proportion)
  dataTest <- data.frame(noms, pourcentage, resilies)
  dataTest$noms <- factor(dataTest$noms, levels = dataTest$noms[order(dataTest$resilies, decreasing = TRUE)])
  ggplot(dataTest, aes(x = noms, y = resilies)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = pourcentage), color = "red", vjust = -0.7, fontface = "bold") +
    xlab(axe_x) +
    ylab("Taux de résiliés") +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 13, color = "black"),
      axis.text.y = element_text(face = "bold", size = 13, color = "black")
    )
}

boxpl <- function(VAR, axe_y) {
  mutate(data2019, outlierVAR = ifelse(is_outlier(data2019[, VAR]), data2019[, VAR], as.numeric(NA))) %>%
    ggplot(., aes(x = as.factor(FLAG_RESILIATION), y = data2019[, VAR])) +
    geom_boxplot(outlier.colour = "red", outlier.size = 1) +
    xlab("Le client a-t-il résilié") +
    ylab(axe_y) +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 13, color = "black"),
      axis.text.y = element_text(face = "bold", size = 13, color = "black")
    ) +
    scale_x_discrete(labels = c("0" = "Non", "1" = "Oui"))
}

###### age ###### "31/12/2020"
# sapply(data2019v2$DATE_NAISSANCE, function(x) {
#return(time_length(interval(x, ymd("2020-12-31")), "years"))
#})


View(head(data2019v2,5))

data2019v2$AGE <- floor(time_length(interval(ymd(data2019v2$DATE_NAISSANCE), ymd("2020-12-31")),"years"))

write.table(x = data2019v2$AGE, file = "data2019v2AGE.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

data2019v2$AGE <- as.numeric(as.matrix(read.table("data2019v2AGE.txt")))

for (csp in levels(data2019v2$CSP)) {
  data2019v2$AGE[is.na(data2019v2$DATE_NAISSANCE) & data2019v2$CSP == csp] <-
    mean(data2019v2$AGE[data2019v2$CSP == csp], na.rm = TRUE)
}
rm(csp)
data2019v2 <- data2019v2[-which(data2019v2$AGE >= 84),]

data2019v2$AGE <- cut(data2019v2$AGE, breaks = c(min(data2019v2$AGE), 24, 50, max(data2019v2$AGE)), labels = c("<= 24", "25-50", ">= 51"), include.lowest = TRUE)

# SEXE inutile ###################################################################
# IDEM

set.seed(237)
genre <- sample(c("Masculin", "Féminin"), length(which(data2019v2$SEXE == "")), replace = TRUE, prob = rep(0.5, 2))
data2019v2$SEXE[which(data2019v2$SEXE == "")] <- ifelse(genre == "Masculin", "Masculin", "Féminin")
data2019v2$SEXE <- droplevels(data2019v2$SEXE)
rm(genre)

# CSP ############################################################################
# IDEM

data2019v2$CSP <- as.character(data2019v2$CSP)

data2019v2$CSP[which(data2019v2$CSP %in% c("Employé", "Sans emploi"))] <- "Employé/Sans emploi"
data2019v2$CSP[which(data2019v2$CSP %in% c("Fonctionnaire", "Cadre", "Autre"))] <- "Fonc/Cadre/Autre"
data2019v2$CSP[which(data2019v2$CSP %in% c("Commerçant", "Profession libérale"))] <- "Commerçant/Libéral"
data2019v2$CSP <- as.factor(data2019v2$CSP)



# DATE_ACTIVATION et DUREE_ACTIVATION ############################################
# IDEM

data2019v2$DUREE_ACTIVATION <- as.numeric(round((ymd("2020-12-31") - data2019v2$DATE_ACTIVATION)/30, digits = 1))
data2019v2$DUREE_ACTIVATION <- cut(data2019v2$DUREE_ACTIVATION, breaks = c(min(data2019v2$DUREE_ACTIVATION), 12, 31.5, 38, max(data2019v2$DUREE_ACTIVATION)), labels = c("<= 12", "12.1-31.5", "31.6-38", ">= 38.1"), include.lowest = TRUE)

# ENSEIGNE inutile ###############################################################
# histo("ENSEIGNE")

# MODE_PAIEMENT inutile ##########################################################
# IDEM

data2019v2$MODE_PAIEMENT <- as.character(data2019v2$MODE_PAIEMENT)
data2019v2$MODE_PAIEMENT[which(data2019v2$MODE_PAIEMENT %in% c("Chèque", "TIP"))] <- "Autre"
data2019v2$MODE_PAIEMENT <- as.factor(data2019v2$MODE_PAIEMENT)

# DUREE_OFFRE_INIT ###############################################################
# IDEM

data2019v2$DUREE_OFFRE_INIT <- as.character(data2019v2$DUREE_OFFRE_INIT)
data2019v2$DUREE_OFFRE_INIT[which(data2019v2$DUREE_OFFRE_INIT %in% c("0.5","1","4","3"))] <- "0.5/1/3/4"
data2019v2$DUREE_OFFRE_INIT[which(data2019v2$DUREE_OFFRE_INIT %in% c("8","10","6","2"))] <- "2/6/8/10"
data2019v2$DUREE_OFFRE_INIT <- as.factor(data2019v2$DUREE_OFFRE_INIT)



data2019v2$DUREE_OFFRE <- as.numeric(as.character(data2019v2$DUREE_OFFRE))
data2019v2$DUREE_OFFRE <- cut(data2019v2$DUREE_OFFRE, breaks = c(min(data2019v2$DUREE_OFFRE), 2, 4, max(data2019v2$DUREE_OFFRE)), labels = c("<= 2", "3/4", ">= 5"), include.lowest = TRUE)


# NB_MIGRATIONS ##################################################################
# IDEM

data2019v2 <- data2019v2[-which(data2019v2$NB_MIGRATIONS >= 9),]
data2019v2$NB_MIGRATIONS <- cut(data2019v2$NB_MIGRATIONS, breaks = c(min(data2019v2$NB_MIGRATIONS), 1, 2, max(data2019v2$NB_MIGRATIONS)), labels = c("<= 1", "2", ">= 3"), include.lowest = TRUE)
# NB_SERVICES ####################################################################


data2019v2$NB_SERVICES <- as.numeric(as.character(data2019v2$NB_SERVICES))
data2019v2 <- data2019v2[-which(data2019v2$NB_SERVICES >= 11),]
data2019v2$NB_SERVICES <- cut(data2019v2$NB_SERVICES, breaks = c(min(data2019v2$NB_SERVICES), 1, 4, max(data2019v2$NB_SERVICES)), labels = c("<= 1", "2-4", ">= 5"), include.lowest = TRUE)


moy_duree_engagement <- mean(data2019v2$DATE_FIN_ENGAGEMENT[!is.na(data2019v2$DATE_FIN_ENGAGEMENT) & data2019v2$NB_REENGAGEMENTS == 0]
                             - data2019v2$DATE_ACTIVATION[!is.na(data2019v2$DATE_FIN_ENGAGEMENT) & data2019v2$NB_REENGAGEMENTS == 0])
for (i in 1:nrow(data2019v2)) {
  if (is.na(data2019v2$DATE_FIN_ENGAGEMENT[i])) {
    data2019v2$DATE_FIN_ENGAGEMENT[i] <- data2019v2$DATE_ACTIVATION[i] + moy_duree_engagement
  }
}
rm(i, moy_duree_engagement)


# création variable Date de fin d'engagement restant DUREE_ENGAGEMENT_RESTANT
data2019v2$DUREE_ENGAGEMENT_RESTANT <- as.numeric(round((data2019v2$DATE_FIN_ENGAGEMENT - ymd("2020-12-31"))/30, digits = 1))

data2019v2$DUREE_ENGAGEMENT_RESTANT <- cut(data2019v2$DUREE_ENGAGEMENT_RESTANT, breaks = c(min(data2019v2$DUREE_ENGAGEMENT_RESTANT), -9, 0, 9, max(data2019v2$DUREE_ENGAGEMENT_RESTANT)), labels = c("<= -9", "-8.9-0", "0.1-9", ">= 9.1"), include.lowest = TRUE)


# NB_REENGAGEMENTS devient FLAG_REENGAGEMENT #####################################
# IDEM

data2019v2$NB_REENGAGEMENTS <- as.numeric(as.character(data2019v2$NB_REENGAGEMENTS))
data2019v2$NB_REENGAGEMENTS <- cut(data2019v2$NB_REENGAGEMENTS, breaks = c(-1, 0, max(data2019v2$NB_REENGAGEMENTS)), labels = c("0", "1"), include.lowest = TRUE)
colnames(data2019v2)[which(colnames(data2019v2) == "NB_REENGAGEMENTS")] <- "FLAG_REENGAGEMENT"

# DATE_DERNIER_REENGAGEMENT et DUREE_DERNIER_REENGAGEMENT ########################
# PRESQUE IDEM (diff entre 11.9 et 12)

data2019v2$DUREE_DERNIER_REENGAGEMENT <- as.numeric(round((ymd("2020-12-31") - data2019v2$DATE_DERNIER_REENGAGEMENT)/30, digits = 1))
data2019v2$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2019v2$DUREE_DERNIER_REENGAGEMENT) & data2019v2$DUREE_DERNIER_REENGAGEMENT < 12)] <- "< 12"
data2019v2$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2019v2$DUREE_DERNIER_REENGAGEMENT) & data2019v2$DUREE_DERNIER_REENGAGEMENT != "< 12")] <- ">= 12"
data2019v2$DUREE_DERNIER_REENGAGEMENT[which(is.na(data2019v2$DUREE_DERNIER_REENGAGEMENT))] <- "Jamais réengagé"
data2019v2$DUREE_DERNIER_REENGAGEMENT <- as.factor(data2019v2$DUREE_DERNIER_REENGAGEMENT)

# SITUATION_IMPAYES ##############################################################
# histo("SITUATION_IMPAYES")

# APPELS et MOY_APPELS_M6 ########################################################
# IDEM

data2019v2$MOY_APPELS_M6 <- rowMeans(cbind(data2019v2$VOL_APPELS_M1,
                                           data2019v2$VOL_APPELS_M2,
                                           data2019v2$VOL_APPELS_M3,
                                           data2019v2$VOL_APPELS_M4,
                                           data2019v2$VOL_APPELS_M5,
                                           data2019v2$VOL_APPELS_M6))
data2019v2$MOY_APPELS_M6 <- cut(data2019v2$MOY_APPELS_M6, breaks = c(min(data2019v2$MOY_APPELS_M6), 9150, 17000, max(data2019v2$MOY_APPELS_M6)), labels = c("<= 9150", "9151-17000", ">= 17001"), include.lowest = TRUE)

# FLAG_APPELS_VERS_INTERNATIONAL #################################################
# histo("FLAG_APPELS_VERS_INTERNATIONAL")

# FLAG_APPELS_DEPUIS_INTERNATIONAL ###############################################
# histo("FLAG_APPELS_DEPUIS_INTERNATIONAL")

# FLAG_APPELS_NUMEROS_SPECIAUX ###################################################
# histo("FLAG_APPELS_NUMEROS_SPECIAUX")

# SMS et MOY_NB_SMS_M6 ###########################################################
# IDEM


# SMS et MOY_NB_SMS_M6 ###########################################################
# IDEM

data2019v2$NB_SMS_M3[is.na(data2019v2$NB_SMS_M3)] <- as.integer(round(mean(data2019v2$NB_SMS_M3, na.rm = TRUE), digits = 0))
data2019v2$MOY_NB_SMS_M6 <- rowMeans(cbind(data2019v2$NB_SMS_M1,
                                           data2019v2$NB_SMS_M2,
                                           data2019v2$NB_SMS_M3,
                                           data2019v2$NB_SMS_M4,
                                           data2019v2$NB_SMS_M5,
                                           data2019v2$NB_SMS_M6))
data2019v2$MOY_NB_SMS_M6 <- cut(data2019v2$MOY_NB_SMS_M6, breaks = c(min(data2019v2$MOY_NB_SMS_M6), 8, max(data2019v2$MOY_NB_SMS_M6)), labels = c("<= 8", ">= 9"), include.lowest = TRUE)

# SEGMENT ########################################################################
# IDEM

data2019v2$SEGMENT <- as.character(data2019v2$SEGMENT)
data2019v2$SEGMENT[which(data2019v2$SEGMENT %in% c("B", "C"))] <- "B/C"
data2019v2$SEGMENT <- as.factor(data2019v2$SEGMENT)


##################################################################################
# SELECTION DES VARIABLES
##################################################################################

# on enléve ID_CLIENT et CODE_POSTAL
# suppression A : SEXE, ENSEIGNE, MODE_PAIEMENT

VAR_UTILES <- c(
  "FLAG_RESILIATION",
  # "SEXE",
  "CSP",
  # "TAILLE_VILLE",
  # "TYPE_VILLE",
  #"REVENU_MOYEN_VILLE",
  "ENSEIGNE",
  # "MODE_PAIEMENT",
  # "DUREE_OFFRE_INIT",
  "DUREE_OFFRE",
  # "NB_MIGRATIONS",
  # "FLAG_MIGRATION_HAUSSE",
  "FLAG_MIGRATION_BAISSE",
  "NB_SERVICES",
  # "FLAG_PERSONNALISATION_REPONDEUR",
  # "FLAG_TELECHARGEMENT_SONNERIE",
  "TELEPHONE_INIT",
  "TELEPHONE",
  "FLAG_REENGAGEMENT",
  "SITUATION_IMPAYES",
  # "FLAG_APPELS_VERS_INTERNATIONAL",
  # "FLAG_APPELS_DEPUIS_INTERNATIONAL",
  "FLAG_APPELS_NUMEROS_SPECIAUX",
  "SEGMENT",
  "AGE",
  "DUREE_ACTIVATION",
  "DUREE_ENGAGEMENT_RESTANT",
  "DUREE_DERNIER_REENGAGEMENT",
  # "MOY_NB_SMS_M6",
  "MOY_APPELS_M6"
)

# VAR_UTILES <- c(
#   "FLAG_RESILIATION",
#   "SEXE",
#   "CSP",
#   "TAILLE_VILLE",
#   "TYPE_VILLE",
#   "REVENU_MOYEN_VILLE",
#   "ENSEIGNE",
#   "MODE_PAIEMENT",
#   "DUREE_OFFRE_INIT",
#   "DUREE_OFFRE",
#   "NB_MIGRATIONS",
#   "FLAG_MIGRATION_HAUSSE",
#   "FLAG_MIGRATION_BAISSE",
#   "NB_SERVICES",
#   "FLAG_PERSONNALISATION_REPONDEUR",
#   "FLAG_TELECHARGEMENT_SONNERIE",
#   "TELEPHONE_INIT",
#   "TELEPHONE",
#   "FLAG_REENGAGEMENT",
#   "SITUATION_IMPAYES",
#   "FLAG_APPELS_VERS_INTERNATIONAL",
#   "FLAG_APPELS_DEPUIS_INTERNATIONAL",
#   "FLAG_APPELS_NUMEROS_SPECIAUX",
#   "SEGMENT",
#   "AGE",
#   "DUREE_ACTIVATION",
#   "DUREE_ENGAGEMENT_RESTANT",
#   "DUREE_DERNIER_REENGAGEMENT",
#   "MOY_NB_SMS_M6",
#   "MOY_APPELS_M6"
# )

# suppression B : "Le coefficient estimé de chaque modalité des variables explicatives doittre significativement différent de 0"
# SEXEMODE_PAIEMENT, NB_MIGRATIONS, FLAG_MIGRATION_HAUSSE,
# TELEPHONE_INIT, FLAG_APPELS_VERS_INTERNATIONAL, FLAG_APPELS_DEPUIS_INTERNATIONAL

# suppression C : critére AIC (procédure backward)
# SEXE MODE_PAIEMENT, NB_MIGRATIONS, FLAG_REENGAGEMENT,
# FLAG_APPELS_VERS_INTERNATIONAL, FLAG_APPELS_DEPUIS_INTERNATIONAL

# suppression D : importance dans la random forest
# MODE_PAIEMENT, DUREE_OFFRE_INIT, FLAG_MIGRATION_HAUSSE, FLAG_APPELS_DEPUIS_INTERNATIONAL,
# FLAG_TELECHARGEMENT_SONNERIE, FLAG_PERSONNALISATION_REPONDEUR, MOY_NB_SMS_M6, FLAG_APPELS_VERS_INTERNATIONAL



data2019v2utile <- data2019v2[, which(colnames(data2019v2) %in% VAR_UTILES)]


##################################################################################
# TRAINING / TEST 
##################################################################################

set.seed(237)
train2019v2 <- sample(1:nrow(data2019v2utile), round(nrow(data2019v2utile)*0.8))
ech_train2019v2 <- data2019v2utile[train2019v2,]
ech_test2019v2 <- data2019v2utile[-train2019v2,]


##################################################################################
# CONSTRUCTION DU MODELE
##################################################################################
# REGRESSION LOGISTIQUE ##########################################################

modele1 <- glm(FLAG_RESILIATION ~ ., data = ech_train2019v2, family = "binomial")
summary(modele1)
# stepAIC(modele1, direction = "backward")


# RANDOM FORESTS #################################################################

# rf <- tune.randomForest(x = ech_train2019v2[,-1], y = ech_train2019v2[,1])
# rf <- rf$best.model
# rf
# avec toutes les variables : 500 arbres, 5 variables tried at each split
# avec 18 variables : 500 arbres, 4 variables tried at each split

set.seed(237)
rf <- randomForest(FLAG_RESILIATION ~ ., data = ech_train2019v2, ntree = 500, mtry = 4, keep.forest = TRUE)

importance <- rf$importance[order(rf$importance[,1], decreasing = TRUE),]
df.importance <- as.data.frame(importance)
ggplot(data = df.importance, aes(x = reorder(rownames(df.importance), -importance), y = importance)) +
  geom_bar(stat = "identity") +
  # theme(axis.text.x = element_text()) +
  xlab("Variables") +
  ylab("Importance") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(face = "bold", size = 10, color = "black", angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 13, color = "black")
  )

rfPred <- predict(rf, newdata = ech_test2019v2, type = "prob")


# Calcul des scores
ech_test2019v2 <- cbind(ech_test2019v2, rf1 = rfPred[,2])
Pred_step <- prediction(ech_test2019v2$rf1, ech_test2019v2$FLAG_RESILIATION)
Perf_step <- performance(Pred_step, "tpr", "fpr")
plot(Perf_step, main = "Courbe ROC", col = "red", lwd = 3)
abline(0, 1, lwd = 3)

# AUC
Perf_step2 <- performance(Pred_step, "auc")
Perf_step2@y.values[1]

# Matrice de confusion
ech_test2019v2_pred <- cbind(ech_test2019v2, response = factor(ifelse(ech_test2019v2$rf1 > 0.5, 1, 0)))
m.confusion <- table(prediction = ech_test2019v2_pred$response, truth = ech_test2019v2_pred$FLAG_RESILIATION)
print(m.confusion)
print(sum(diag(m.confusion))/sum(m.confusion)*100)
# avec toutes les variables : 87.66581
# avec 18 variables : 88.03254
# on ne change pas le seuil de 0.5 0.18 pour tenir compte du taux Y=1, déséquilibré dans la base

##################################################################################
# APPLICATION DU MODELE SUR LA BASE 2020
##################################################################################
##################################################################################
# TRAITEMENT : VALEURS MANQUANTES ET ABERRANTES + SEPARATION EN CLASSES
##################################################################################

# DATE_NAISSANCE et AGE ############################################################################
# calcul de l'ége exact vis-é-vis de la période d'étude
# sapply(data2020v2$DATE_NAISSANCE, function(x) {
#return(time_length(interval(x, ymd("2020-03-31")), "years"))
#})



data2020v2$AGE <- floor(time_length(interval(ymd(data2020v2$DATE_NAISSANCE), ymd("2021-03-31")),"years"))
write.table(x = data2020v2$AGE, file = "data2021v2AGE.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

data2020v2$AGE <- as.numeric(as.matrix(read.table("data2021v2AGE.txt")))

for (csp in levels(data2020v2$CSP)) {
  data2020v2$AGE[is.na(data2020v2$DATE_NAISSANCE) & data2020v2$CSP == csp] <-
    mean(data2020v2$AGE[data2020v2$CSP == csp], na.rm = TRUE)
}
rm(csp)
data2020v2 <- data2020v2[-which(data2020v2$AGE >= 84),]
data2020v2$AGE <- cut(data2020v2$AGE, breaks = c(min(data2020v2$AGE), 24, 50, max(data2020v2$AGE)), labels = c("<= 24", "25-50", ">= 51"), include.lowest = TRUE)

# SEXE inutile ###################################################################
# IDEM

set.seed(237)
genre <- sample(c("Masculin", "Féminin"), length(which(data2020v2$SEXE == "")), replace = TRUE, prob = rep(0.5, 2))
data2020v2$SEXE[which(data2020v2$SEXE == "")] <- ifelse(genre == "Masculin", "Masculin", "Féminin")
data2020v2$SEXE <- droplevels(data2020v2$SEXE)
rm(genre)


# CSP ############################################################################
# IDEM

data2020v2$CSP <- as.character(data2020v2$CSP)
data2020v2$CSP[which(data2020v2$CSP %in% c("Employé", "Sans emploi"))] <- "Employé/Sans emploi"
data2020v2$CSP[which(data2020v2$CSP %in% c("Fonctionnaire", "Cadre", "Autre"))] <- "Fonc/Cadre/Autre"
data2020v2$CSP[which(data2020v2$CSP %in% c("Commerçant", "Profession libérale"))] <- "Commerçant/Libéral"
data2020v2$CSP <- as.factor(data2020v2$CSP)

# CODE_POSTAL inutile ############################################################
# TAILLE_VILLE ###################################################################
# IDEM




# DATE_ACTIVATION et DUREE_ACTIVATION ############################################
# IDEM

data2020v2$DUREE_ACTIVATION <- as.numeric(round((ymd("2021-03-31") - data2020v2$DATE_ACTIVATION)/30, digits = 1))
data2020v2$DUREE_ACTIVATION <- cut(data2020v2$DUREE_ACTIVATION, breaks = c(min(data2020v2$DUREE_ACTIVATION), 12, 31.5, 38, max(data2020v2$DUREE_ACTIVATION)), labels = c("<= 12", "12.1-31.5", "31.6-38", ">= 38.1"), include.lowest = TRUE)


# DUREE_OFFRE_INIT ###############################################################
# IDEM

data2020v2$DUREE_OFFRE_INIT <- as.character(data2020v2$DUREE_OFFRE_INIT)
data2020v2$DUREE_OFFRE_INIT[which(data2020v2$DUREE_OFFRE_INIT %in% c("0.5","1","4","3"))] <- "0.5/1/3/4"
data2020v2$DUREE_OFFRE_INIT[which(data2020v2$DUREE_OFFRE_INIT %in% c("8","10","6","2"))] <- "2/6/8/10"
data2020v2$DUREE_OFFRE_INIT <- as.factor(data2020v2$DUREE_OFFRE_INIT)


# DUREE_OFFRE ####################################################################
# IDEM

data2020v2$DUREE_OFFRE <- as.numeric(as.character(data2020v2$DUREE_OFFRE))
data2020v2$DUREE_OFFRE <- cut(data2020v2$DUREE_OFFRE, breaks = c(min(data2020v2$DUREE_OFFRE), 2, 4, max(data2020v2$DUREE_OFFRE)), labels = c("<= 2", "3/4", ">= 5"), include.lowest = TRUE)



# NB_MIGRATIONS ##################################################################
# IDEM

data2020v2 <- data2020v2[-which(data2020v2$NB_MIGRATIONS >= 9),]
data2020v2$NB_MIGRATIONS <- cut(data2020v2$NB_MIGRATIONS, breaks = c(min(data2020v2$NB_MIGRATIONS), 1, 2, max(data2020v2$NB_MIGRATIONS)), labels = c("<= 1", "2", ">= 3"), include.lowest = TRUE)


# FLAG_MIGRATION_HAUSSE ##########################################################
# histo("FLAG_MIGRATION_HAUSSE")

# FLAG_MIGRATION_BAISSE ##########################################################
# histo("FLAG_MIGRATION_BAISSE")

# NB_SERVICES ####################################################################

data2020v2$NB_SERVICES <- as.numeric(as.character(data2020v2$NB_SERVICES))
data2020v2 <- data2020v2[-which(data2020v2$NB_SERVICES >= 11),]
data2020v2$NB_SERVICES <- cut(data2020v2$NB_SERVICES, breaks = c(min(data2020v2$NB_SERVICES), 1, 4, max(data2020v2$NB_SERVICES)), labels = c("<= 1", "2-4", ">= 5"), include.lowest = TRUE)


# FLAG_PERSONNALISATION_REPONDEUR ################################################
# histo("FLAG_PERSONNALISATION_REPONDEUR")

# FLAG_TELECHARGEMENT_SONNERIE ###################################################
# histo("FLAG_TELECHARGEMENT_SONNERIE")

# TELEPHONE_INIT #################################################################
# histo("TELEPHONE_INIT")

# TELEPHONE ######################################################################
# histo("TELEPHONE")

# DATE_FIN_ENGAGEMENT et DUREE_ENGAGEMENT_RESTANT ####################################
# IDEM



moy_duree_engagement <- mean(data2020v2$DATE_FIN_ENGAGEMENT[!is.na(data2020v2$DATE_FIN_ENGAGEMENT) & data2020v2$NB_REENGAGEMENTS == 0]
                             - data2020v2$DATE_ACTIVATION[!is.na(data2020v2$DATE_FIN_ENGAGEMENT) & data2020v2$NB_REENGAGEMENTS == 0])
for (i in 1:nrow(data2020v2)) {
  if (is.na(data2020v2$DATE_FIN_ENGAGEMENT[i])) {
    data2020v2$DATE_FIN_ENGAGEMENT[i] <- data2020v2$DATE_ACTIVATION[i] + moy_duree_engagement
  }
}
rm(i, moy_duree_engagement)

# création variable Date de fin d'engagement restant DUREE_ENGAGEMENT_RESTANT
data2020v2$DUREE_ENGAGEMENT_RESTANT <- as.numeric(round((data2020v2$DATE_FIN_ENGAGEMENT - ymd("2021-03-31"))/30, digits = 1))
data2020v2$DUREE_ENGAGEMENT_RESTANT <- cut(data2020v2$DUREE_ENGAGEMENT_RESTANT, breaks = c(min(data2020v2$DUREE_ENGAGEMENT_RESTANT), -9, 0, 9, max(data2020v2$DUREE_ENGAGEMENT_RESTANT)), labels = c("<= -9", "-8.9-0", "0.1-9", ">= 9.1"), include.lowest = TRUE)



# NB_REENGAGEMENTS devient FLAG_REENGAGEMENT #####################################
# IDEM

data2020v2$NB_REENGAGEMENTS <- as.numeric(as.character(data2020v2$NB_REENGAGEMENTS))
data2020v2$NB_REENGAGEMENTS <- cut(data2020v2$NB_REENGAGEMENTS, breaks = c(-1, 0, max(data2020v2$NB_REENGAGEMENTS)), labels = c("0", "1"), include.lowest = TRUE)
colnames(data2020v2)[which(colnames(data2020v2) == "NB_REENGAGEMENTS")] <- "FLAG_REENGAGEMENT"

# DATE_DERNIER_REENGAGEMENT et DUREE_DERNIER_REENGAGEMENT ########################
# PRESQUE IDEM (diff entre 11.9 et 12)


data2020v2$DUREE_DERNIER_REENGAGEMENT <- as.numeric(round((ymd("2021-03-31") - data2020v2$DATE_DERNIER_REENGAGEMENT)/30, digits = 1))
data2020v2$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2020v2$DUREE_DERNIER_REENGAGEMENT) & data2020v2$DUREE_DERNIER_REENGAGEMENT < 12)] <- "< 12"
data2020v2$DUREE_DERNIER_REENGAGEMENT[which(!is.na(data2020v2$DUREE_DERNIER_REENGAGEMENT) & data2020v2$DUREE_DERNIER_REENGAGEMENT != "< 12")] <- ">= 12"
data2020v2$DUREE_DERNIER_REENGAGEMENT[which(is.na(data2020v2$DUREE_DERNIER_REENGAGEMENT))] <- "Jamais réengagé"
data2020v2$DUREE_DERNIER_REENGAGEMENT <- as.factor(data2020v2$DUREE_DERNIER_REENGAGEMENT)




# SITUATION_IMPAYES ##############################################################
# histo("SITUATION_IMPAYES")

# APPELS et MOY_APPELS_M6 ########################################################
# IDEM

data2020v2$MOY_APPELS_M6 <- rowMeans(cbind(data2020v2$VOL_APPELS_M1,
                                           data2020v2$VOL_APPELS_M2,
                                           data2020v2$VOL_APPELS_M3,
                                           data2020v2$VOL_APPELS_M4,
                                           data2020v2$VOL_APPELS_M5,
                                           data2020v2$VOL_APPELS_M6))
data2020v2$MOY_APPELS_M6 <- cut(data2020v2$MOY_APPELS_M6, breaks = c(min(data2020v2$MOY_APPELS_M6), 9150, 17000, max(data2020v2$MOY_APPELS_M6)), labels = c("<= 9150", "9151-17000", ">= 17001"), include.lowest = TRUE)

# FLAG_APPELS_VERS_INTERNATIONAL #################################################
# histo("FLAG_APPELS_VERS_INTERNATIONAL")

# FLAG_APPELS_DEPUIS_INTERNATIONAL ###############################################
# histo("FLAG_APPELS_DEPUIS_INTERNATIONAL")

# FLAG_APPELS_NUMEROS_SPECIAUX ###################################################
# histo("FLAG_APPELS_NUMEROS_SPECIAUX")

# SMS et MOY_NB_SMS_M6 ###########################################################
# IDEM

data2020v2$NB_SMS_M3[is.na(data2020v2$NB_SMS_M3)] <- as.integer(round(mean(data2020v2$NB_SMS_M3, na.rm = TRUE), digits = 0))
data2020v2$MOY_NB_SMS_M6 <- rowMeans(cbind(data2020v2$NB_SMS_M1,
                                           data2020v2$NB_SMS_M2,
                                           data2020v2$NB_SMS_M3,
                                           data2020v2$NB_SMS_M4,
                                           data2020v2$NB_SMS_M5,
                                           data2020v2$NB_SMS_M6))
data2020v2$MOY_NB_SMS_M6 <- cut(data2020v2$MOY_NB_SMS_M6, breaks = c(min(data2020v2$MOY_NB_SMS_M6), 8, max(data2020v2$MOY_NB_SMS_M6)), labels = c("<= 8", ">= 9"), include.lowest = TRUE)


# SEGMENT ########################################################################
# IDEM

data2020v2$SEGMENT <- as.character(data2020v2$SEGMENT)
data2020v2$SEGMENT[which(data2020v2$SEGMENT %in% c("B", "C"))] <- "B/C"
data2020v2$SEGMENT <- as.factor(data2020v2$SEGMENT)


##################################################################################
# SELECTION DES VARIABLES
##################################################################################

VAR_UTILES_PRED <- c(
  "ID_CLIENT",
  # "SEXE",
  "CSP",
  # "TAILLE_VILLE",
  # "TYPE_VILLE",
  #"REVENU_MOYEN_VILLE",
  "ENSEIGNE",
  # "MODE_PAIEMENT",
  # "DUREE_OFFRE_INIT",
  "DUREE_OFFRE",
  # "NB_MIGRATIONS",
  # "FLAG_MIGRATION_HAUSSE",
  "FLAG_MIGRATION_BAISSE",
  "NB_SERVICES",
  # "FLAG_PERSONNALISATION_REPONDEUR",
  # "FLAG_TELECHARGEMENT_SONNERIE",
  "TELEPHONE_INIT",
  "TELEPHONE",
  "FLAG_REENGAGEMENT",
  "SITUATION_IMPAYES",
  # "FLAG_APPELS_VERS_INTERNATIONAL",
  # "FLAG_APPELS_DEPUIS_INTERNATIONAL",
  "FLAG_APPELS_NUMEROS_SPECIAUX",
  "SEGMENT",
  "AGE",
  "DUREE_ACTIVATION",
  "DUREE_ENGAGEMENT_RESTANT",
  "DUREE_DERNIER_REENGAGEMENT",
  # "MOY_NB_SMS_M6",
  "MOY_APPELS_M6"
)

data2020v2utile <- data2020v2[, which(colnames(data2020v2) %in% VAR_UTILES_PRED)]


##################################################################################
# ATTRIBUTION DU SCORE
##################################################################################

score <- predict(rf, newdata = data2020v2utile[,-1], type = "prob")
predictionsV2 <- cbind(data2020v2utile$ID_CLIENT, score = score[,2])
predictionsV2 <- as.data.frame(predictionsV2)

base_C5 <- predictionsV2[order(predictionsV2$score, decreasing = TRUE),]
final <- base_C5[1:2000, "V1"]

write.table(
  x = final,
  file = "C5_LEMOFOUET_V2.txt",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE)

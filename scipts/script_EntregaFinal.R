# Entrega Final
rm(list=ls())
setwd("C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1/Entrega-Final/stores/")

library(tidyverse)
library(table1)

births <- read_csv("microdata_nacimientos.csv")

# Limpieza de variables:
# Bajo peso al nacer var dependiente
births <- births %>% mutate(PESO_NAC = ifelse(PESO_NAC == 9, NA, PESO_NAC))
births <- births %>% mutate(bajo_peso = ifelse(PESO_NAC == 1 |
                                                 PESO_NAC == 2 |
                                                 PESO_NAC == 3 |
                                                 PESO_NAC == 4,'SI','NO'))
table(is.na(births$IDPERTET)) #12,6K NAS

# Nacio en hospital
births <- births %>% mutate(SIT_PARTO = ifelse(SIT_PARTO == 9, NA, SIT_PARTO))
births <- births %>% mutate(hospital = ifelse(SIT_PARTO == 1,1,0))

# Prematuro
births <- births %>% mutate(T_GES = ifelse(T_GES == 9 | T_GES == 6, NA, T_GES))

births <- births %>% mutate(prematuro = ifelse(T_GES == 1 |
                                                 T_GES == 2 |
                                                 T_GES == 3,1,0))

# Num consultas prenatales
births <- births %>% mutate(n_consultas = ifelse(NUMCONSUL == 99, NA, NUMCONSUL))

# APGAR1 APGAR2
births <- births %>% mutate(APGAR1 = ifelse(APGAR1 == 99, NA, APGAR1))
births <- births %>% mutate(APGAR2 = ifelse(APGAR2 == 99, NA, APGAR2))

# Identidad etnica (factor)
births <- births %>% mutate(id_etnica = ifelse(IDPERTET == 9, NA, IDPERTET))
#births$id_etnica <- as.factor(births$IDPERTET)
# Edad Madre (factor)
births <- births %>% mutate(EDAD_MADRE = ifelse(EDAD_MADRE == 99, NA, EDAD_MADRE))

# Madre casada
births <- births %>% mutate(EST_CIVM = ifelse(EST_CIVM == 9, NA, EST_CIVM))
births <- births %>% mutate(m_casada = ifelse(EST_CIVM == 6, 1, 0))

# Ultimo anio escolar alcanzado por la madre y padre
births <- births %>% mutate(ULTCURMAD = ifelse(ULTCURMAD == 99, NA, ULTCURMAD))
p1 = "[:digit:]+"
births <- births %>% 
  mutate(m_anios_colegio = str_extract(string = births$ULTCURMAD,pattern = p1))
births$m_anios_colegio <- as.numeric(births$m_anios_colegio)

births <- births %>% mutate(ULTCURPAD = ifelse(ULTCURPAD == 99, NA, ULTCURPAD))
births <- births %>% 
  mutate(p_anios_colegio = str_extract(string = births$ULTCURPAD,pattern = p1))
births$p_anios_colegio <- as.numeric(births$p_anios_colegio)
# Num hijos
births <- births %>% 
  mutate(n_hijos = str_extract(string = births$N_HIJOSV,pattern = p1))
births$n_hijos <- as.numeric(births$n_hijos)
# Nun embarazos
births <- births %>% mutate(n_emb = N_EMB)

# La madre esta en el regimen contributivo
births <- births %>% mutate(m_reg_contribu = ifelse(SEG_SOCIAL == 1, 1, 0))

# crear identificados
births <- births %>% select(bajo_peso,
                            hospital,
                            prematuro,
                            id_etnica,
                            m_casada,
                            m_anios_colegio,
                            p_anios_colegio,
                            n_hijos,
                            n_emb,
                            m_reg_contribu)

# Eliminar NAs en varianle dependiente
births <- births %>% drop_na()

# Crear identificador
births <- births %>% mutate(id = 1:1119514)

# Estadisticas descriptivas
descriptivas <- births
y <- c("bajo_peso", "hospital", "prematuro", "id_etnica", "m_casada", "m_reg_contribu")
descriptivas[y] <- lapply(descriptivas[y], factor)

descriptiva_1 <- table1(~ bajo_peso+hospital+prematuro+id_etnica+m_casada+
                          m_anios_colegio+p_anios_colegio+n_hijos+
                          n_emb +m_reg_contribu, 
                        data=descriptivas, overall="Total")

descriptiva_1
# Separar Test y Train set: 80/20 (895,611 / 221,902)
set.seed(10101)
train_sample <- sample(1119514, 895611)

train_set <- births[train_sample, ]
test_set <- births[-train_sample, ]

#test_set <- test_set %>% select(-bajo_peso)

# SMOTE como técnica de balanceo
library("ROSE")
set.seed(10101)
smote <- ovun.sample(bajo_peso ~ ., train_set, method='both')
train_set = smote$data
prop.table(table(train_set$bajo_peso)) #49% no, 50% si

# XG BOOST
library(xgboost)
library(e1071)

grid_default <- expand.grid(nrounds = c(50,100),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

set.seed(10101)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     verbose=FALSE,
                     savePredictions = T)

xgboost <- train(
  as.factor(bajo_peso) ~.,
  data = train_set,
  method = "xgbTree",
  trControl = ctrl,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)

xgboost

# Importancia de variables
var_Imp <- varImp(xgboost, scale = FALSE)
var_Imp
# El modelo que optimiza Sens es:
#nrounds = 50,
#max_depth = 6,
#eta = 0.01,
#gamma = 1,
#min_child_weight = 50,
#colsample_bytree = 0.7,
#subsample = 0.6


pred_xgb <- predict(xgboost,test_set)

confusionMatrix(as.factor(test_set$bajo_peso),pred_xgb)

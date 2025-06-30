library(parallel)
library(doParallel)
library(readr)
library(mice)
library(caret)
library(ggplot2) 
library(dplyr) 
library(psych)


GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing


setwd("C:/Users/HP/Documents/UCM/TFM/BD")

data <- read_csv("data.csv")

data <- subset(data, año >= 2023)


dput(names(data))

data <- as.data.frame(data)

listconti<-c("año", 
             "mes","hora", "dia", "fin_de_semana", "hora_pico", 
             "semana_del_año", "feriado")

listclass<-c("dia_semana")

vardep<-c("viajes")

means <-apply(data[,listconti],2,mean)
sds<-sapply(data[,listconti],sd)

# Estandarizo solo las continuas y uno con las categoricas

datacon<-scale(data[,listconti], center = means, scale = sds)
numerocont<-which(colnames(data)%in%listconti)
data<-cbind(datacon,data[,-numerocont,drop=FALSE ])

data[,vardep]<-as.factor(data[,vardep])



library(dummies)

databis<- dummy.data.frame(data, listclass, sep = ".")

dput(names(databis))

vardep<-"viajes"
nombres1<-c("año", "mes", "hora", "dia", "fin_de_semana", 
            "hora_pico", "semana_del_año", "feriado", "dia_semana.0", "dia_semana.1", "dia_semana.2", "dia_semana.3", 
            "dia_semana.4", "dia_semana.5", "dia_semana.6")
y<-databis[,vardep]
x<-databis[,nombres1]

databis$viajes <- as.numeric(as.character(databis$viajes))

# Step repetido AIC
source("funcion steprepetido.R")

lista <- steprepetido(data = databis, vardep = c("viajes"),
                      listconti = c("año", "mes", "hora", "dia", "fin_de_semana", 
                                    "hora_pico", "semana_del_año", "feriado", "dia_semana.0", 
                                    "dia_semana.1", "dia_semana.2", "dia_semana.3", "dia_semana.4", 
                                    "dia_semana.5", "dia_semana.6"),
                      sinicio = 1234, sfinal = 1234, porcen = 0.7, criterio = "AIC")

tabla <- lista[[1]]
dput(lista[[2]][[1]])

# Step repetido BIC
lista <- steprepetido(data = databis, vardep = c("viajes"),
                      listconti = c("año", "mes", "hora", "dia", "fin_de_semana", 
                                    "hora_pico", "semana_del_año", "feriado", "dia_semana.0", 
                                    "dia_semana.1", "dia_semana.2", "dia_semana.3", "dia_semana.4", 
                                    "dia_semana.5", "dia_semana.6"),
                      sinicio = 1234, sfinal = 1234, porcen = 0.7, criterio = "BIC")

tabla <- lista[[1]]
dput(lista[[2]][[1]])

# COMPARACION VIA CV REPETIDA Y BOXPLOT

source("cruzadas avnnet y lin.R")

medias3<-cruzadalin(data=databis,
                    vardep="viajes",listconti=
                      c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                        "feriado", "dia_semana.5", "dia_semana.4"),
                    listclass=c(""),grupos=2,sinicio=1234,repe=25)

medias3$modelo="STEPAIC"

medias4<-cruzadalin(data=databis,
                    vardep="viajes",listconti=
                      c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                        "feriado", "dia_semana.5"),
                    listclass=c(""),grupos=2,sinicio=1234,repe=25)

medias4$modelo="Reg L"

medias5<-cruzadalin(data=databis,
                    vardep="viajes",listconti=
                      c("año", "mes", "hora", "dia", "fin_de_semana", 
                        "hora_pico", "semana_del_año", "feriado", "dia_semana.0", 
                        "dia_semana.1", "dia_semana.2", "dia_semana.3", "dia_semana.4", 
                        "dia_semana.5", "dia_semana.6"),
                    listclass=c(""),grupos=2,sinicio=1234,repe=25)

medias5$modelo="Reg"


union1 <- rbind(medias3, medias4, medias5)

par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo)
union1$error2 <- sqrt(union1$error)
par(cex.axis = 0.8, las = 2)  
boxplot(data = union1, col = "pink", error2 ~ modelo)

describe(databis)

# Número de variables en cada modelo
nvars <- c(
  STEPAIC = length(c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", "feriado", "dia_semana.5", "dia_semana.4")),
  STEPBIC = length(c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", "feriado", "dia_semana.5")),
  Reg = length(c("año", "mes", "hora", "dia", "fin_de_semana", "hora_pico", "semana_del_año", "feriado",
                 "dia_semana.0", "dia_semana.1", "dia_semana.2", "dia_semana.3", "dia_semana.4", "dia_semana.5", "dia_semana.6"))
)

# Calcular mean y sd del error y error2 para cada modelo
summary_df <- rbind(
  STEPAIC = c(
    mean_error = mean(medias3$error),
    sd_error = sd(medias3$error),
    mean_error2 = mean(sqrt(medias3$error)),
    sd_error2 = sd(sqrt(medias3$error)),
    nvars = nvars["STEPAIC"]
  ),
  STEPBIC = c(
    mean_error = mean(medias4$error),
    sd_error = sd(medias4$error),
    mean_error2 = mean(sqrt(medias4$error)),
    sd_error2 = sd(sqrt(medias4$error)),
    nvars = nvars["STEPBIC"]
  ),
  Reg = c(
    mean_error = mean(medias5$error),
    sd_error = sd(medias5$error),
    mean_error2 = mean(sqrt(medias5$error)),
    sd_error2 = sd(sqrt(medias5$error)),
    nvars = nvars["Reg"]
  )
)

# Convertir a data.frame
summary_df <- as.data.frame(summary_df)
summary_df$modelo <- rownames(summary_df)
rownames(summary_df) <- NULL

variables <- list(
  STEPAIC = c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", "feriado", "dia_semana.5", "dia_semana.4"),
  STEPBIC = c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", "feriado", "dia_semana.5"),
  Reg = c("año", "mes", "hora", "dia", "fin_de_semana", "hora_pico", "semana_del_año", "feriado",
          "dia_semana.0", "dia_semana.1", "dia_semana.2", "dia_semana.3", "dia_semana.4", "dia_semana.5", "dia_semana.6")
)

summary_df$variables <- sapply(variables, function(x) paste(x, collapse = ", "))

print(summary_df)

#REGRESION LINEAL

# Variables seleccionadas
variables <- c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", "feriado", "dia_semana.5")

# Fórmula para la regresión lineal
formula <- as.formula(paste("viajes ~", paste(variables, collapse = " + ")))

# Ajustar el modelo de regresión lineal
modelo_lineal <- lm(formula, data = databis)

# Resumen del modelo
summary(modelo_lineal)


#MODELADO

#Red Neuronal

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 

set.seed(1234)
nnetgrid <-  expand.grid(size=c(5,10,15,20),decay=c(0.01,0.1,0.001),bag=F)

completo<-data.frame()
listaiter<-c(20,30,50,100,200,500,1000)

for (iter in listaiter)
{
  rednnet<- train(viajes~.,
                  data=databis,
                  method="avNNet",linout = TRUE,maxit=iter,
                  trControl=control,repeats=5,tuneGrid=nnetgrid,trace=F)
  rednnet$results$itera<-iter
  completo<-rbind(completo,rednnet$results)
  
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(itera), y=RMSE, 
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)


medias_red<-cruzadaavnnet(data=databis,
                          vardep="viajes",listconti=
                            c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                              "feriado", "dia_semana.5"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=5,repeticiones=5,itera=200,
                          size=c(20),decay=c(0.01))

medias_red$modelo="Red"

union1 <- rbind(medias3, medias4, medias_red)

par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo)
union1$error2 <- sqrt(union1$error)
par(cex.axis = 0.8, las = 2)  
boxplot(data = union1, col = "pink", error2 ~ modelo)

completo[order(completo$RMSE), ]

#Random Forest y Bagging

source ("cruzada rf continua.R")

#RANDOM FOREST

set.seed(1234)
mtry_vals <- c(2,3,4,5,6)
nodesize_vals <- c(5, 10, 15)
results <- data.frame()

for (nodesize_val in nodesize_vals) {
  rfgrid <- expand.grid(mtry = mtry_vals)
  
  control <- trainControl(method = "cv", number = 4, savePredictions = "all")
  
  rf <- train(viajes ~ hora_pico+hora+fin_de_semana+año+semana_del_año+feriado+dia_semana.5, data = databis, method = "rf",
              trControl = control, tuneGrid = rfgrid,
              ntree = 150, sampsize = 600,
              nodesize = nodesize_val, replace = TRUE,
              importance = TRUE)
  
  temp <- rf$results
  temp$nodesize <- nodesize_val
  results <- rbind(results, temp)
}

print(results)
# Cargar librerías necesarias
library(ggplot2)
library(caret)

# Obtener importancia de variables
importancia <- varImp(rf)$importance

# Asegurar que es un data frame y agregar nombres de variables
importancia <- data.frame(variable = rownames(importancia), importancia)
rownames(importancia) <- NULL

# Filtrar posibles NAs
importancia <- importancia[!is.na(importancia$Overall), ]

# Ordenar por importancia
importancia <- importancia[order(importancia$Overall, decreasing = TRUE), ]

# Seleccionar top 10
top_vars <- head(importancia, 10)

ggplot(top_vars, aes(x = reorder(variable, Overall), y = Overall)) +
  geom_col(fill = "#80B1D3") +
  coord_flip() +
  labs(
    title = "Importancia de las 10 variables más relevantes",
    x = "Variable",
    y = "Importancia (Overall)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "plain"),
    axis.title = element_text(face = "plain"),
    axis.text = element_text(face = "plain") # texto en diagonal
  )
#BAGGING

set.seed(1234)
mtry_vals <- c(7)
nodesize_vals <- c(5, 10, 15)
results <- data.frame()

for (nodesize_val in nodesize_vals) {
  rfgrid <- expand.grid(mtry = mtry_vals)
  
  control <- trainControl(method = "cv", number = 4, savePredictions = "all")
  
  rf <- train(viajes ~ hora_pico+hora+fin_de_semana+año+semana_del_año+feriado+dia_semana.5, data = databis, method = "rf",
              trControl = control, tuneGrid = rfgrid,
              ntree = 150, sampsize = 600,
              nodesize = nodesize_val, replace = TRUE,
              importance = TRUE)
  
  temp <- rf$results
  temp$nodesize <- nodesize_val
  results <- rbind(results, temp)
}

print(results)

library(randomForest)
rf_model <- randomForest(viajes ~ ., data = data2, ntree = 500)
plot(rf_model,
     main = "Árboles óptimos")  

print(results)

medias_bagging<-cruzadarf(data=databis,
                          vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año","feriado", "dia_semana.5"),
                          listclass=c(""),
                          grupos=10,sinicio=1234,repe=10,nodesize=5,
                          mtry=7,ntree=150,replace=TRUE,sampsize=600)

medias_bagging$modelo="Bagging"

medias_rf<-cruzadarf(data=databis,
                     vardep="viajes",listconti=
                       c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                         "feriado", "dia_semana.5"),listclass=c(""),
                     grupos=10,sinicio=1234,repe=10,nodesize=5,
                     mtry=6,ntree=150,replace=TRUE,sampsize=600)

medias_rf$modelo="RF"

union1 <- rbind(medias3, medias4, medias_red, medias_rf, medias_bagging)

par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo, 
        xlab = "Modelo", ylab = "Error")

union1$error2 <- sqrt(union1$error)

par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error2 ~ modelo, 
        xlab = "Modelo", ylab = "RMSE")
#GRADIENT BOOSTING

source ("cruzada gbm continua.R")

set.seed(1234)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all")

gbm<- train(viajes~hora_pico+hora+fin_de_semana+año+semana_del_año+feriado+dia_semana.5
            ,data=data2,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm)



medias_gbm<-cruzadagbm(data=data2,
                       vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                                                   "feriado", "dia_semana.5"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=10,
                       n.minobsinnode=5,shrinkage=0.03,n.trees=5000,interaction.depth=2)

medias_gbm$modelo="GRM"


union1<- rbind(medias3, medias4, medias_red,medias_rf, medias_bagging, medias_gbm)
par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo)
union1$error2 <- sqrt(union1$error)
par(cex.axis = 0.8, las = 2)  
boxplot(data = union1, col = "pink", error2 ~ modelo)


#XGBOOST 
source ("cruzada xgboost continua.R")

set.seed(1234)
xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

xgbm<- train(viajes~hora_pico+hora+fin_de_semana+año+semana_del_año+feriado+dia_semana.5,data=data2,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)


medias_xgb<-cruzadaxgbm(data=data2,
                        vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                                                    "feriado", "dia_semana.5"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,
                        min_child_weight=10,eta=0.05,nrounds=100,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)

medias_xgb$modelo="XGB"

union1<- rbind(medias_red,medias_rf, medias_bagging, medias_gbm,medias_xgb)
par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo)
union1$error2 <- sqrt(union1$error)
par(cex.axis = 0.8, las = 2)  
boxplot(data = union1, col = "pink", error2 ~ modelo)


#SVM MODELS
source ("cruzada SVM continua lineal.R")
source ("cruzada SVM continua polinomial.R")
source ("cruzada SVM continua RBF.R")

#  SVM LINEAL: SOLO PARÁMETRO C

set.seed(1234)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(viajes~.,data=data3,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results


medias_svm<-cruzadaSVM(data=data3,
                       vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                                                   "feriado", "dia_semana.5"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=5,C=5)

medias_svm$modelo="SVM"

listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
            "feriado", "dia_semana.5")
data3<-databis[,c(listconti,vardep)]

data3 <- data3[sample(nrow(data3), 600), ]

#  SVM Polinomial: PARÁMETROS C, degree, scale


SVMgrid<-expand.grid(C=c(0.01,0.1,0.05,0.5,1,5),
                     degree=c(2,3),scale=c(0.1,0.5,1,5))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(viajes~.,data=data3,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

medias_poly<-cruzadaSVMpoly(data=data3,
                            vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                                                        "feriado", "dia_semana.5"),
                            listclass=c(""),
                            grupos=4,sinicio=1234,repe=5,
                            C=0.5,degree=3,scale=0.5)

medias_poly$modelo="SVMPoli"

#  SVM RBF: PARÁMETROS C, sigma
SVMgrid<-expand.grid(C=c(0.5,1,2,5,10),
                     sigma=c(0.0001,0.005,0.01,0.05))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(viajes~.,data=data3,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)


dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x = factor(C), y = RMSE, 
                color = factor(sigma))) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  labs(
    x = "C",
    y = "RMSE",
    color = "Sigma"
  ) +
  theme_minimal()

medias_svmrbf<-cruzadaSVMRBF(data=data3,
                             vardep="viajes",listconti=c("hora_pico", "hora", "fin_de_semana", "año", "semana_del_año", 
                                                         "feriado", "dia_semana.5"),
                             listclass=c(""),
                             grupos=4,sinicio=1234,repe=5,
                             C=10,sigma=0.05)

medias_svmrbf$modelo="SVMRBF"

union1<- rbind(medias_svmrbf,medias4,medias_red,medias_rf, medias_bagging, medias_gbm,medias_xgb,medias_svm,medias_poly)
par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error ~ modelo, 
        xlab = "Modelo", ylab = "Error")

union1$error2 <- sqrt(union1$error)

par(cex.axis = 0.8, las = 2)
boxplot(data = union1, col = "pink", error2 ~ modelo, 
        xlab = "Modelo", ylab = "RMSE")


library(dplyr)

resumen_modelos <- union1 %>%
  group_by(modelo) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    Rsquared = mean(Rsquared, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    RMSESD = sd(RMSE, na.rm = TRUE),
    RsquaredSD = sd(Rsquared, na.rm = TRUE),
    MAESD = sd(MAE, na.rm = TRUE)
  )
print(resumen_modelos)

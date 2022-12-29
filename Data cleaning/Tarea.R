# Cargo las librerias 

library(questionr)

library(psych)

library(car)

library(corrplot)

library(caret)

library(ggplot2)

library(lmSupport)

library(unmarked)

library(VGAM)

library(pROC)

library(glmnet)

library(readxl)

# Cargo las funciones
source("FuncionesRosa.R")

datos <- read_excel("DatosImpuestos_Tarea.xlsx")

datos <- datos[,-c(3,4,5,8)]

str(datos)

#datos[,c(2,4,8,9,27,28)] <- lapply(datos[,c(2,4,8,9,27,28)], factor)
summary(datos)

#sapply(Filter(is.numeric, datos),function(x) length(unique(x)))

describe(Filter(is.numeric, datos))

datos$Tamano <- recode.na(datos$Tamano,"?")

# Missings no declarados variables cuantitativas (-1, 99999)
datos$Locales<-replace(datos$Locales,which(datos$Locales==99999),NA)

# Valores fuera de rango
datos$Empresario<-replace(datos$Empresario, which((datos$Empresario < 0)|(datos$Empresario>100)), NA)
datos$DeudaSaldada<-replace(datos$DeudaSaldada, which((datos$DeudaSaldada < 0)|(datos$DeudaSaldada>100)), NA)


#Variables cualitativas con categorias poco representadas
#freq(datos$Actividad_Municipio)
#datos$Actividad_Municipio<-recode(datos$Actividad_Municipio, "c('Agricultura', 'Ganaderia')='Agropecuario'")

#Eliminar las comunidades con poca representacion (quizas lo haga)
freq(datos$CCAA)
#datos$CCAA<-recode(datos$CCAA, "'Ceuta'='CyM';'Melilla'='CyM'")
datos$CCAA<-recode.na(datos$CCAA,"Ceuta")
datos$CCAA<-recode.na(datos$CCAA,"Melilla")

#Eliminar los distritos con poca representacion
freq(datos$Distrito)
datos$Distrito<-recode.na(datos$Distrito,"51")
datos$Distrito<-recode.na(datos$Distrito,"52")

#Indico la variableObj, el ID y las Input 
varObjCont<-datos$Vehiculo
varObjBin<-datos$Vehiculo_Cuali
#input<-as.data.frame(datos[, -c(6, 9)])
input<-as.data.frame(datos[, -c(3,5)])
#row.names(input)<-datos$ID #Se considera la variable identificadora (no tengo una identificadora)

##At?picos
# Cuento el porcentaje de at?picos de cada variable. 
sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[2]])/nrow(input)


## MISSINGS
#Busco si existe alg?n patr?n en los missings, que me pueda ayudar a entenderlos
#corrplot(cor(is.na(input[colnames(input)[colSums(is.na(input))>0]])),method = "ellipse",type = "upper") #No se aprecia ning?n patr?n

#Proporci?n de missings por variable y observaci?n
input$prop_missings<-apply(is.na(input),1,mean)
summary(input$prop_missings)
(prop_missingsVars<-apply(is.na(input),2,mean))

## Imputaciones
# Imputo todas las cuantitativas, seleccionar el tipo de imputaci?n: media, mediana o aleatorio
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio"))
summary(input)

# Imputo todas las cualitativas, seleccionar el tipo de imputaci?n: moda o aleatorio
# Si solo se quiere imputar una, variable<-ImputacionCuali(variable,"moda")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) ImputacionCuali(x,"aleatorio"))
# A veces se cambia el tipo de factor a character al imputar, as? que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

# Reviso que no queden datos missings
summary(input)


#Obtengo la importancia de las variables. Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes
#graficoVcramer(input,varObjBin)
#graficoVcramer(input,varObjCont)


#Todas las variables num?ricas frente a la objetivo continua
#graficoCorrelacion(varObjCont,input) #Nos fijamos en la forma de las l?neas rojas (si hay muchas variables num?ricas, tarda un poco)
#corrplot(cor(cbind(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")

#Busco las mejores transformaciones para las variables num?ricas con respesto a los dos tipos de variables
input_cont<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjCont))
input_bin<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjBin))

saveRDS(data.frame(input_bin,varObjBin),"todo_bin")
saveRDS(data.frame(input_cont,varObjCont),"todo_cont")


## Comienzo con la regresi?n lineal
todo<-data.frame(input,varObjCont)

#Obtengo la partici?n
set.seed(2665411)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,] 

#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) #En test hay bastante diferencia, seguramente sobren variables

graficoVcramer(todo,varObjCont) #Pruebo con las m?s importantes

modelo2<-lm(varObjCont~Distrito+CCAA+Vivienda+Salario_2+Salario_4+Salario_1,data=data_train)
modelEffectSizes(modelo2)
summary(modelo2)
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 

#Pruebo un modelo con menos variables, bas?ndome en la importancia de las variables
modelo3<-lm(varObjCont~Distrito+CCAA+Vivienda,data=data_train)
summary(modelo3)
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 

#Pruebo con una interaccion sobre el anterior
# Se podr?an probar todas las interacciones dos a dos. Esta es la que mejor resultado da
modelo4<-lm(varObjCont~Distrito+CCAA+Vivienda+Distrito:CCAA+Vivienda:Distrito,data=data_train)
summary(modelo4)
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test)

#Hago validaci?n repetida para ver qu? modelo es mejor
modelo1VC <- train(formula(modelo1),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo2VC <- train(formula(modelo2),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo3VC <- train(formula(modelo3),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo4VC <- train(formula(modelo4),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

results<-data.frame(rbind(modelo1VC$resample,modelo2VC$resample,modelo3VC$resample,modelo4VC$resample),modelo=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100)))
#boxplot(Rsquared~modelo,data=results)
aggregate(Rsquared~modelo, data = results, mean) #el 4 tiene mayor R2 medio
aggregate(Rsquared~modelo, data = results, sd) #tb tiene mayor variabilidad

length(coef(modelo1));length(coef(modelo2));length(coef(modelo3));length(coef(modelo4))

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) 

modelEffectSizes(modelo1)
#barplot(sort(modelEffectSizes(modelo1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

datos<-readRDS("todo_cont") #todo_cont contiene las variables y transformaciones para la variable continua

#Hago la partici?n
set.seed(12345678)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

# Este fue el modelo ganador el d?a 2
modeloManual<-modelo1
summary(modeloManual)
Rsq(modeloManual,"varObjCont",data_train)
Rsq(modeloManual,"varObjCont",data_test)

# Seleccion de variables "cl?sica"
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train) #Modelo maximo, le quitamos las transformaciones
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
Rsq(modeloStepAIC,"varObjCont",data_test)

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
summary(modeloBackAIC)
Rsq(modeloBackAIC,"varObjCont",data_test) #son iguales

coef(modeloStepAIC)
coef(modeloBackAIC)

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
Rsq(modeloStepBIC,"varObjCont",data_test) #Un pelin peor que el anterior, habr? que mirar el n?mero de par?metros

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)
Rsq(modeloBackBIC,"varObjCont",data_test) # son iguales

coef(modeloStepBIC)
coef(modeloBackBIC)

#M?todo Stepwise y Backward son iguales,ver mejor modelo AIC o BIC seg?n n?mero par?metros
modeloStepAIC$rank
modeloStepBIC$rank

Rsq(modeloStepBIC,"varObjCont",data_test)
Rsq(modeloStepAIC,"varObjCont",data_test)

#Genero interacciones
formInt<-formulaInteracciones(datos[,c(1:23,45)],24)#en el subconjunto de las vbles. originales, la objetivo est? en la columna 25
fullInt<-lm(formInt, data=data_train) #Modelo con todas las variables y todas las interacciones

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
modeloStepAIC_int$rank
summary(modeloStepAIC_int)
Rsq(modeloStepAIC_int,"varObjCont",data_test) #Parecen algo mejores que los anteriores

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_int)
Rsq(modeloStepBIC_int,"varObjCont",data_test) #Un pelin mejor

modeloStepAIC_int$rank #muchos m?s par?metros
modeloStepBIC_int$rank

#Pruebo con todas las transf y las variables originales
fullT<-lm(varObjCont~., data=data_train)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
Rsq(modeloStepAIC_trans,"varObjCont",data_test)

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_trans)
Rsq(modeloStepBIC_trans,"varObjCont",data_test) 

modeloStepAIC_trans$rank 
modeloStepBIC_trans$rank

#No est? claro cu?l es preferible.

#Trans e interacciones
formIntT<-formulaInteracciones(datos,45)
fullIntT<-lm(formIntT, data=data_train)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
Rsq(modeloStepAIC_transInt,"varObjCont",data_test) 

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_transInt)
Rsq(modeloStepBIC_transInt,"varObjCont",data_test)  

modeloStepAIC_transInt$rank 
modeloStepBIC_transInt$rank

#Por el principio de parsimonia, es preferible el modeloStepBIC_transInt

## Pruebo los mejores de cada con validacion cruzada repetida
total<-c()
modelos<-sapply(list(modeloStepAIC,modeloStepBIC,modeloStepBIC_int,modeloStepAIC_trans,modeloStepBIC_trans,modeloStepBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
boxplot(Rsquared~modelo,data=total,main="R-Square") 
aggregate(Rsquared~modelo, data = total, mean) #el 4 y el 7 son mejores
aggregate(Rsquared~modelo, data = total, sd) #su variabilidad es algo m?s alta tb
#vemos el n?mero de parametros
length(coef(modeloStepBIC_int))
length(coef(modeloStepBIC_transInt))
#Igual n?mero de par?metros
formula(modeloStepBIC_int)
formula(modeloStepBIC_transInt)

## Seleccion aleatoria (se coge la submuestra de los datos de entrenamiento)


freq(modelosGenerados,sort="dec")


#los otros 4 son similares en R2.Tomaremos como ganador el segundo pues tiene menos variables.

#Una vez decidido el mejor modelo, hay que evaluarlo 
ModeloGanador<-lm(varObjCont ~ Distrito + xVivienda + logxDeudaFinanc + Salario_4 + 
                    logxMayor_65 + DeudaFinanc + logxEdad_18_65 + DeudaSaldada + 
                    logxMenor_18 + Actividad_Municipio + raiz4Locales + Edad_18_65 + 
                    logxVivienda_Grande + Salario_4:Actividad_Municipio + logxMayor_65:Actividad_Municipio,data=data_train)


# Vemos los coeficientes del modelo ganador
coef(ModeloGanador)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(ModeloGanador,"varObjCont",data_train)
Rsq(ModeloGanador,"varObjCont",data_test) 

#Este c?digo se puede aplicar a regresi?n log?stica con peque?as modificaciones (b?sicamente cambiar lm() por glm(), con la opci?n family=binomial, RSQ por Pseudo-R)
# Para ello, ver c?digo del pdf Regresi?nLog?sticaconR



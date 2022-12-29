

#Recategorizo categ?ricas con "suficientes" observaciones missings
#Solo la variable Clasificaci?n que es la que tiene un 26% missing
#Se considera una categor?a m?s los missing.

input$Ganaderia<-recode(input$Ganaderia,"NA=0",as.numeric = T)
input$Vivienda_Grande<-recode(input$Vivienda_Grande,"NA=0",as.numeric = T)
input$Empresario<-recode(input$Empresario,"NA=0",as.numeric = T)
input$Hosteleria<-recode(input$Hosteleria,"NA=0",as.numeric = T)
input$Comercios<-recode(input$Comercios,"NA=0",as.numeric = T)
input$Agricultura<-recode(input$Agricultura,"NA=0",as.numeric = T)
input$Locales<-recode(input$Locales,"NA=0",as.numeric = T)
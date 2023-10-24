library(shiny)


library(dplyr)
library(ggplot2)
library(readxl)
library("stringr")
library(DT)
library(ggpubr)
library(tidyr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ggcorrplot)
library(party)
library(tree)
library(rattle)
######################################################## wczytywanie ###########################################################

data <- readLines("data/data_hungarian.data") #wczytuje linjki z pliku po kolei
#wylapuje indeksy kiedy pojawia sie "name"
ind <- grep("name",data)

data_proc <- vector("character", length(ind))
for(i in ind){

  data_proc[i/10] <- paste(data[(i-9)],data[i-8],data[i-7],data[i-6],data[i-5],data[i-4],data[i-3],data[i-2],data[i-1],data[i])

  
  data_proc[i/10]<-gsub("name","NA",data_proc[i/10])
  data_proc[i/10]<-gsub("-9. ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub("-9 ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub(" ", ",",data_proc[i/10])
  #-9 oraz 9 oznaczaly nieprawidlowe lub brakujace wartosci
}
rm(i)

df <- matrix(nrow=length(ind),ncol=76)
ind <- ind/10
for(i in ind){
  vector_test<-str_split(data_proc[i], ",")
 vector_test<- (unlist(vector_test))
 vector_test <- replace(vector_test, vector_test == "NA", NA)
 vector_test <- replace(vector_test, vector_test == "NA ", NA)
 vector_test<- as.numeric((vector_test))
  df[i,] <-vector_test
}

rm(ind)
rm(i)
df <- as.data.frame(df)
df <- cbind(df$V3,df$V4,df$V9,df$V10,df$V12,df$V16,df$V19,df$V32,df$V38,df$V40,df$V41,df$V51,df$V58,df$V5,df$V6,df$V7,df$V13,df$V14,df$V15,df$V17,df$V18)
Hungarian_data <- as.data.frame(df)
colnames(Hungarian_data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","thal","diag","painloc","painexer","relrest","smoke","cigs","years","dm","famhist")



rm(df)
rm(vector_test)
rm(data_proc)
rm(data)

###################################################################################


data <- readLines("data/data_va.data") #wczytuje linjki z pliku po kolei
#wylapuje indeksy kiedy pojawia sie "name"
ind <- grep("name",data)

data_proc <- vector("character", length(ind))
for(i in ind){
  
  data_proc[i/10] <- paste(data[(i-9)],data[i-8],data[i-7],data[i-6],data[i-5],data[i-4],data[i-3],data[i-2],data[i-1],data[i])
  

  
  data_proc[i/10]<-gsub("name","NA",data_proc[i/10])
  data_proc[i/10]<-gsub("-9. ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub("-9 ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub(" ", ",",data_proc[i/10])
  #-9 oraz 9 oznaczaly nieprawidlowe lub brakujace wartosci
}
rm(i)


df <- matrix(nrow=length(ind),ncol=76)
ind <- ind/10

for(i in ind){
  vector_test<-str_split(data_proc[i], ",")
  vector_test<- (unlist(vector_test))
  vector_test <- replace(vector_test, vector_test == "NA", NA)
  vector_test <- replace(vector_test, vector_test == "NA ", NA)
  vector_test<- as.numeric((vector_test))
  df[i,] <-vector_test
}
rm(ind)
rm(i)

df <- as.data.frame(df)
df <- cbind(df$V3,df$V4,df$V9,df$V10,df$V12,df$V16,df$V19,df$V32,df$V38,df$V40,df$V41,df$V51,df$V58,df$V5,df$V6,df$V7,df$V13,df$V14,df$V15,df$V17,df$V18)
Va_data <- as.data.frame(df)
colnames(Va_data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","thal","diag","painloc","painexer","relrest","smoke","cigs","years","dm","famhist")


heart_data <-  rbind(Hungarian_data,Va_data)

rm(df)
rm(vector_test)
rm(data_proc)
rm(data)

#------------------------------------------------------------------------#


data <- readLines("data/data_switzerland.data") #wczytuje linjki z pliku po kolei
#wylapuje indeksy kiedy pojawia sie "name"
ind <- grep("name",data)

data_proc <- vector("character", length(ind))
for(i in ind){
  
  data_proc[i/10] <- paste(data[(i-9)],data[i-8],data[i-7],data[i-6],data[i-5],data[i-4],data[i-3],data[i-2],data[i-1],data[i])
  

  
  data_proc[i/10]<-gsub("name","NA",data_proc[i/10])
  data_proc[i/10]<-gsub("-9. ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub("-9 ", "NA ",data_proc[i/10])
  data_proc[i/10]<-gsub(" ", ",",data_proc[i/10])
  #-9 oraz 9 oznaczaly nieprawidlowe lub brakujace wartosci
}
rm(i)


df <- matrix(nrow=length(ind),ncol=76)
ind <- ind/10

for(i in ind){
  vector_test<-str_split(data_proc[i], ",")
  vector_test<- (unlist(vector_test))
  vector_test <- replace(vector_test, vector_test == "NA", NA)
  vector_test <- replace(vector_test, vector_test == "NA ", NA)
  vector_test<- as.numeric((vector_test))
  df[i,] <-vector_test
}
rm(ind)
rm(i)

df <- as.data.frame(df)
df <- cbind(df$V3,df$V4,df$V9,df$V10,df$V12,df$V16,df$V19,df$V32,df$V38,df$V40,df$V41,df$V51,df$V58,df$V5,df$V6,df$V7,df$V13,df$V14,df$V15,df$V17,df$V18)
swiss_data <- as.data.frame(df)
colnames(swiss_data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","thal","diag","painloc","painexer","relrest","smoke","cigs","years","dm","famhist")


heart_data <-  rbind(heart_data,swiss_data)

rm(df)
rm(vector_test)
rm(data_proc)
rm(data)




#--------------------#
 

rm(Hungarian_data)
rm(Va_data)
rm(swiss_data)

#------------- dostosowanie danych -----------------#


heart_data$diag2 <- heart_data$diag
heart_data$diag2[heart_data$diag2>0] = rep(1,length(heart_data$diag2[heart_data$diag2>0]))



heart_data$chol[heart_data$chol==0] <- mean(heart_data$chol, na.rm=TRUE)
heart_data$chol[is.na(heart_data$chol)] <- mean(heart_data$chol, na.rm=TRUE)

heart_data$trestbps[heart_data$trestbps==0] <- mean(heart_data$trestbps, na.rm=TRUE)

heart_data$fbs[is.na(heart_data$fbs)] <- rep(0,length(heart_data$fbs[is.na(heart_data$fbs)]))
heart_data$exang[is.na(heart_data$exang)] <- rep(0,length(heart_data$exang[is.na(heart_data$exang)]))

heart_data$relrest[is.na(heart_data$relrest)] <- rep(0,length(heart_data$relrest[is.na(heart_data$relrest)]))


heart_data = subset(heart_data, select = -c(slope,oldpeak,thal,dm,famhist))


#### koniec wczytywania - poczatek usuwania dodatkowo ###





#-------------------------------------------------    SHINY SERVER  --------------------------------------------------------------------#

shinyServer(function(input, output,session) {
  
  
  
  result <- heart_data
  #modyfikacja danych obslugiwanych
  dane <- reactive(dplyr::filter(result, sex == input$jaka_plec &
                                   age >= input$jaki_wiek[1] & age <= input$jaki_wiek[2]))
  dane_palacz <- reactive(dplyr::filter(result, sex == 1 & smoke == input$czy_pali &
                                          age >= input$jaki_wiek[1] & age <= input$jaki_wiek[2]))
  
  
  output$distPlot <- renderPlot({
    
    #boxplot
     
    ggplot(dane_palacz(),aes(y=trestbps, x=as.factor(diag2)))+ geom_boxplot(na.rm=TRUE)+
   scale_x_discrete(labels=c("zdrowy", "chory"))+
      labs(y="Spoczynkowe cisnienie krwi",x="Wystepowanie choroby")+
      theme(legend.position="top")+theme(legend.title = element_blank())
  
     })
  
    output$plot_corr <- renderPlot({
      
      corr <- round(cor(result),1)
      ggcorrplot(corr)
      
    })
  
    output$plot_density <- renderPlot({
      
      

      ggplot(dane(),aes( x=chol, color=as.factor(diag2)))+ geom_density(na.rm = TRUE)+
      scale_color_discrete(labels=c("zdrowy", "chory"))+
        labs(x="poziom cholesterolu",y="gestosc wystepowania")+
        theme(legend.position="top")+theme(legend.title = element_blank())
        
      
      
    })
    
   
    
 
    
    output$plot_hist_chory <- renderPlot({
      
      ggplot(dane(),aes(x=thalach,fill=as.factor(diag2)))+ geom_histogram(na.rm=TRUE,binwidth=10)+
        scale_fill_manual(values=c("wheat2","lightblue"),labels=c("zdrowy","chory") )+
        labs(x="maksymalne tetno podczas wysilku",y="czestosc wystepowania")+
        theme(legend.position="top")+theme(legend.title = element_blank())
  
      
      
    
      
    })
    
    

    
    output$text <- renderText({ 
      
      #drzewa decyzyjne
      training <- heart_data[1:300,]
      test <- heart_data[301:617,]
      test$diag2 <- as.factor(test$diag2)
      training$diag2 <- as.factor(training$diag2)
      tree <- rpart(diag2~., training)
      predicted_Classes <- predict(tree, test, type="class")
      
      our_test<-test[1,]
      #wprowadzanie danych z testu:
      
      our_test$age=input$test_age
      our_test$sex=as.integer(input$test_sex)
      our_test$cp=as.integer(input$test_cp)
      our_test$painloc=as.integer(input$test_painloc)
      our_test$trestbps=input$test_trestbps
      our_test$chol=input$test_chol
      our_test$fbs=as.integer(input$test_fbs)
      our_test$thalach=input$test_thalach
      our_test$exang=as.integer(input$test_exang)
      our_test$painexer=as.integer(input$test_painexer)
      our_test$relrest=as.integer(input$test_relrest)
      
      
      
       our_result <- predict(tree, our_test, type ="class")
      
      if(our_result[1]==0)
        diagnosis = "zdrowy"
      else if(our_result[1]==1)
        diagnosis="chory"
      else diagnosis="Zapraszam do wypelnienia testu"
      
      diagnosis})
    
   
  })

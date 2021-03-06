library(ggplot2)
library(shiny,warn.conflicts = FALSE)
library(shinythemes,warn.conflicts = FALSE)
library(readxl,warn.conflicts = FALSE)
library(dplyr,warn.conflicts = FALSE)
library(questionr,warn.conflicts = FALSE)
library(rpart,warn.conflicts = FALSE)
library(rpart.plot,warn.conflicts = FALSE)
library(nnet,warn.conflicts = FALSE)
library(NeuralNetTools,warn.conflicts = FALSE)
library(kernlab,warn.conflicts = FALSE)
library(ROCR,warn.conflicts = FALSE)
library(pROC,warn.conflicts = FALSE)
setwd(getwd())

gdata = read_excel("german credit dataset.xls" )
attach(gdata)
gdata$Good[Good == 2] = 0 #2 refers to bad #1 refers to good



#####################################
#Preparer les modeles de comparaison#
#####################################
gdata$Good = as.factor(gdata$Good)
attach(gdata)
train_id=sample(1000,750)
data_train=gdata[train_id,]
data_test=gdata[-train_id,]
logit=glm(Good~.,data = data_train,family = binomial)
prob_test=predict(logit,data_test,type = "response")
pred_test=prediction(prob_test,data_test$Good)
perf_roc_test=performance(pred_test,measure = "tpr",x.measure ="fpr")

mycontrol = rpart.control(cp = 0, xval = 10)
model_ar = rpart(Good~ .,method = "class" ,control = mycontrol, data=data_train)
prob_test_ar=predict(model_ar,data_test,type = "prob")[,2]
pred_test_ar=prediction(prob_test_ar,data_test$Good)
perf_roc_test_ar=performance(pred_test_ar,measure = "tpr",x.measure ="fpr")

Neural = nnet(Good~ .,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
fitNeural = predict(Neural,newdata=data_test)
prednn = prediction( fitNeural, data_test$Good)
perfnn = performance(prednn, "tpr", "fpr")
######################################################


shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = shinytheme("cosmo"),
      "Projet Machine Learning realisé par Fedi Hamdi",
      ## L'Analyse Univariee
      tabPanel("L'Analyse Univariee",
               sidebarPanel(
                 selectInput("type","Selectionner le type de la variable",
                             choices = c("Quantitative","Qualitative")),
                 conditionalPanel( condition = "input.type == 'Quantitative'",
                                   selectInput("vquanti","Choisir la variable",
                                               choices = c(colnames(gdata[c(2,5,13)]))
                                   )
                 ),
                 conditionalPanel( condition = "input.type == 'Qualitative'",
                                   selectInput("vquali","Choisir la variable",
                                               choices = c(colnames(gdata[-c(2,5,13)]))
                                   ))
               ),
               mainPanel(
                 h4("Statistiques descriptives "),
                 hr(),
                 h4(verbatimTextOutput("desc_univariee")),
                 hr(),
                 h4("Les Graphes"),
                 hr(),
                 plotOutput("hist_univariee"),
                 hr(),
                 plotOutput("plot_univariee")
               )
      ),
      tabPanel("L'analyse Bi-variee",
               sidebarPanel(
                 selectInput("type1","Selectionner le type de la variable",
                             choices = c("Quantitative","Qualitative")),
                 conditionalPanel( condition = "input.type1 == 'Quantitative'",
                                   selectInput("vquanti1","Choisir la variable ~ Good",
                                               choices = c(colnames(gdata[c(2,5,13)]))
                                   )
                 ),
                 conditionalPanel( condition = "input.type1 == 'Qualitative'",
                                   selectInput("vquali1","Choisir la variable ~ Good",
                                               choices = c(colnames(gdata[-c(2,5,13)]))
                                   ))
               ),
               mainPanel(
                 plotOutput("box_bivariee"),
                 hr(),
                 h4(tableOutput("test_simple")),
                 plotOutput("mos_bivariee"),
                 hr(),
                 plotOutput("bar_bivariee"),
                 h4(tableOutput("res_bi"))
               )
      ),
      tabPanel("Regression Logistique",

               mainPanel(
                 h4("Le Modele:"),
                 hr(),
                 verbatimTextOutput("mdl"),
                 hr(),
                 plotOutput("pred"),
                 hr(),
                 tableOutput("glm")

               ),
               sidebarPanel(
                 sliderInput("slider1", label = h3("Choisir la taille de 'Train Data'"), min = 0,
                             max = 1000, value = 750),
                 checkboxGroupInput("checkGroup", label = h3("Choisir les variables explicatives"),
                                    choices = c(colnames(gdata[,-21])),
                                    selected = c(colnames(gdata[,c(1,2,3)]))
                 )
               )

      ),
      tabPanel(
        "Machine Learning",
        mainPanel(
          h4("Le Modele"),
          hr(),
          verbatimTextOutput("mdl2"),
          hr(),
          conditionalPanel(
            condition = "input.meth == 'Reseaux de neurones(nnet)'",
            plotOutput("plot_rn"),
            hr(),
            plotOutput("neural", width = 1200 , height = 1500)
          ),
          conditionalPanel(
            condition = "input.meth == 'Arbre de decision'",
            plotOutput("arbre"),
            hr(),
            plotOutput("arbre2", width = 1200 , height = 1500)
          )
        ),
        sidebarPanel(
          selectInput("meth","Selectionner la methode",
                      choices = c("Arbre de decision","Reseaux de neurones(nnet)")),
          sliderInput("slider2", label = h3("Choisir la taille de 'Train Data'"), min = 0,
                      max = 1000, value = 750),
          checkboxGroupInput("checkGroup2", label = h3("Choisir les variables explicatives"),
                             choices = c(colnames(gdata[,-21])),
                             selected = c(colnames(gdata[,-21]))
          )
        )
      ),

      tabPanel(
        "Comparaison des modeles",
        mainPanel(
          h1("Comparaison des modeles pour 75% de 'Train Data'"),
          hr(),
          plotOutput("comparaison"),
          hr(),
          tableOutput("tab_comp")
        )
      )

    )),
  server = function(input, output) {

    output$hist_univariee = renderPlot({

      quali = gdata[,input$vquali]
      quanti = gdata[,input$vquanti]
      if(input$type == "Qualitative"){
        barplot(table(quali),xlab = names(quali),ylab = "Frequence",col = "#CC0066",main= paste("Plot de ",names(quali)))
        #plot_ly(y = ~table(quali) , type = "bar" , line = list(color = "yellow"))%>%layout(title = paste("Plot de ",names(quali)))
      }
      if(input$type == 'Quantitative'){
        boxplot(quanti,xlab = names(quanti),ylab = "Frequence",col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ",names(quanti)))
        #plot_ly(y = ~unlist(quanti) , type = "box" , line = list(color = "green"))%>%layout(title = paste("Boxplot de ",names(quanti)))
      }

    }
    )

    output$plot_univariee = renderPlot({

      quali = gdata[,input$vquali]
      quanti = gdata[,input$vquanti]
      if(input$type == "Qualitative"){
        plot(density(unlist(quali)), na.rm = TRUE, main = paste("Densite de ",names(quali)))
      }
      if(input$type == 'Quantitative'){
        plot(density(unlist(quanti), na.rm = TRUE), main = paste("Densite de ",names(quanti)))

      }

    }
    )

    output$desc_univariee = renderText({

      quali = gdata[,input$vquali]
      quanti = gdata[,input$vquanti]
      if(input$type == "Qualitative"){
        summary(quali)
      }
      else if(input$type == 'Quantitative'){
        summary(quanti)

      }

    }
    )

    output$box_bivariee = renderPlot({
      quali1 = gdata[,input$vquali1]
      quanti1 = gdata[,input$vquanti1]
      if(input$type1 == "Qualitative"){
        y = unlist(quali1)
        boxplot(y~Good,xlab = "Good",ylab = names(quali1),names=c("yes","no"),col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ","Good~",names(quali1)))
        #mosaicplot(Good ~ y,xlab = "Good",ylab = names(quali1),shade = TRUE, main = "Graphe en mosaique")
      }
      if(input$type1 == "Quantitative"){
        x = unlist(quanti1)
        boxplot(x~Good,xlab ="Good",ylab = names(quanti1),col = "#3399FF",border = "#cc0099",main = paste("Boxplot de ","Good~",names(quanti1)))

      }

    }
    )

    output$mos_bivariee = renderPlot({
      quali1 = gdata[,input$vquali1]
      if(input$type1 == "Qualitative"){
        y = unlist(quali1)
        mosaicplot(Good ~ y,xlab = "Good",ylab = names(quali1),shade = TRUE, main = "Graphe en mosaique")
      }
    }
    )

    output$bar_bivariee = renderPlot({
      quali1 = gdata[,input$vquali1]
      if(input$type1 == "Qualitative"){
        y = unlist(quali1)
        bartable = table(Good,y)
        barplot(bartable, col = c("tomato","lightskyblue") ,legend = c("Yes","No"), main = paste(names(quali1),"Selon Good"))
      }
    }
    )

    output$test_simple = renderTable({

      quanti1 = gdata[,input$vquanti1]
      if(input$type1 == 'Quantitative'){

        x1 = unlist(quanti1)
        kt = as.data.frame(unlist(t.test(x1~Good)))

      }

    }, rownames = T,colnames = F
    )

    output$res_bi = renderTable({
      if(input$type1 == "Qualitative"){
        quali1 = gdata[,input$vquali1]
        y = unlist(quali1)
        bartable = table(Good,y)
        as.data.frame(lprop(bartable, digits = 2, percent = TRUE))
      }
    }
    )

    output$mdl = renderText({
      form = paste("Good~", paste(c(input$checkGroup), collapse= "+"))
      modl = as.character(unlist(form))
    }
    )

    output$glm = renderTable({
      #gdata$Good[Good == 2] = 0
      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id = unlist(input$slider1)
      train_id = sample(1000,id)
      train_data = gdata[train_id,]
      test_data = gdata[-train_id,]
      form = paste("Good~", paste(c(input$checkGroup), collapse= "+"))
      Logit = glm(formula(form),data = train_data,family = "binomial")
      l = summary(Logit)
      k = as.data.frame(l$coefficients)
      k
    }, rownames = T,colnames = T
    )

    output$pred = renderPlot({
      #gdata$Good[Good == 2] = 0
      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id = unlist(input$slider1)
      train_id = sample(1000,id)
      train_data = gdata[train_id,]
      test_data = gdata[-train_id,]
      form = paste("Good~", paste(c(input$checkGroup), collapse= "+"))
      Logit = glm(formula(form),data = train_data,family = "binomial")
      ##prediction

      #calcul des indicateurs sur la base train
      prob_train = predict(Logit,train_data,type = "response")
      pred_train = prediction(prob_train,train_data$Good)
      perf_roc_train = performance(pred_train , measure = "tpr", x.measure = "fpr")
      auc_train = performance(pred_train, measure = "auc")

      #indicateur sur la base test
      prob_test = predict(Logit,test_data,type = "response")
      pred_test = prediction(prob_test,test_data$Good)
      perf_roc_test = performance(pred_test , measure = "tpr", x.measure = "fpr")
      auc_test = performance(pred_test, measure = "auc")
      at = unlist(auc_train@y.values[[1]])
      att = unlist(auc_test@y.values[[1]])
      precision=abs(auc_test@y.values[[1]]/auc_train@y.values[[1]])
      plot(perf_roc_train,col = "red",print.cutoffs.at=seq(0,1,by=0.25))
      abline(0,1,col="green",lty=3)
      par(new=TRUE)
      plot(perf_roc_test,col = "blue",print.cutoffs.at=seq(0,1,by=0.25))
      text(0.9,0.6,paste("Precision = ",round(precision,digits = 3)),col ="blue")
      text(0.9,0.4,paste("auc_train = ",as.numeric(at)),col="red")
      text(0.9,0.2,paste("auc_test = ",as.numeric(att)),col="blue")


    })

    output$mdl2 = renderText({
      form = paste("Good~", paste(c(input$checkGroup2), collapse= "+"))
      modl = as.character(unlist(form))
    }
    )

    output$arbre = renderPlot({
      #if(input$meth == 'Arbre de decision'){

      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id2 = unlist(input$slider2)
      train_id2 = sample(1000,id2)
      train_data2 = gdata[train_id2,]
      test_data2 = gdata[-train_id2,]
      form = paste("Good~", paste(c(input$checkGroup2), collapse= "+"))
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_arbre <- rpart(formula(form),data = train_data2,method = "class" ,control = mycontrol)

      ##prediction

      #calcul des indicateurs sur la base train
      prob_train2 = predict(model_arbre,train_data2,type = "prob")[,2]
      pred_train2 = prediction(prob_train2,train_data2$Good)
      perf_roc_train2 = performance(pred_train2 , measure = "tpr", x.measure = "fpr")
      auc_train2 = performance(pred_train2, measure = "auc")

      #indicateur sur la base test
      prob_test2 = predict(model_arbre,test_data2,type = "prob")[,2]
      pred_test2 = prediction(prob_test2,test_data2$Good)
      perf_roc_test2 = performance(pred_test2 , measure = "tpr", x.measure = "fpr")
      auc_test2 = performance(pred_test2, measure = "auc")
      at2 = unlist(auc_train2@y.values[[1]])
      att2 = unlist(auc_test2@y.values[[1]])
      plot(perf_roc_train2,col = "red",print.cutoffs.at=seq(0,1,by=0.25))
      abline(0,1,col="green",lty=3)
      par(new=TRUE)
      plot(perf_roc_test2,col = "blue",print.cutoffs.at=seq(0,1,by=0.25))
      text(0.9,0.4,paste("auc_train = ",as.numeric(at2)),col="red")
      text(0.9,0.2,paste("auc_test = ",as.numeric(att2)),col="blue")
      # }
    })

    output$arbre2 = renderPlot({

      # if(input$meth == 'Arbre de decision'){
      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id2 = unlist(input$slider2)
      train_id2 = sample(1000,id2)
      train_data2 = gdata[train_id2,]
      test_data2 = gdata[-train_id2,]
      form = paste("Good~", paste(c(input$checkGroup2), collapse= "+"))
      mycontrol = rpart.control(cp = 0, xval = 10)
      model_arbre <- rpart(formula(form),data = train_data2,method = "class" ,control = mycontrol)

      prp(model_arbre,type = 2 , extra = 1)
      #}
    })

    output$plot_rn = renderPlot({
      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id2 = unlist(input$slider2)
      train_id2 = sample(1000,id2)
      data_train2 = gdata[train_id2,]
      data_test2 = gdata[-train_id2,]
      form = paste("Good~", paste(c(input$checkGroup2), collapse= "+"))

      Neural<- nnet(data_train2$Good~.,data = data_train2,size=10,maxit=500,decay=.001, linout=F, trace = F)

      fitNeural_train <- predict(Neural,newdata=data_train2)
      prednn_train = prediction( fitNeural_train, data_train2$Good)
      perfnn_train = performance(prednn_train, "tpr", "fpr")
      auc_train_nn = performance(prednn_train,measure="auc")
      fitNeural <- predict(Neural, newdata=data_test2)
      prednn = prediction(fitNeural, data_test2$Good)
      perfnn <- performance(prednn, "tpr", "fpr")
      auc_test_nn=performance(prednn,measure="auc")
      plot(perfnn_train,col="red",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      par(new=TRUE)
      plot(perfnn,col="green",print.cutoffs.at=seq(0,1,by=0.25),lwd=3)
      abline(0,1,lty=3)
      text(x=0.9,y=0.2,paste("auc(train)=",round(auc_train_nn@y.values[[1]],digits = 3)),col ="red")
      text(x=0.9,y=0.1,paste("auc(test)=",round(auc_test_nn@y.values[[1]],digits = 3)),col ="green")

    })

    output$neural = renderPlot({
      gdata[,-c(2,5,13,21)] = as.factor(unlist(gdata[,-c(2,5,13,21)]))
      attach(gdata)
      id2 = unlist(input$slider2)
      train_id2 = sample(1000,id2)
      data_train2 = gdata[train_id2,]
      data_test2 = gdata[-train_id2,]
      form = paste("Good~", paste(c(input$checkGroup2), collapse= "+"))

      Neural<- nnet(data_train2$Good~.,data = data_train2,size=10,maxit=500,decay=.001, linout=F, trace = F)
      wts <- neuralweights(Neural)
      struct <- wts$struct
      wts <- unlist(wts$wts)
      plotnet(Neural, struct=struct)

    })

    output$comparaison = renderPlot({


      plot(perf_roc_test, col='blue', lty=1, main='ROCs: Performance du Modele: Comparaison')
      plot(perf_roc_test_ar, col='gold',lty=2, add=TRUE);
      plot(perfnn, col='dark orange',lty=3, add=TRUE);
      legend(0.6,0.5,
             c('m1:Regression Logistique','m2: Arbre de Decision',
               'm3: Reseaux de Neurones'),
             col=c('blue','gold', 'dark orange'),
             lwd=3)
    })

    output$tab_comp = renderTable({

      m1_AUROC = round(performance(pred_test, measure = "auc")@y.values[[1]]*100, 2)
      m1_KS = round(max(attr(perf_roc_test,'y.values')[[1]]-attr(perf_roc_test,'x.values')[[1]])*100, 2)
      m1_Gini = (2*m1_AUROC - 100)

      m2_AUROC <- round(performance(pred_test_ar, measure = "auc")@y.values[[1]]*100, 2)
      m2_KS <- round(max(attr(perf_roc_test_ar,'y.values')[[1]]-attr(perf_roc_test_ar,'x.values')[[1]])*100, 2)
      m2_Gini <- (2*m2_AUROC - 100)

      m3_AUROC <- round(performance(prednn, measure = "auc")@y.values[[1]]*100, 2)
      m3_KS <- round(max(attr(perfnn,'y.values')[[1]] - attr(perfnn,'x.values')[[1]])*100, 2)
      m3_Gini <- (2*m3_AUROC - 100)

      models = c('m1:Regression Logistique', 'm2:Arabre de decesion','m3:Reseaux de neurones')
      models_AUC <- c(m1_AUROC, m2_AUROC, m3_AUROC)
      models_KS = c(m1_KS, m2_KS, m3_KS)
      models_Gini <- c(m1_Gini, m2_Gini, m3_Gini)
      model_performance_metric <- as.data.frame(cbind(models, models_AUC, models_KS, models_Gini))
      colnames(model_performance_metric) = c("Modele", "AUC", "KS", "Gini")
      model_performance_metric

    })
  }
)

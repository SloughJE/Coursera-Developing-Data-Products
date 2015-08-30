
# code before shinyserver function, it gets called ONCE.
# code inside gets executed every time user 'submits' 
#setwd("~/Desktop/Courses/Coursera/Data Products/Project/Pima")
library(MASS)
library(caret)
library(shiny)
library(ggthemes)
library(shinyapps)
library(e1071)
library(gridGraphics)

pima=read.table("http://mlearn.ics.uci.edu/databases/pima-indians-diabetes/pima-indians-diabetes.data",sep=",",
                col.names=c("times_pregnant","plasma_glucose","diastolic_BP","tri_skin_fold","insulin","BMI","d_ped","age","diagnosis"))
# charts <- subset(pima, pima$insulin != 0 & pima$plasma_glucose != 0  & pima$age !=0 & pima$BMI !=0)
# 
# charts$diagnosis=factor(charts$diagnosis,labels=c("0","1"))
pima$diagnosis=factor(pima$diagnosis,labels=c("positive","negative"))

validData <- subset(pima, pima$insulin != 0 & pima$plasma_glucose != 0  & pima$age !=0 & pima$BMI !=0)

set.seed(1234)

train_part = createDataPartition(validData$diagnosis, p = 0.75, list = FALSE)
training = validData[train_part, ]
testing = validData[-train_part, ]

set.seed(1234)
diagn<-train(diagnosis~times_pregnant+plasma_glucose+diastolic_BP+tri_skin_fold+insulin+BMI+d_ped+age,
             data=training,method="glm",family="binomial")

predict(diagn,newdata=testing,type="prob")

glm.mod=glm(diagnosis~times_pregnant+plasma_glucose+diastolic_BP+tri_skin_fold+insulin+BMI+d_ped+age,
               data=training,family="binomial")
# summary(glm.mod)
# predict(diagn,newdata=testing,type="prob")

diabetesRisk <- function(x1,x2,x3,x4,x5,x6, x7, x8){
  values=as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8))
  colnames(values )= c("times_pregnant","plasma_glucose","diastolic_BP","tri_skin_fold",
                       "insulin","BMI","d_ped","age")
  pred=predict(diagn, newdata=values,type="prob")[[2]]
  
  cat(pred)}

#diabetesRisk(0,200,2,30,100,28,.6,35)


imp=varImp(diagn)
imp.df=data.frame(imp$importance)

imp.df$names <- rownames(imp.df)

colnames(imp.df)=c("Importance","Variable")
imp.sort =  imp.df[order(-imp.df$Importance),] 

imp.sort = transform(imp.sort, 
                     Variable = reorder(Variable, Importance))

VIP=ggplot(data=imp.sort, aes(x=Variable, y=Importance)) + 
  ylab("Importance")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.6)+ 
  coord_flip()+theme_few() +
  scale_x_discrete(breaks=c("plasma_glucose", "BMI", "times_pregnant","age","insulin","d_ped",
                            "diastolic_BP","tri_skin_fold"),labels=c("plasma glucose", "BMI", 
                                                                     "# of pregnancies","age","serum insulin","diabetes pedigree function","diastolic blood pressure","triceps skin-fold"))

shinyServer( function(input, output) {
  
  output$prediction <- renderPrint({diabetesRisk(input$times_pregnant,input$plasma_glucose,input$diastolic_BP,input$tri_skin_fold,input$insulin,input$BMI,input$d_ped,input$age)}) 
  
  output$VarImpPlot <- renderPlot({VIP})
  
# output$var_plot <- renderPlot({(replayPlot(glucose))})

})

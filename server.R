
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



# logi.hist.plot.better=function (independ, depend, logi.mod = 1, type = "dit", boxp = TRUE, 
#                                 rug = FALSE, ylabel = "Probability", ylabel2 = "", 
#                                 xlabel = "", main = "", las.h = 1, counts = FALSE, ...) 
# {
#   logi.scater <- function(independ, depend, scater = "n", x.lab = xlabel, 
#                           las = las.h) {
#     plot(independ, depend, cex = 1, type = scater, ylab = ylabel, 
#          xlab = x.lab, main = mainlabel, cex.lab = 1.2, las = las,yaxt="n")
#     axis(2, at=c(0,1),labels=c("negative","positive"), las=2)
#     axis(4)
#     mtext(4, ylab="frequency") 
#     
#   }
#   logi.rug <- function(independ, depend, pch.rug = 16, cex.rug = 1) {
#     points(independ, depend, pch = pch.rug, cex = cex.rug)
#   }
#   logi.box <- function(independ, depend, col.box = "grey", 
#                        x.lab = xlabel, las = las.h) {
#     plot(independ, depend, cex = 1, type = "n", ylim = c(-0.1, 
#                                                          1.1),ylab = ylabel2, xlab = x.lab, cex.lab = 1.2,
#          
#          las = las)
#     axis(4, at=c(0,1),labels=c("negative","positive"), col.axis="black", las=2,cex.axis=.8)
#     
#     indep.1 <- independ[depend == 1]
#     indep.0 <- independ[depend == 0]
#     boxplot(indep.1, horizontal = TRUE, add = TRUE, at = 1.05, 
#             boxwex = 0.1, col = col.box, notch = TRUE,outline=FALSE)
#     boxplot(indep.0, horizontal = TRUE, add = TRUE, at = -0.05, 
#             boxwex = 0.1, col = col.box, notch = TRUE,outline=FALSE)
#   }
#   logi.curve <- function(independ, depend, mod = logi.mod, 
#                          col.cur = "indianred", lwd.cur = 5) {
#     if (mod == 1) 
#       mod3 <- glm(depend ~ independ, family = binomial)
#     if (mod == 2) 
#       mod3 <- glm(depend ~ independ + I(independ^2), family = binomial)
#     x.new <- seq(min(independ), max(independ), len = 100)
#     y.new <- predict(mod3, data.frame(independ = x.new), 
#                      type = "response")
#     lines(x.new, y.new, lwd = lwd.cur, col = col.cur)
#   }
#   logi.dit <- function(independ, depend, cex.p = 1, pch.dit = 1, 
#                        incre = 0.02) {
#     indep.0 <- independ[depend == 0]
#     indep.1 <- independ[depend == 1]
#     uni.plot.0 <- function(x) length(which(indep.0 == x))
#     uni.plot.1 <- function(x) length(which(indep.1 == x))
#     cosa.0 <- apply(as.matrix(unique(indep.0)), 1, uni.plot.0)
#     cosa.1 <- apply(as.matrix(unique(indep.1)), 1, uni.plot.1)
#     points(independ, depend, pch = pch.dit, cex = cex.p)
#     for (i in 1:max(cosa.0)) {
#       for (j in 1:i) {
#         points(unique(indep.0)[which(cosa.0 == i + 1)], 
#                rep(0 + incre * j, length(which(cosa.0 == i + 
#                                                  1))), pch = pch.dit, cex = cex.p)
#       }
#     }
#     for (i in 1:max(cosa.1)) {
#       for (j in 1:i) {
#         points(unique(indep.1)[which(cosa.1 == i + 1)], 
#                rep(1 - incre * j, length(which(cosa.1 == i + 
#                                                  1))), pch = pch.dit, cex = cex.p)
#       }
#     }
#   }
#   logi.hist <- function(independ, depend, scale.hist = 5, col.hist = "blue", 
#                         count.hist = TRUE, intervalo = 0, las.h1 = las.h) {
#     h.br <- hist(independ, plot = FALSE)$br
#     if (intervalo > 0) 
#       h.br <- seq(from = range(h.br)[1], to = range(h.br)[2], 
#                   by = intervalo)
#     h.x <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$mid
#     h.y0 <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$counts
#     h.y1 <- hist(independ[depend == 1], breaks = h.br, plot = FALSE)$counts
#     h.y0n <- h.y0/(max(c(h.y0, h.y1)) * scale.hist)
#     h.y1n <- 1 - h.y1/(max(c(h.y0, h.y1)) * scale.hist)
#     for (i in 1:length(h.y0n)) {
#       if (h.y0n[i] > 0) 
#         polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
#                 c(0, rep(h.y0n[i], 2), 0), col = col.hist)
#     }
#     for (i in 1:length(h.y1n)) {
#       if (h.y1n[i] < 1) 
#         polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
#                 c(h.y1n[i], 1, 1, h.y1n[i]), col = col.hist)
#     }
#     if (counts == TRUE) 
#       for (i in 1:length(h.x)) {
#         text(h.x[i], h.y1n[i], h.y1[i], cex = 1, pos = 1)
#         text(h.x[i], h.y0n[i], h.y0[i], cex = 1, pos = 3)
#       }
#     axis.hist <- function(h.y0, h.y1, scale.hist, las = las.h1) {
#       tope <- max(c(h.y0, h.y1))
#       label.down <- c(0, (ceiling(tope/10)) * 5, (ceiling(tope/10)) * 
#                         10)
#       label.up <- c((ceiling(tope/10)) * 10, (ceiling(tope/10)) * 
#                       5, 0)
#       at.down <- label.down/(tope * scale.hist)
#       at.up <- 1 - (label.up/(tope * scale.hist))
#       at.hist <- c(at.down, at.up)
#       label.hist <- c(label.down, label.up)
#       axis(side = 4, at = at.hist, labels = label.hist, 
#            las = las)
#       mtext(ylabel2, side = 4, line = 2, cex = 1.2)
#     }
#     axis.hist(h.y0, h.y1, scale.hist)
#     axis(side = 2, las = las.h1)
#   }
#   old.mar <- par()$mar
#   par(mar = c(5.1, 4.1, 4.1, 4.1))
#   if (boxp == TRUE) 
#     logi.box(independ, depend)
#   if (boxp == FALSE) 
#     logi.scater(independ, depend)
#   if (type != "dit") 
#     logi.hist(independ, depend, ...)
#   if (rug == TRUE) 
#     logi.rug(independ, depend)
#   logi.curve(independ, depend)
#   if (type == "dit") 
#     logi.dit(independ, depend)
#   par(mar = old.mar)
# }
# 
# #function for pregnancies logi.hist.plot
# # 
# # logi.hist.plot.preg=function (independ, depend, logi.mod = 1, type = "dit", boxp = TRUE, 
# #                               rug = FALSE, ylabel = "Probability", ylabel2 = "", 
# #                               xlabel = "", main = "", las.h = 1, counts = FALSE, ...) 
# {
#   logi.scater <- function(independ, depend, scater = "n", x.lab = xlabel, 
#                           las = las.h) {
#     plot(independ, depend, cex = 1, type = scater, ylab = ylabel, 
#          xlab = x.lab, main = mainlabel, cex.lab = 1.2, las = las,yaxt="n")
#     axis(2, at=c(0,1),labels=c("negative","positive"), las=2)
#     axis(4)
#     mtext(4, ylab="frequency") 
#     
#   }
#   logi.rug <- function(independ, depend, pch.rug = 16, cex.rug = 1) {
#     points(independ, depend, pch = pch.rug, cex = cex.rug)
#   }
#   logi.box <- function(independ, depend, col.box = "grey", 
#                        x.lab = xlabel, las = las.h) {
#     plot(independ, depend, cex = 1, type = "n", ylim = c(-0.1, 
#                                                          1.1),ylab = ylabel2, xlab = x.lab, cex.lab = 1.2,
#          
#          las = las)
#     axis(4, at=c(0,1),labels=c("negative","positive"), col.axis="black", las=2,cex.axis=.8)
#     mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
#     mtext("predicted probability",side=2,line=2.5,cex=1.2) 
#     title("Number of Pregnancies vs. Predicted Probability of Diabetes")
#     legend(14,.2,"prediction",
#            lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
#     
#     indep.1 <- independ[depend == 1]
#     indep.0 <- independ[depend == 0]
#     boxplot(indep.1, horizontal = TRUE, add = TRUE, at = 1.05, 
#             boxwex = 0.1, col = col.box, notch = TRUE,outline=FALSE)
#     boxplot(indep.0, horizontal = TRUE, add = TRUE, at = -0.05, 
#             boxwex = 0.1, col = col.box, notch = TRUE,outline=FALSE)
#   }
#   logi.curve <- function(independ, depend, mod = logi.mod, 
#                          col.cur = "indianred", lwd.cur = 5) {
#     if (mod == 1) 
#       mod3 <- glm(depend ~ independ, family = binomial)
#     if (mod == 2) 
#       mod3 <- glm(depend ~ independ + I(independ^2), family = binomial)
#     x.new <- seq(min(independ), max(independ), len = 100)
#     y.new <- predict(mod3, data.frame(independ = x.new), 
#                      type = "response")
#     lines(x.new, y.new, lwd = lwd.cur, col = col.cur)
#   }
#   logi.dit <- function(independ, depend, cex.p = 1, pch.dit = 1, 
#                        incre = 0.01) {
#     indep.0 <- independ[depend == 0]
#     indep.1 <- independ[depend == 1]
#     uni.plot.0 <- function(x) length(which(indep.0 == x))
#     uni.plot.1 <- function(x) length(which(indep.1 == x))
#     cosa.0 <- apply(as.matrix(unique(indep.0)), 1, uni.plot.0)
#     cosa.1 <- apply(as.matrix(unique(indep.1)), 1, uni.plot.1)
#     points(independ, depend, pch = pch.dit, cex = cex.p)
#     for (i in 1:max(cosa.0)) {
#       for (j in 1:i) {
#         points(unique(indep.0)[which(cosa.0 == i + 1)], 
#                rep(0 + incre * j, length(which(cosa.0 == i + 
#                                                  1))), pch = pch.dit, cex = cex.p)
#       }
#     }
#     for (i in 1:max(cosa.1)) {
#       for (j in 1:i) {
#         points(unique(indep.1)[which(cosa.1 == i + 1)], 
#                rep(1 - incre * j, length(which(cosa.1 == i + 
#                                                  1))), pch = pch.dit, cex = cex.p)
#       }
#     }
#   }
#   logi.hist <- function(independ, depend, scale.hist = 5, col.hist = "blue", 
#                         count.hist = TRUE, intervalo = 0, las.h1 = las.h) {
#     h.br <- hist(independ, plot = FALSE)$br
#     if (intervalo > 0) 
#       h.br <- seq(from = range(h.br)[1], to = range(h.br)[2], 
#                   by = intervalo)
#     h.x <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$mid
#     h.y0 <- hist(independ[depend == 0], breaks = h.br, plot = FALSE)$counts
#     h.y1 <- hist(independ[depend == 1], breaks = h.br, plot = FALSE)$counts
#     h.y0n <- h.y0/(max(c(h.y0, h.y1)) * scale.hist)
#     h.y1n <- 1 - h.y1/(max(c(h.y0, h.y1)) * scale.hist)
#     for (i in 1:length(h.y0n)) {
#       if (h.y0n[i] > 0) 
#         polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
#                 c(0, rep(h.y0n[i], 2), 0), col = col.hist)
#     }
#     for (i in 1:length(h.y1n)) {
#       if (h.y1n[i] < 1) 
#         polygon(c(rep(h.br[i], 2), rep(h.br[i + 1], 2)), 
#                 c(h.y1n[i], 1, 1, h.y1n[i]), col = col.hist)
#     }
#     if (counts == TRUE) 
#       for (i in 1:length(h.x)) {
#         text(h.x[i], h.y1n[i], h.y1[i], cex = 1, pos = 1)
#         text(h.x[i], h.y0n[i], h.y0[i], cex = 1, pos = 3)
#       }
#     axis.hist <- function(h.y0, h.y1, scale.hist, las = las.h1) {
#       tope <- max(c(h.y0, h.y1))
#       label.down <- c(0, (ceiling(tope/10)) * 5, (ceiling(tope/10)) * 
#                         10)
#       label.up <- c((ceiling(tope/10)) * 10, (ceiling(tope/10)) * 
#                       5, 0)
#       at.down <- label.down/(tope * scale.hist)
#       at.up <- 1 - (label.up/(tope * scale.hist))
#       at.hist <- c(at.down, at.up)
#       label.hist <- c(label.down, label.up)
#       axis(side = 4, at = at.hist, labels = label.hist, 
#            las = las)
#       mtext(ylabel2, side = 4, line = 2, cex = 1.2)
#     }
#     axis.hist(h.y0, h.y1, scale.hist)
#     axis(side = 2, las = las.h1)
#   }
#   old.mar <- par()$mar
#   par(mar = c(5.1, 4.1, 4.1, 4.1))
#   if (boxp == TRUE) 
#     logi.box(independ, depend)
#   if (boxp == FALSE) 
#     logi.scater(independ, depend)
#   if (type != "dit") 
#     logi.hist(independ, depend, ...)
#   if (rug == TRUE) 
#     logi.rug(independ, depend)
#   logi.curve(independ, depend)
#   if (type == "dit") 
#     logi.dit(independ, depend)
#   
#   recordPlot()
# }
# 

#png("glucose.png",width=3000,height=1400,res=200)

# logi.hist.plot.better(charts$plasma_glucose,charts$diagnosis,
#                       boxp=TRUE,type="dit",col="grey",xlab="plasma glucose concentration")
# mtext("diabetes diagnosis",side=4,line=-1,cex=1.2) 
# mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# title("Plasma Glucose vs. Predicted Probability of Diabetes")
# legend(170,.2,"prediction",
#        lty=1,lwd=4,col="indianred",bty = "n",cex=1)
# #dev.off()
# 
# glucose=recordPlot()

# 
# # BMI
# 
# # logi.hist.plot.better(charts$BMI,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="body mass index (kg/(height in m)^2)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("BMI vs. Predicted Probability of Diabetes")
# # legend(60,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # BMI=recordPlot()
# # 
# # # Pedigree
# # logi.hist.plot.better(charts$d_ped,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="body mass index (kg/(height in m)^2)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("Diabetes Pedigree Function vs. Predicted Probability of Diabetes")
# # legend(2.1,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # dped=recordPlot()
# # 
# # # times_pregnant need to change
# # 
# # pregnancies=logi.hist.plot.preg(charts$times_pregnant,charts$diagnosis,
# #                                 boxp=TRUE,type="dit",col="grey",xlab="number of pregnancies")
# # 
# # # age
# # 
# # logi.hist.plot.better(charts$age,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="age (years)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("Age vs. Predicted Probability of Diabetes")
# # legend(72,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # 
# # age=recordPlot()
# # # insulin
# # 
# # logi.hist.plot.better(charts$insulin,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="insulin (mu U/ml)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("Insulin vs. Predicted Probability of Diabetes")
# # legend(740,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # insulin=recordPlot()
# # # triceps skin fold
# # 
# # 
# # logi.hist.plot.better(charts$tri_skin_fold,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="triceps skin-fold (mm)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("Triceps Skin-Fold  vs. Predicted Probability of Diabetes")
# # legend(55,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # triceps=recordPlot()
# # #
# # 
# # logi.hist.plot.better(charts$diastolic_BP,charts$diagnosis,
# #                       boxp=TRUE,type="dit",col="grey",xlab="diastolic blood pressure (mm Hg)")
# # mtext("diabetes diagnosis",side=4,line=1,cex=1.2) 
# # mtext("predicted probability",side=2,line=2.5,cex=1.2) 
# # title("Diastolic Blodd Pressure  vs. Predicted Probability of Diabetes")
# # legend(95,.2,"prediction",
# #        lty=1,lwd=4,col="indianred",bty = "n",cex=.7)
# # BP=recordPlot()
# # 
# # 
# # plot.type= function(x){
# #   
# #   if (x == 1){ 
# #     print(glucose)}
# #   
# #   if (x == 2)
# #     print(BMI)
# #   if (x == 3)
# #     print(dped)
# #   if (x == 4)
# #     print(pregnancies)
# #   if (x == 5)
# #     print(age)
# #   if (x == 6)
# #     print(insulin)
# #   if (x == 7)
# #     print(triceps)
# #   if (x == 8)
# #     print(BP)
# # }
# 

shinyServer( function(input, output) {
  
  output$prediction <- renderPrint({diabetesRisk(input$times_pregnant,input$plasma_glucose,input$diastolic_BP,input$tri_skin_fold,input$insulin,input$BMI,input$d_ped,input$age)}) 
  
  output$VarImpPlot <- renderPlot({VIP})
  
# output$var_plot <- renderPlot({(replayPlot(glucose))})

})

tt=TEBATRANSF[,-1]
tt$cancel=as.factor(tt$cancel) #importante (não rodou sem isto??!!)
tt=as.data.frame(tt)
set.seed(123)
flag=sample(1:2000, 1000)
cnt=rpart.control(maxdepth = 2, minsplit = 100) 
#às vezes rodando com maxdepth=1 deu problema, passar para 2
lrn=tt[flag,]
tst=tt[-flag,]
lrn=as.data.frame(lrn);tst=as.data.frame(tst) #importante

tt.adaboost <- boosting(cancel ~ ., data = lrn, mfinal = 100, control = cnt)
head(tt.adaboost$prob, 10)
head(tt.adaboost$class,10)
tt.adaboost$trees  #imprime as árvores intermediárias


#importancia das vars (critério: Gini)
round(sort(tt.adaboost$importance ,decreasing = T),2)
barplot(sort(tt.adaboost$importance, decreasing = TRUE), 
        main = "Variables Relative Importance", col = "red", 
        horiz = TRUE, las = 1, cex.names = 1, xlim = c(0, 40))

#análise de overfit
errorevol.lrn <- errorevol(tt.adaboost,lrn)
errorevol.tst <- errorevol(tt.adaboost, tst) 
plot(errorevol.lrn$error, type = "l", main="Boosting error versus number of trees",
     xlab="Iterations",ylab="Error",col="red",lwd = 2)
grid(col=3)
lines(errorevol.tst[[1]], cex = 0.5, col = "blue", lty = 1, lwd = 2)
legend("topright", c("lrn", "tst"), col = c("red", "blue"), lty = 1, lwd = 2)
abline(h = min(errorevol.tst[[1]]), col = "red", lty = 2, lwd = 2)
abline(h = min(errorevol.lrn[[1]]), col = "blue", lty = 2, lwd = 2)
min(errorevol.tst$error)
which(errorevol.tst$error<.1800001) # pois deve haver algumas casas decimais nao impressas
#considerando 23 iterações
pred.lrn <- predict(tt.adaboost, newdata = lrn, newmfinal = 23 ) 
head(pred.lrn$prob)
pred.lrn$confusion
pred.lrn$error
pred.tst <- predict(tt.adaboost, newdata = tst,newmfinal = 23)
pred.tst$confusion
pred.tst$error 
tst$psim=pred.tst$prob[,2]

HMeasure(tst$cancel,tst$psim)$metrics

set.seed(123)
tt.adaboost.cv <- boosting.cv(cancel ~ ., data = tt, mfinal = 100, control = cnt)
tt.adaboost.cv$confusion
tt.adaboost.cv$error

qq=onlineretailAmostra 
qq$InvoiceNo=as.numeric(qq$InvoiceNo)
qq$StockCode=as.factor(qq$StockCode)
qq$Description=as.factor(qq$Description)
qq=qq[complete.cases(qq),] #eliminar transações em branco
qq.order=qq[sort(qq$CustomerID),]

library(plyr)

##############################################################################################
#Agrupando cestas por cliente, mesmo que tenha diferentes invoices
itemList =ddply(qq,c("CustomerID"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID=NULL  #desnecessário
dim(itemList)

#gerando csv para transformação posterior
write.csv(itemList,"customer_basket.csv", quote = FALSE, row.names = TRUE)
#gerando a base de dados no formato transactions
tr.itens=read.transactions("customer_basket.csv",format = "basket", sep = ",")
summary(tr.itens)

#######################################################################################

#Agrupando cestas por invoice; um cliente pode aparecer mais de uma vez
transList =ddply(qq,c("InvoiceNo"), 
                function(df1)paste(df1$Description, 
                                   collapse = ","))
dim(transList)
transList$CustomerID=NULL  #desnecessário

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)




 
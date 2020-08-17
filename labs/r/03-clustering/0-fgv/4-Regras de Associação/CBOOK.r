library(readxl)
CHARLESBOOK4000editado <- read_excel("4-Regras de Associação/DADOS PAX   Association Rules  2020  07 02.xlsx", sheet="CBC4000editado")
CHARLESBOOK4000editado

#considerando as colunas de produtos
cbc=CHARLESBOOK4000editado[,8:16]  #sem considerar o livro alvo Florence
head(cbc,10)
cbc=cbc[rowSums(cbc)>0,]
which(rowSums(cbc)==0)
#criando uma matriz binária indicando se comprou ou não (sem considerar quantos comprou)
cbc2=ifelse(cbc>0,1,0)
head(cbc2)

#antes de usar a base de dados devemos transforma-la em "matrix'
cbc.mat=as.matrix(cbc2)
head(cbc.mat)
class(cbc.mat)

# gerando a matriz de transações para pode rodar o algoritmo
install.packages(c("arules"))
library(arules)
cbc.tr=as(cbc.mat,"transactions")

inspect(cbc.tr) 
summary(cbc.tr)

#analisando os produtos graficamente
itemFrequency(cbc.tr) # suporte de cada item 
itemFrequencyPlot(cbc.tr, col=rainbow(2)) #não ordena
itemFrequencyPlot(cbc.tr,topN=9, main="item support", col=rainbow(2));grid(col=3)

#geração das regras
rules=apriori(data = cbc.tr, parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=7, maxtime=25, target="rules" ))
#maxlen: an integer value for the maximal number of items per item set (default: 10 items)
#maxtime: time limit in seconds for checking subsets. maxtime=0 disables the time limit. (default: 5 seconds)
#minlen=2 evita regras com LHS vazios
rules #dá o número de regras
inspect(sort(rules, by="lift")) #ordenado pelo lift


?install.packages
install.packages(c("arulesViz"),dependencies = )
library("devtools")
install_github("mhahsler/arulesViz")

library(arulesViz)
plot(rules, col=rainbow(3)) 

#poderíamos criar um conjunto de regras já ordenado. 
rules.sorted=sort(rules, by="confidence")
inspect(rules.sorted[1:20])

#obtendo regras redundantes
# A rule is more general if it has the same RHS but one or more items removed from the LHS ("menorLHS").
#A rule is redundant if a more general rules with the same or a higher confidence exists.
#A more specific rule is redundant if it is only equally or even less predictive than a more general rule
# a rule X -> Y is redundant if for some X' (subset of) X, conf(X' -> Y) >= conf(X -> Y)

inspect(rules.sorted[is.redundant(rules.sorted)]) # output em branco se nao houver regras redundantes
rules.pruned=rules.sorted[!is.redundant(rules.sorted, measure="confidence")];rules.pruned
inspect(rules.pruned)
#Coverage (also called cover or LHS-support) is the support of the left-hand-side of the rule, i.e.,
#It represents a measure of to how often the rule can be applied.

#focando em determinado produto ou itemset

rules.child=apriori(data = cbc.tr, 
              parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=5, maxtime=25, target="rules"), 
              appearance = list(rhs="ChildBks"))
inspect(sort(rules.child, by="confidence"))
inspect(rules.child[is.redundant(rules.child)]) 


#outros gráficos 

inspect(rules.sorted[1:10])
plot(rules.sorted[1:10], method="grouped")


sink("regras.txt") #cria arquivo .txt em branco; na realidade direciona todo o output para um arquivo txt

inspect(rules.sorted) #prenche o arquivo em branco

sink( ) #sai do modo sink (alternativa sink(file = NULL)), output volta para o painel de output

write(rules.sorted, file="CBC_rules_sorted.csv", sep=",")
inspect(rules.sorted) #prenche o arquivo em branco

#trabalhando apenas com os itemsets
is=apriori(data = cbc.tr, parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=7, maxtime=25, target="frequent itemsets" ))
inspect(is)

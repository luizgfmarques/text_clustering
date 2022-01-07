#Loading Database and Data Wrangling it
source("Script 01 - Data Wrangling.R")

#filtering the database for tests 
bd_CDs_031<-bd_CDs
bd_CDs_031<-filter(bd_CDs,cta=="SJK031 - Junção de Asa")
bd_CDs_031<-filter(bd_CDs_031,modo_falha=="Interferência (Atrito)" | modo_falha=="Metalização Faltando/Incorreta"| modo_falha=="Cravação")

#New data wrangling, this time focus in text cleaning
source("Script 02 - Text Clustering.R")

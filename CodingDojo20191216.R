require(tidyr)
require(dplyr)
require(readr)
require(purrr)
require(janitor)

# Lecture des données
inputTest<- read_delim("CD2019test.txt", 
                                ";", escape_double = FALSE, col_names = FALSE, 
                                trim_ws = TRUE)
inputReal<- read_delim("CD2019.txt", 
                       ";", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)

# inputTest->input
inputReal->input

# Calcul du coût moyen par événement
input %>% mutate(k=row_number()) %>% rowwise() %>% 
  mutate(nbParticipants=ncol(input[k,] %>% remove_empty())-2) %>% 
  mutate(avgPrice=X2/nbParticipants)-> input

# Vecteur des vacanciers participants
n<-ncol(input)
vacanciers<-input$X1
for( k in 3:n){
  var<-paste0("X",k)
  vacanciers<-c(vacanciers,input[,var])
}
vacanciers<-unique(vacanciers) %>% unlist
vacanciers<-vacanciers[!is.na(vacanciers)]
vacanciers<-as.character(unique(vacanciers))

# Tableau du budget initialisé à zéro
budget<-data.frame(vacancier=vacanciers)
budget$credit<-0
#budget$debit<-0 # inutile, crédit négatif pour les débiteurs

# Calcul du budget à la fin des vacances
process<-function(line){
  budget2<-budget
  budget2$credit[budget2$vacancier==line$X1]<-budget2$credit[budget2$vacancier==line$X1]+line$X2
  debiteurs<-(line[,3:n] %>% gather)$value
 # budget2$debit[budget2$vacancier %in% debiteurs] <- budget2$debit[budget2$vacancier %in% debiteurs] + line$avgPrice
  budget2$credit[budget2$vacancier %in% debiteurs] <- budget2$credit[budget2$vacancier %in% debiteurs] - line$avgPrice
  budget<<-budget2
}

# input %>% map(process)
# Serait bien plus beau mais j'ai mal choisi mes paramètres pour la fonction process()
for( k in 1:nrow(input)){
  process(line = input[k,])
}

budget %>% mutate(credit=round(credit,2))->budget
# Contrôle
sum(budget$credit)

# Calculs des paiements à réaliser

paiements<-data.frame(de=c(),pour=c(),montant=c())

addPaiement<-function(budget){
  nbVac<-nrow(budget)
  budget %>% arrange(credit) %>% mutate(vacancier=as.character(vacancier)) -> budget
  de<-budget$vacancier[1]
  pour<-budget$vacancier[nbVac]
  montant<-min(abs(budget[1,2]),abs(budget[nbVac,2]))
  paiements<<-bind_rows(paiements,data.frame(de,pour,montant))
  budget[1,2]<-budget[1,2]+montant  
  budget[nbVac,2]<-budget[nbVac,2]-montant 
  return(budget)
}

budget
paiements
# addPaiement(budget)

# Créances recouvrées dans la limite de limCentimes
limCentimes<-10
while(any(budget$credit>limCentimes/100)){
  budget<-addPaiement(budget)
}

# Résultats
paiements
budget

install.packages("dplyr")
require(dplyr)

# Lecture ----
library(readr)
input2 <- read_csv("input2.txt", col_names = FALSE)
View(input2)

input2 %>% mutate_all(as.logical)->input

inputMatrix<-as.matrix(input)

# Piste finalement inutilisée ----
isNeighbour<-function(a,b,x,y){
  (abs(a-x)<=1 & abs(b-y)==1) |
  (abs(a-x)==1 & (b-y)==0) 
}
isNeighbour(17,28,18,27)
isNeighbour(17,28,17,28)
isNeighbour(17,5,18,5)
isNeighbour(1,28,1,27)
isNeighbour(17,30,18,27)


# Mise sous forme de data.frame ----
x<-rep(1:28,times=28)
y<-rep(1:28,each=28)
inputMatrix[1,3]
colnames(inputMatrix) <- NULL

data<-data.frame(x,y,robert=TRUE)

for(i in 1:28){
  for(j in 1:28){
    data$robert[data$x==i&data$y==j]<-inputMatrix[i,j]
  }
}

# Calcul de l'étape suivante ----
nextStep<-function(a,b){
  data$robert[data$x==a & data$y==b] -> isAlive
  neighbours<-data[abs(data$x-a) <2 & abs(data$y-b) <2,]
  s<-sum(neighbours$robert)
  return(s==3 | (s==4 & isAlive))
}

nextStepForDF<-function(x,y,robert){
  a<-x
  b<-y
  robert -> isAlive
  neighbours<-data[abs(data$x-a) <2 & abs(data$y-b) <2,]
  s<-sum(neighbours$robert)
  newRobert<-(s==3 | (s==4 & isAlive))
  data.frame(x=x,y=y,robert=newRobert)
}

#Tests
nextStep(4,6)
nextStep(4,7)
nextStep(4,8)
nextStep(4,9)

nextStepForDF(4,8,FALSE)

age1<-pmap_df(data,nextStepForDF)

# Un data.frame à chaque âge ----
data->age0
for(year in 1:100){
  data<-get(paste0("age",year-1))
  assign(x = paste0("age",year),pmap_df(data,nextStepForDF))
}


# Représentations graphiques ----
install.packages("ggformula")
require(ggformula)
# Départ
gf_tile((-x)~y,fill=~robert,data=age0)
# Arrivée
gf_tile((-x)~y,fill=~robert,data=age100)

# Dynamique
install.packages("plotly")
require(plotly)

# Ajout d'une variable 'year'
for(year in 1:100){
  dataProv<-get(paste0("age",year))
  dataProv$year<-year
  assign(x = paste0("age",year),dataProv)
}
age0$year<-0
# Tout dans le même data.frame
df<-age0
for(year in 1:100){
  dataProv<-get(paste0("age",year))
  df %>% bind_rows(dataProv) -> df
}
# Vérif
str(df)
101*28*28
# Graphique !
fig <- df %>%
  plot_ly(
    x = ~y, 
    y = ~(-x), 
    color = ~robert, 
    frame = ~year,
    type = 'scatter',
    mode = 'markers'
  )

fig

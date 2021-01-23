library(esquisse)
library(gridExtra)
library(rlist)
library(tidyr)
library(tibble)
library(data.table)
library(sqldf)
library(tidyverse)
library(hrbrthemes)
library(gganimate)
library(gifski)
library(transformr)
library(rmarkdown)

#Formatage des données
#Nous avons fusionner les départements 2A et 2B en un département 20 représantant la Corse
data <- read.csv2('covid.csv', sep = ',')
liste_dep <- read.csv2('liste_dep.csv',sep=',',encoding='UTF-8')

week_int = c(NULL)
incidence = c(NULL)

table_dep = data.frame(cbind(1:103))
colnames(table_dep)[1] = 'Dep'
num_dep = unique(data$dep)
table_dep = add_column(table_dep, d = num_dep)
colnames(table_dep)[2] = 'Num_dep'
pop_dep = c(NULL)

data$pop <- as.integer(data$pop)


for (k in 1:length(data$week)) {
  semaine = strsplit(data$week[k], 'S')[[1]][2]
  week_int[k] = as.numeric(semaine)
  incidence[k] = (as.numeric(data$P[k]) / as.numeric(data$pop[k])) * 100000
  if (data$cl_age90[k] == 0 && week_int[k] == 21) {
    pop_dep[data$dep[k]] = as.numeric(data$pop[k])
  }
  
}

dep_P = sqldf('SELECT sum(P) as P FROM data GROUP BY dep;')
table_dep = add_column(table_dep, d = (dep_P))

data = add_column(data, d = week_int, .after = "week")

colnames(data)[3] = 'week_int'

data = add_column(data, d = incidence, .after = "P")

colnames(data)[6] = 'Taux_incidence'



pop_dep = data.frame(pop_dep)
pop_dep = subset(pop_dep,!is.na(pop_dep))



table_dep = add_column(table_dep, d = pop_dep)

colnames(table_dep)[4] = 'Pop'
colnames(table_dep)[3] = 'P'

#Creation de la table classe_pop



table_age = sqldf(
  'SELECT  cl_age90,sum(P) as P,sum(pop)/(max(week_int)-min(week_int)+1) as Pop FROM data GROUP BY cl_age90;'
)

#Table week

table_week_classe = sqldf('SELECT DISTINCT week_int FROM data')
col_incidence_classe = c(1:length(table_week_classe$week_int))
col_incidence_classe = data.frame(col_incidence_classe)

for (i in table_age$cl_age90) {
  a = paste('SELECT Pop FROM table_age WHERE cl_age90=', i, ';')
  
  pop = sqldf(a)
  for (k in table_week_classe$week_int) {
    b = paste('SELECT sum(P) as P FROM data WHERE week_int=',
              k,
              'AND cl_age90=',
              i,
              ';')
    
    sum_p = sqldf(b)
    
    incidence_week = (sum_p$P / pop$Pop) * 100000
    col_incidence_classe$col_incidence_classe[k - 20] = incidence_week
    
  }
  table_week_classe = add_column(table_week_classe, d = col_incidence_classe$col_incidence_classe)
  
  
  
}

colnames(table_week_classe) = c(
  'week_int',
  'classe_0',
  'classe_9',
  'classe_19',
  'classe_29',
  'classe_39',
  'classe_49',
  'classe_59',
  'classe_69',
  'classe_79',
  'classe_89',
  'classe_90'
)



#Création table_week_dep

table_week_dep = sqldf('SELECT DISTINCT week_int FROM data')
col_incidence_dep = c(1:length(table_week_classe$week_int))
col_incidence_dep = data.frame(col_incidence_dep)


# formatage table_dep suite aux erreurs
table_super_dep = data.frame(matrix(nrow = nrow(table_dep), ncol = ncol(table_dep)))


for (i in 1:ncol(table_super_dep)) {
  for (j in 1:nrow(table_super_dep))
    
    table_super_dep[j, i] = table_dep[j, i]
}

colnames(table_super_dep) = c('Dep', 'Num_dep', 'P', 'Pop')


for (i in table_dep$Num_dep) {
  a = paste('SELECT Pop FROM table_super_dep WHERE Num_dep=', i, ';')
  
  pop_dep = sqldf(a)
  for (k in table_week_classe$week_int) {
    b = paste('SELECT sum(P) as P FROM data WHERE week_int=',
              k,
              'AND dep=',
              i,
              ';')
    
    sum_dep_p = sqldf(b)
    
    incidence_week = (sum_dep_p$P / pop_dep$Pop) * 100000
    col_incidence_dep$col_incidence_dep[k - 20] = incidence_week
    
  }
  table_week_dep = add_column(table_week_dep, d = col_incidence_dep)
  
  
  
  
}

#Noms colonnes table_week_dep


k = 2
for (i in table_super_dep$Num_dep) {
  colnames(table_week_dep)[k] = paste("Dep_", i)
  k = k + 1
}

data %>%
  filter(Taux_incidence >= 0L & Taux_incidence <= 100L) %>%
  ggplot() +
  aes(x = week_int, y = cl_age90, color = Taux_incidence) +
  geom_tile(size = 10L) +
  
  scale_color_gradient(low = '#001CBD', high = '#E70000') +
  labs(x = "Numéro de semaine(2020)", y = "Classe d'âge", title = "Evolution du taux d'incidence par classe d'âge") +
  theme_light()


#Heatmap avec nos données calculés à la main table_week_classe

table_week_classe_melted = melt(as.data.table(table_week_classe), id = 'week_int')
colnames(table_week_classe_melted)[2] = 'Classe'
colnames(table_week_classe_melted)[3] = 'Taux_incidence'

ggplot(table_week_classe_melted,
       aes(factor(week_int), Classe, fill = Taux_incidence)) +
  geom_tile() +
  scale_fill_gradient(low = '#001CBD', high = '#E70000') +
  geom_text(aes(label = round(Taux_incidence, 1)), color = 'white') +
  labs(x = "Numéro de semaine(2020)", y = "Classe d'âge", title = "Evolution du taux d'incidence par classe d'âge") +
  theme_light()

#Heatmap avec nos données calculés à la main table_week_dep

table_week_dep_melted = melt(as.data.table(table_week_dep), id = 'week_int')
colnames(table_week_dep_melted)[2] = 'Dep'
colnames(table_week_dep_melted)[3] = 'Taux_incidence'

heatmap_table_week_dep = ggplot(table_week_dep_melted, aes(factor(week_int), Dep, fill =
                                                             Taux_incidence)) +
  geom_tile() +
  scale_fill_gradient(low = '#001CBD', high = '#E70000') +
  geom_text(aes(label = round(Taux_incidence, 1)), color = 'white') +
  labs(x = "Numéro de semaine(2020)", y = "Département", title = "Evolution du taux d'incidence par Département") +
  theme_light()


ggsave(
  plot = heatmap_table_week_dep,
  width = 30,
  height = 30,
  dpi = 300,
  filename = "heatmap_table_week_dep.pdf",
  limitsize = FALSE
)



#Multiples courbes de l'évolution de l'incidence par classe âge en fonction du temps


multiple_courbe <-ggplot(
  table_week_classe_melted,
  aes(
    x = week_int,
    y = Taux_incidence,
    group = Classe,
    color = Classe
  )
) +
  geom_line() +
  labs(x = "Numéro de semaine(2020)", y = "Taux d'incidence", title = "Evolution du taux d'incidence par Classe") +
  theme_light() 


multiple_courbe
anim <- ggplot(
  table_week_classe_melted,
  aes(
    x = week_int,
    y = Taux_incidence,
    group = Classe,
    color = Classe
  )
) +
  geom_line() +
  
  
  transition_reveal(
    week_int
  )+
labs(x = "Numéro de semaine(2020)", y = "Taux d'incidence", title = "Evolution du taux d'incidence par Classe") +
  geom_text(aes(x = min(week_int), y = max(Taux_incidence), label = as.factor(week_int)) , hjust=-2, vjust = 3, alpha = 0.5,  col = "gray", size = 20)+
  theme_light() +
  view_follow()

anim

animate(
  plot = anim,
  render = gifski_renderer(),
  height = 600,
  width = 800, 
  duration = 10,
  fps = 25)

anim_save('anim.gif')



ggplot(table_week_classe_melted) +
  aes(x = week_int, y = Taux_incidence, colour = Classe, size = Taux_incidence) +
  geom_line() +
  scale_color_hue() +
  theme_minimal() +
  facet_wrap(vars(Classe))





#On va ajouter le nom des départements dans notre jeu de données data pour 
liste_dep_transform =list(NULL)
for (i in 1:length(liste_dep$nom)){
  liste_dep_transform <-append(liste_dep_transform,rep(liste_dep$nom[i],length(table_age$cl_age90)*length(table_week_dep$week_int)))
}
liste_dep_transform=liste_dep_transform[-1]

#On l'ajoute à data

  

data <- add_column(data,d=liste_dep_transform,.after=data$dep)

#On le renomme
colnames(data)[2]= 'Nom_Dep'

#On l'ajoute à table_week_dep_melted
liste_dep_transform =list(NULL)
for (i in 1:length(liste_dep$nom)){
  liste_dep_transform <-append(liste_dep_transform,rep(liste_dep$nom[i],length(table_week_dep$week_int)))
}
liste_dep_transform=liste_dep_transform[-1]
table_week_dep_melted <- add_column(table_week_dep_melted,d=liste_dep_transform,.after=table_week_dep_melted$Dep)
colnames(table_week_dep_melted)[2]= 'Nom_Dep'



data_bar_race <- table_week_dep_melted %>%
  group_by(week_int) %>%
  arrange(week_int,desc(Taux_incidence)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <=15)

head(data_bar_race)  


bar_race <- data_bar_race %>%
  ggplot()+
  geom_col(aes(ranking,Taux_incidence,fill=Dep))+
  geom_text(aes(ranking,Taux_incidence,label=floor(Taux_incidence)),hjust=-0.1)+
  geom_text(aes(ranking,y=0,label=Nom_Dep),hjust=1.1)+
  geom_text(aes(x=15,y=max(Taux_incidence),label=as.factor(week_int)),vjust=0.2,alpha=0.5,color='grey',size=20)+
  coord_flip(clip='off',expand = FALSE)+scale_x_reverse()+
  theme_minimal() + theme(
    panel.grid=element_blank(),
    legend.position = 'none',
    axis.ticks.y =element_blank(),
    axis.title = element_blank(),
    axis.text.y=element_blank(),
    plot.margin = margin(1,4,1,5,'cm')
  )+
  transition_states(week_int,state_length = 0,transition_length = 1)+
  enter_fade()+
  exit_fade()+
  ease_aes('quadratic-in-out')

  bar_race
 
  animate(
    plot = bar_race,
    render = gifski_renderer(),
    height = 600,
    width = 1000, 
    duration = 40,
    fps = 25)

anim_save('bar_race.gif')

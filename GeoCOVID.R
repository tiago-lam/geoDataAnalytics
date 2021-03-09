COVID <- read.csv('/Users/tiagomachado/Desktop/NU Desktop/GeoExplorer_DataExtraction/GeoCovidAnalysis/COVID.csv', header = TRUE)
attach(COVID)
#detach(COVID)

library(psych)
library(dplyr)
COVID <- 
  COVID %>% 
  mutate_at(vars(starts_with("Post_Q31_")), 
            funs("rc" = recode(.,
                               "Strongly disagree"=1,
                               "Disagree"=2,
                               "Somewhat disagree"=3,
                               "Neither agree nor disagree"=4,
                               "Somewhat agree"=5,
                               "Agree"=6,
                               "Strongly agree"=7)))

COVID <- 
  COVID %>% 
  mutate_at(vars(starts_with("Post_Q33_")), 
            funs("rc" = recode(.,
                               "Strongly disagree"=1,
                               "Disagree"=2,
                               "Somewhat disagree"=3,
                               "Neither agree nor disagree"=4,
                               "Somewhat agree"=5,
                               "Agree"=6,
                               "Strongly agree"=7)))
attach(COVID)
#Recode
COVID$Post_Q31_1_rc_r <- 8 - Post_Q31_1_rc
COVID$Post_Q31_5_rc_r <- 8 - Post_Q31_5_rc

COVID$Post_Q33_1_rc_r <- 8 - Post_Q33_1_rc
COVID$Post_Q33_3_rc_r <- 8 - Post_Q33_3_rc
attach(COVID)

#We first looked at the entire scale
COV <- data.frame(Post_Q31_1_rc_r, Post_Q31_2_rc, Post_Q31_3_rc, Post_Q31_4_rc, Post_Q31_5_rc_r)
alpha(COV)
#reliability is low regardless

#then at the items associated with Psychosocial Consequences
GOE <- data.frame(Post_Q33_1_rc_r, Post_Q33_2_rc, Post_Q33_3_rc_r, Post_Q33_4_rc, Post_Q33_5_rc, Post_Q33_6_rc)
alpha(GOE)
#if item 1 is dropped it gets to .71

#then at the items associated with Functional Consequences
ALL <- data.frame(Post_Q31_1_rc_r, Post_Q31_2_rc, Post_Q31_3_rc, Post_Q31_4_rc, Post_Q31_5_rc_r, Post_Q33_1_rc_r, Post_Q33_2_rc, Post_Q33_3_rc_r, Post_Q33_4_rc, Post_Q33_5_rc, Post_Q33_6_rc)
alpha(ALL)
#reliability of all items is .67. Dropping both #1s makes it go toe .68

ALL <- data.frame(Post_Q31_2_rc, Post_Q31_3_rc, Post_Q31_4_rc, Post_Q31_5_rc, Post_Q33_2_rc, Post_Q33_3_rc, Post_Q33_4_rc, Post_Q33_5_rc, Post_Q33_6_rc)
alpha(ALL)
#if both 1s are dropped the reliability is .7. 

ALL <- data.frame(Post_Q31_2_rc, Post_Q31_3_rc, Post_Q31_4_rc, Post_Q33_2_rc, Post_Q33_4_rc, Post_Q33_5_rc, Post_Q33_6_rc)
alpha(ALL)


fit <- fa(ALL, nfactors=2, fm="wls", rotate="oblimin", cor="poly")
print(fit, digits=2, cutoff=0.3)
summary(fit)

str(fit)

library(psych)
#bfi <- bfi[,1:24]                 #select variables to include in fa
#fit <- fa(bfi, 2)                 #estimate model with 2 factors
fs <- factor.scores(ALL, fit) #obtain factor scores
#fs <- matrix(fs)
fs <- fs$scores                   #get the columns of factor scores for each case
ALL <- cbind(ALL,fs)
View(ALL)
View(fs)

plot(ALL$WLS1,ALL$WLS2)

ALL$COV <- (Post_Q31_2_rc + Post_Q31_3_rc + Post_Q31_4_rc) / 3 
ALL$GEO <- (Post_Q33_2_rc + Post_Q33_4_rc + Post_Q33_5_rc + Post_Q33_6_rc) / 4

plot(ALL$COV,ALL$GEO)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


install.packages("factoextra")
install.packages("tidyverse")

WLS1 <- ALL$WLS1
df <- data.frame(WLS1)
df$WLS2 <- ALL$WLS2
df <- na.omit(df)
df <- scale(df)


hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

clusterCut <- cutree(hc3, 4)

dfc <- data.frame(clusterCut)


fviz_dend(hc3, rect = TRUE)
fviz_cluster(hc3)


df$circle <- dat

df <- data.frame(df)
img <- ggplot(df, aes(WLS1, WLS2)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue')) +
  theme(panel.background = element_rect(fill = 'transparent', color = "pink"),
        panel.grid.major = element_line(color = 'gray'))
img

ggsave(img, filename = "/Users/tiagomachado/Desktop/cluster.png",  bg = "transparent")

clusters <- hclust(dist(ALL[, 3:4]), method = 'average')
plot(clusters)


#######################################Clustering Scores (PQ47)######################################################################
pq47 <- COVID$Post_Q47
gender <- COVID$Pre_Q37
cluster_colors <- COVID$cluster

table(pq47, gender, cluster_colors)

score_per_cluster <- data.frame(pq47, cluster_colors)
gender_per_colors <- data.frame(gender, cluster_colors)

blackCluster <- score_per_cluster$pq47[score_per_cluster$cluster_colors == 'black']
redCluster <- score_per_cluster$pq47[score_per_cluster$cluster_colors == 'red']
greenCluster <- score_per_cluster$pq47[score_per_cluster$cluster_colors == 'green']
blueCluster <- score_per_cluster$pq47[score_per_cluster$cluster_colors == 'blue']

median(blackCluster)
mean(blackCluster)
sd(blackCluster)

median(redCluster)
mean(redCluster)
sd(redCluster)

median(blueCluster)
mean(blueCluster)
sd(blueCluster)

median(greenCluster)
mean(greenCluster)
sd(greenCluster)

######################################genders in clusters############################################################################
gender_in_black <- gender_per_colors$gender[gender_per_colors$cluster_colors == 'black']
length(gender_in_black)
length(gender_in_black[gender_in_black == 'Man'])
length(gender_in_black[gender_in_black == 'Woman'])
length(gender_in_black[gender_in_black == 'Prefer not to answer'])
length(gender_in_black[gender_in_black == ''])

gender_in_red <- gender_per_colors$gender[gender_per_colors$cluster_colors == 'red']
length(gender_in_red)
length(gender_in_red[gender_in_red == 'Man'])
length(gender_in_red[gender_in_red == 'Woman'])
length(gender_in_red[gender_in_red == 'Prefer not to answer'])
length(gender_in_red[gender_in_red == ''])

gender_in_blue <- gender_per_colors$gender[gender_per_colors$cluster_colors == 'blue']
length(gender_in_blue)
length(gender_in_blue[gender_in_blue == 'Man'])
length(gender_in_blue[gender_in_blue == 'Woman'])
length(gender_in_blue[gender_in_blue == 'Prefer not to answer'])
length(gender_in_blue[gender_in_blue == ''])

gender_in_green <- gender_per_colors$gender[gender_per_colors$cluster_colors == 'green']
length(gender_in_green)
length(gender_in_green[gender_in_green == 'Man'])
length(gender_in_green[gender_in_green == 'Woman'])
length(gender_in_green[gender_in_green == 'Prefer not to answer'])
length(gender_in_green[gender_in_green == ''])

######################################race in clusters############################################################################
race <- COVID$Pre_Q38_alt
cluster_colors <- COVID$cluster

race_per_cluster <- data.frame(race, cluster_colors)
race_c_black <- race_per_cluster$race[race_per_cluster$cluster_colors == 'black']
race_c_red <- race_per_cluster$race[race_per_cluster$cluster_colors == 'red']
race_c_green <- race_per_cluster$race[race_per_cluster$cluster_colors == 'green']
race_c_blue <- race_per_cluster$race[race_per_cluster$cluster_colors == 'blue']

black_tb <- table(race_c_black)
red_tb <- table(race_c_red)
green_tb <- table(race_c_green)
blue_tb <- table(race_c_blue)

hist(black_tb)

######################################race and gender###############################################################################
race_gender_cluster = data.frame(race, gender, cluster_colors)


race_non_empty <- race_gender_cluster$race[race_gender_cluster$cluster_colors == 'black' | race_gender_cluster$cluster_colors == 'red' 
                                           | race_gender_cluster$cluster_colors == 'blue' | race_gender_cluster$cluster_colors == 'green']
gender_non_empty <- race_gender_cluster$gender[race_gender_cluster$cluster_colors == 'black' | race_gender_cluster$cluster_colors == 'red' 
                                             | race_gender_cluster$cluster_colors == 'blue' | race_gender_cluster$cluster_colors == 'green']
cluster_non_empty <- race_gender_cluster$cluster_colors[race_gender_cluster$cluster_colors == 'black' | race_gender_cluster$cluster_colors == 'red' 
                                                | race_gender_cluster$cluster_colors == 'blue' | race_gender_cluster$cluster_colors == 'green']

#total
race_gender_cluster_non_empty <- data.frame(race_non_empty, gender_non_empty, cluster_non_empty)
tab1(race_gender_cluster_non_empty$gender_non_empty, sort.group = "decreasing", cum.percent = TRUE)
tab1(race_gender_cluster_non_empty$race_non_empty, sort.group = "decreasing", cum.percent = TRUE)

print_race_freqs_by_gender(race_gender_cluster_non_empty, 'red', 'Woman')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'red', 'Man')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'black', 'Woman')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'black', 'Man')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'green', 'Woman')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'green', 'Man')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'blue', 'Woman')
print_race_freqs_by_gender(race_gender_cluster_non_empty, 'blue', 'Man')
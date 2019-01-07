install.packages("ggplot2")
install.packages("plyr")

#Load library
library("ggplot2")

#Read data
localidad = read.csv("los lagos.csv") 
num_respuestas=19# 17 magallanes, 19 los lagos, 13 aysen 


#GRAPH 1 - Bars graph for average and standard deviation of each factors risk

#Calculate average and sd, based on aggregation of column 11 (factor code)
localidad.riesgo.mean<-aggregate(localidad[11], list(localidad$Codigo), mean)
localidad.riesgo.sd<-aggregate(localidad[11], list(localidad$Codigo), sd)

#Group information in one data frame
data <- data.frame(
  factor=localidad.riesgo.mean[c(1)],
  mean=localidad.riesgo.mean[c(2)],
  sd=localidad.riesgo.sd[c(2)]
)

#Change columns name
colnames(data) <- c("factor", "mean", "sd")

#Generamos graph with error lines
ggplot(data) +
  #Avergages bar
  geom_bar( aes(x=factor, y=mean), stat="identity", fill="blue", alpha=0.5) + 
  #Errors asociated to sd
  geom_errorbar( aes(x=factor, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=0.3) +
  #Axis text alignment
  theme(axis.text.x = element_text(angle=45, hjust=1, color = "black"))+
  #Edges name
  ylab("Riesgo")+xlab("Factor")

#GRAPH 2 - Stacked bar graph for frequency of occurancy of each value asigned to the probability of each factor

library(plyr)
#For each factor, count the amount of time each probability occurs
tabla <-count(localidad, c("Codigo", "Probabilidad_n"))

#Change legend order
tabla$Probabilidad_n2 <- factor(tabla$Probabilidad_n, c("Casi seguro","Probable","Ocasional","Poco usual","Casi nulo"))


#Plot
ggplot(tabla, aes(x = Codigo, y = freq, fill = Probabilidad_n2, label = freq)) +
  geom_bar(stat = "identity", colour="black") +
  #Text indicating number of occurrences
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  #Flip graph horizontally
  coord_flip()+
  #Egdges parameters
  scale_y_continuous(limits=c(0,num_respuestas), breaks=c(0,num_respuestas))+
  #Colors for different probabilities
  scale_fill_manual(values = c("#3366CC","#109618","yellow", "#FF9900","#DC3912") )+
  labs(fill="Probabilidad")+
  #Edges names
  xlab("Factores")+ylab("Frecuencia")


#GRAPH 3 - Stacked bar graph. Same as previous one but with consequences value instead of probability

library(plyr)
tabla <-count(localidad, c("Codigo", "Consecuencia_n"))

tabla$Consecuencia_n2 <- factor(tabla$Consecuencia_n, c("Catastrofica", "Mayor", "Moderada","Menor","Insignificante"))

ggplot(tabla, aes(x = Codigo, y = freq, fill = Consecuencia_n2, label = freq)) + geom_bar(stat = "identity", colour="black") + geom_text(size = 3, position = position_stack(vjust = 0.5))+coord_flip()+ scale_y_continuous(limits=c(0,num_respuestas), breaks=c(0,num_respuestas))+scale_fill_manual(values = c("#3366CC","#109618","yellow", "#FF9900","#DC3912") )+labs(fill="Probabilidad")+xlab("Factores")+ylab("Frecuencia")

#GRAPH 4 - Biplot probability consequence with standard deviation

#Calculate average and standard deviation for probability and consequence
factor=aggregate(localidad[8], list(localidad$Codigo), mean)[c(1)]
probabilidad.mean=aggregate(localidad[8], list(localidad$Codigo), mean)[c(2)]
probabilidad.sd=aggregate(localidad[8], list(localidad$Codigo), sd)[c(2)]
consecuencia.mean=aggregate(localidad[9], list(localidad$Codigo), mean)[c(2)]
consecuencia.sd=aggregate(localidad[9], list(localidad$Codigo), sd)[c(2)]



#Pllot points
qplot(probabilidad.mean,consecuencia.mean)+
  #Standar deviation
  geom_errorbar(aes(ymin=consecuencia.mean-consecuencia.sd, ymax=consecuencia.mean+consecuencia.sd), width=0.05)+
  geom_errorbarh(aes(xmin=probabilidad.mean-probabilidad.sd, xmax=probabilidad.mean+probabilidad.sd), width=0.1)+
  #Limits and margins of edges
  scale_x_continuous(limits = c(0, 6),expand = c(0,0))+scale_y_continuous(limits = c(0, 6),expand = c(0,0))+
  #Add factors names
  geom_text(aes(label=rownames(factor)),hjust=0, vjust=0, color="blue")+
  #Names for edges
  xlab("Probabilidad")+ylab("Consecuencia")+
  #Paint background
  geom_rect(aes(xmin = 3, xmax = 5, ymin = 3, ymax = 5),fill = "red", alpha = 0.02)+
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 4, ymax = 5),fill = "pink", alpha = 0.02)+
  geom_rect(aes(xmin = 4, xmax = 5, ymin = 2, ymax = 3),fill = "pink", alpha = 0.02)+
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 3, ymax = 5),fill = "yellow", alpha = 0.02)+
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 2, ymax = 4),fill = "yellow", alpha = 0.02)+
  geom_rect(aes(xmin = 3, xmax = 4, ymin = 1, ymax = 3),fill = "yellow", alpha = 0.02)+
  geom_rect(aes(xmin = 4, xmax = 5, ymin = 1, ymax = 2),fill = "yellow", alpha = 0.02)+
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 3, ymax = 5),fill = "lightgreen", alpha = 0.02)+
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 1, ymax = 3),fill = "lightgreen", alpha = 0.02)+
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 1, ymax = 2),fill = "lightgreen", alpha = 0.02)+
  geom_rect(aes(xmin = 3, xmax = 5, ymin = 0, ymax = 1),fill = "lightgreen", alpha = 0.02)+
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 3),fill = "green", alpha = 0.02)+
  geom_rect(aes(xmin = 1, xmax = 3, ymin = 0, ymax = 1),fill = "green", alpha = 0.02)+
  geom_rect(aes(xmin = 0, xmax = 6, ymin = 5, ymax = 6),fill = "white", alpha = 0.02)+
  #Arrange grid
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

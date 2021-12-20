library(tidyverse)


voti <-  readxl::read_xlsx("/Users/andreabarghetti/Desktop/Fun/nutella blind tests/smartella.xlsx", sheet= 1)  %>% 
  gather(marca, voto, 2:6)

nutella <- readxl::read_xlsx("/Users/andreabarghetti/Desktop/Fun/nutella blind tests/smartella.xlsx", sheet= 2)%>% 
  gather(marca, voto, 2:6) %>% 
  filter(!is.na(voto))


# simiglianza alla nutella
nutella %>%
  count(marca, giudice,voto) %>%
  mutate(n=n/sum(n)) %>%
  ggplot(aes(x=reorder(marca, desc(n)), y=n, fill=giudice)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=giudice, y=n), position = "stack", vjust=5) +
  ggtitle("Somiglianza alla Nutella")+
  theme_classic() +
  theme(legend.position = "none")+
  xlab("") + ylab("")


#miglior marca
brutta1 <- ggplot(voti,
       aes(x=marca, y=voto, group=marca)) +
  geom_boxplot(fill="#8B4513") +
  stat_summary(fun=mean, geom="point", color="black", fill="red", size=6, shape=21) +
  geom_point(aes(fill=giudice), position=position_jitter(width = .3,height = 0.5, seed = 123), size=2, shape=21) +
  geom_text(aes(group=giudice, label=giudice, angle=45), position=position_jitter(width = .3, height = 0.5, seed = 123), 
            size=5, hjust=0, vjust=0, check_overlap = F) +
  stat_summary(fun.y=mean, colour="black", geom="text", show.legend = FALSE, 
               vjust=-1,size=5, aes(label=round(..y.., digits=1))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, size=12),
        axis.text.y = element_text(hjust = 1, size=12)) +
  xlab("")

#miglior marca
brutta2 <-ggplot(voti,
       aes(x=marca, y=voto, group=marca)) +
  geom_violin(fill="#8B4513") +
  stat_summary(fun.y=mean, geom="point", color="black", fill="red", size=10, shape=21) +
  geom_point(aes(fill=giudice), position=position_jitter(width = .1,height = 0.1, seed = 123), size=2, shape=21) +
  ggrepel::geom_text_repel(aes(group=giudice, label=giudice, angle=0, col=giudice), position=position_jitter(width = 0, height = .5, seed = 123), 
            size=5, hjust=.5, vjust=.5) +
  stat_summary(fun.y=mean, colour="red", geom="text", show.legend = FALSE, 
               vjust=-1,size=5, aes(label=round(..y.., digits=1))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, size=12),
        axis.text.y = element_text(hjust = 1, size=12)) +
  xlab("")

#miglior marca
ggplot(voti,
       aes(x=marca, y=voto, group=marca)) +
  geom_violin(fill="grey",trim = T, col="black") +
  geom_point(aes(fill=giudice), position=position_jitter(width = .1,height = 0.1, seed = 123), size=4, shape=21) +
  stat_summary(fun.y=mean, colour="black", geom="label", show.legend = FALSE,
               vjust=--.5,size=5, aes(label=round(..y.., digits=1))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, size=12),
        axis.text.y = element_text(hjust = 1, size=12)) +
  xlab("") +
  ggtitle("Migliortella")


# preferenze by giudice
voti2<-nutella %>% select(giudice, guess=marca) %>% right_join(voti) %>%
  mutate(colore = guess==marca,
         colore=case_when(colore==T&marca=="Nutella"~"a", colore==T&marca!="Nutella"~"b", T~"c"))

ggplot(voti2,
       aes(x=giudice, y=voto, group=giudice)) +
  geom_violin(fill="grey",trim = T, col="black", alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="black", fill="white", size=5, shape=21) +
  ggrepel::geom_text_repel(aes(label=marca, col=colore), position=position_jitter(width = 0, height = 0.05, seed = 123),direction = "y") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        legend.position = "none") +
  xlab("") +
  scale_color_manual(values = c("blue", "red", "black")) +
  ggtitle("Preferenze", subtitle = "in rosso quella creduta la Nutella ")




#preferenze individuali

ggplot(voti2,aes(x=reorder(marca, voto), y=voto)) +
  geom_bar(stat="identity", fill="grey", col="black") +
  theme_classic() +
  facet_wrap(~giudice, scales = "free_x") +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        text = element_text(size=12)) +
  ggtitle("preferenze personali") +
  xlab("") +
  ylab("voto medio")



# medioman
voti %>% group_by(marca) %>%
  summarise(voto_medio=mean(voto)) %>%
  right_join(voti) %>%
  mutate(scarto=abs(voto_medio-voto)) %>%
  group_by(giudice) %>%
  summarise(scarto=sum(scarto)) %>% #usa scarto quadratico medio!
  arrange(scarto)


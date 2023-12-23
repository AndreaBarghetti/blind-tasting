library(tidyverse)

wine_info = read_csv("wine_xmas2023/WineInfo.csv") %>% 
  rename(Vino = Nome2,
         NomeCompleto = Nome) %>% 
  mutate(Bottiglia = paste(Vino,Lettera))
wine_ratings = read_csv("wine_xmas2023/WineRatings.csv") %>% 
  rename(Lettera=Vino,
         Giudizio=Voto,
         GiudizioGradazione=Gradazione)
wine_guesses = read_csv("wine_xmas2023/WineGuesses.csv") %>% 
  gather(Vino, Lettera, -1) %>% 
  mutate(Vino = str_remove(Vino, "[12]$"))

results = wine_ratings %>% 
  left_join(wine_info)



# indovina il doppione ####
paired_wines_rating <- results %>%
  filter(Lettera %in% c("B","E")) %>% 
  group_by(Nome) %>% 
  summarise(min=min(Giudizio), max=max(Giudizio)) %>% 
  ggplot(aes(x= reorder(Nome, abs(min-max)), y=min, yend=max, xend=Nome)) +
  geom_segment(size=2) +
  geom_point(aes(y=min),
             size=3, stat="identity") +
  geom_point(aes(y=max),
             size=3, stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(y="Giudizio", x="") +
  ggtitle("Giudizio sulle bottiglie identiche") +
  scale_y_continuous(limits = c(1,5))


ggsave(plot = paired_wines_rating,
       filename = "paired_wines_rating.png",
       path = "wine_xmas2023/plots/", 
       width = 16,height = 12,
       units = "cm",
       device = "png")

# indovina il vino ####
wine_guesses %>% 
  left_join(wine_info %>% 
              select(Lettera, Assegnato = Vino)) %>%
  group_by(Nome) %>% 
  summarise(punteggio = sum(Vino==Assegnato)) %>% 
  arrange(desc(punteggio))

indoVino <- wine_guesses %>% 
  left_join(select(wine_info,Lettera,VinoInd = Vino)) %>% 
  select(-Lettera) %>% 
  mutate(corretto = Vino==VinoInd) %>% 
  group_by(Nome, Vino) %>% 
  summarise(VinoInd = unique(VinoInd) %>% str_c(collapse = "\n"),
            corretto=all(corretto)) %>% 
  ggplot(aes(x=Vino,y=Nome, fill=corretto)) +
  geom_tile(col="black", show.legend = F,alpha=.5) +
  scale_fill_manual(values = c("red","green")) +
  geom_text(aes(label=map_chr(VinoInd,str_c, collapse="\\n"))) +
  theme_classic() +
  labs(x=NULL, y=NULL) +
  ggtitle("Indovina il Vino!")

ggsave(plot = indoVino,
       filename = "indoVino.png",
       path = "wine_xmas2023/plots/", 
       width =20,height = 14,
       units = "cm",
       device = "png")

## correlatione prezzo giudizio ####

costpay1 <- results %>%
  ggplot(aes(y=Offerta, x=Giudizio)) +
  geom_boxplot(aes(group=Giudizio)) +
  geom_point(shape=21, size=3, position = position_dodge2(width = .5), aes(fill=Vino), show.legend = T, alpha=.5) +
  theme_classic() +
  labs(y="Quanto credi che costa", x="quanto ti piace",fill="") +
  geom_smooth(method="lm", col="gray", linetype="dashed", se=F)

costpay2 <- results %>%
  ggplot(aes(y=`Prezzo asta`, x=Giudizio)) +
  geom_boxplot(aes(group=Giudizio)) +
  geom_point(shape=21, size=3, position = position_dodge2(width = .5),
             aes(fill=Vino), alpha=.5) +
  theme_classic() +
  labs(y="Quanto veramente costa", x="quanto ti piace",fill="")+
  geom_smooth(method="lm", col="gray", linetype="dashed", se=F)

pricecorrplot = ggpubr::ggarrange(plotlist = list(A=costpay1, B=costpay2), 
                  common.legend = T, legend = "bottom")


ggsave(plot = pricecorrplot,
       filename = "pricecorrplot.png",
       path = "wine_xmas2023/plots/", 
       width =18, height = 12,
       units = "cm",
       device = "png")

lmdl = lm(data=results, Giudizio~`Prezzo asta`) 

broom::tidy(lmdl)


#gara asta ####
auction_rank <- results %>% 
  mutate(diff = Offerta - `Prezzo asta`,
         Punteggio = ifelse(diff>=0,`Prezzo asta`, 0)
         )  %>% 
  group_by(Nome) %>% 
  summarise(Punteggio = sum(Punteggio)) %>% 
  arrange(Punteggio)

auction_rankplot = auction_rank %>% 
  ggplot(aes(x=reorder(Nome, Punteggio), y=Punteggio)) +
  geom_col(aes(fill=Punteggio), col="black", show.legend = F) +
  theme_bw() +
  scale_fill_continuous(type = "viridis",direction=1) +
  labs(x="") +
  ggtitle("Indovina il prezzo!")

ggsave(plot = auction_rankplot,
       filename = "auction_rank.png",
       path = "wine_xmas2023/plots/", 
       width =16, height = 10,
       units = "cm",
       device = "png")

# Grado alcolico percepito ####
etohbyname <- results %>%
  select(Nome, GiudizioGradazione) %>%
  ggplot(aes(x=Nome, y=GiudizioGradazione)) +
  geom_violin() +
  geom_point(size=2, shape=21, position= position_dodge2(width = 0.5)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle("Grado alcolico percepito per sommelier") +
  labs(y="Grado alcolico percepito", x="")

ethobywine <- results %>%
  select(Vino, GiudizioGradazione, Gradazione) %>%
  ggplot(aes(x=Vino, y=GiudizioGradazione)) +
  geom_violin() +
  geom_point(size=2, shape=21, position= position_dodge2(width = 0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(data= wine_info, col="darkred",alpha=.5,size=5, aes(x=Vino, y=Gradazione)) +
  ggtitle("Grado alcolico percepito per vino") +
  labs(y="Grado alcolico percepito", x="")

ggsave(plot = ethobywine,
       filename = "ethobywine.png",
       path = "wine_xmas2023/plots/", 
       width =16,height = 10,
       units = "cm",
       device = "png")


## Indovina il grado alcolico!####
ehohguess_rank <- results %>% 
  mutate(diff = abs(GiudizioGradazione - Gradazione)) %>%
  group_by(Nome) %>% 
  summarise(Punteggio = mean(diff)) %>% 
  arrange(Punteggio) %>% 
  filter(!is.na(Punteggio))

ehohguess_rankplot = ehohguess_rank %>% 
  ggplot(aes(x=reorder(Nome, Punteggio), y=Punteggio)) +
  geom_col(aes(fill=Punteggio), col="black", show.legend = F) +
  theme_bw() +
  scale_fill_continuous(type = "viridis", trans = 'reverse') +
  labs(x="", y="Errore medio") +
  ggtitle("Indovina il grado alcolico!")


## Classifica dei vini 
vino_rating_bottle <- results %>%
  ggplot(aes(y=Giudizio, x = Bottiglia)) +
  geom_violin() +
  geom_point(position=position_dodge2(width=.2), shape=21, alpha=.5, col="black", size=2) +
  geom_point(size=5,shape=21, stat="summary", fill="darkred", alpha=.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") + ylab("Giudizio") +
  ggtitle("Giudizi sul vino")

ggsave(plot = vino_rating_bottle,
       filename = "vino_rating_bottle.png",
       path = "wine_xmas2023/plots/", 
       width =16,height = 10,
       units = "cm",
       device = "png")

convenienza = results %>% 
  group_by(Vino) %>% 
  summarise(Prezzo = mean(`Prezzo asta`),
            Giudizio = mean(Giudizio),
            GradoAlcolico = mean(Gradazione)) %>% 
  ggplot(aes(x=Prezzo, y=Giudizio)) +
  geom_point(size=5, show.legend = T) +
  theme_bw() +
  scale_y_continuous(breaks = c(1:5), limits = c(1,5)) +
  scale_x_continuous(breaks = c(2,5,10,20,40)) +
  coord_cartesian(xlim = c(0,50)) +
  geom_text(aes(label=Vino), hjust=0, nudge_x = 1) +
  labs(x="Prezzo (€)", y="Giudizio medio") +
  ggtitle("Qualitá vs prezzo")
 
ggsave(plot = convenienza,
       filename = "convenienza.png",
       path = "wine_xmas2023/plots/", 
       width =12,height = 12,
       units = "cm",
       device = "png")


classifica <- results %>% 
  group_by(Vino) %>% 
  summarise(Giudizio=mean(Giudizio)) %>% 
  arrange(desc(Giudizio)) %>% 
  arrange(desc(Giudizio))

# Giudizi sul vino per sommelier ####
rating_per_sommelier <- results %>%
  ggplot(aes(y=Giudizio, x= Nome)) +
  geom_violin(trim = T, scale = "count") +
  geom_point(position=position_dodge2(width=.5), 
             size=2, shape=21,alpha=1, stat="identity") +
  geom_point(stat="summary", fill="darkred", alpha=.5,shape=21, size=5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(x="", y="Giudizio") +
  ggtitle("Giudizi sul vino per sommelier")

ggsave(plot = rating_per_sommelier,
       filename = "rating_per_sommelier.png",
       path = "wine_xmas2023/plots/", 
       width =18,height = 12,
       units = "cm",
       device = "png")


# Clustering ####

heatmap <- results %>%
  select(Nome,Lettera,Giudizio, Vino) %>%
  unite(Vino, Vino, Lettera, sep= " ") %>%
  spread(Vino, Giudizio) %>%
  column_to_rownames("Nome") %>%
  as.matrix() %>% #t() %>%
  scale(center = F, scale = F) %>%
  pheatmap::pheatmap()


ggsave(plot = heatmap,
       filename = "heatmap.png",
       path = "wine_xmas2023/plots/", 
       width =10,height = 12,
       units = "cm",
       device = "png")

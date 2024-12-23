library(tidyverse)

plots_dir = "wine_xmas2024/plots/"

wine_info = read_csv("wine_xmas2024/WineInfo.csv") %>% 
  rename(Vino = Nome2,
         NomeCompleto = Nome) %>% 
  mutate(Bottiglia = paste(Vino,Lettera))

wine_ratings = read_csv("wine_xmas2024/WineRatings.csv") %>% 
  rename(Lettera=Vino,
         Giudizio=Voto)

results = wine_ratings %>% 
  left_join(wine_info)

# trova lo champagne ####
trovaLoChampagne = results %>% 
  select(Nome, Champagne, Vino, Tipo) %>% 
  mutate(Champagne = replace_na(Champagne,'no')) %>% 
  mutate(
    result = case_when(Champagne=='yes'&Tipo=='Champagne'~'Bravo, é Champagne!',
                       Champagne=='yes'&Tipo!='Champagne'~'sbagliato, non é Champagne...',
                       Champagne=='no'&Tipo!='Champagne'~'',
                       Champagne=='no'&Tipo=='Champagne'~'sbagliato, é Champagne...')) %>% 
  ggplot(aes(x=Nome,y=reorder(Vino, Tipo), fill=result))+
  geom_tile(col='gray') +
  theme_classic() +
  labs(x='',y='', fill='') +
  ggtitle("Trova lo Champagne!") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  scale_fill_manual(values = c('white','gold','black','gray'))

ggsave(plot = trovaLoChampagne,
       filename = "trovaLoChampagne.png",
       path = plots_dir, 
       width =18,height = 8,
       units = "cm",
       device = "png")

# gara vini ####
classifica <- results %>% 
  group_by(Vino) %>% 
  summarise(Giudizio=mean(Giudizio, na.rm = T)) %>% 
  arrange(desc(Giudizio)) %>% 
  arrange(desc(Giudizio))

vino_rating_bottle <- results %>%
  ggplot(aes(y=Giudizio, x = Bottiglia)) +
  geom_violin() +
  geom_point(position=position_dodge2(width=.2), shape=21, alpha=.5, col="black", size=2) +
  # geom_point(size=5,shape=21, stat="summary", fill="darkred", alpha=.5) +
  geom_point(size=5,shape=21, stat="summary", aes(fill=ifelse(Tipo=='Champagne','Champagne','Spumante')), alpha=.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") + ylab("Giudizio") +
  labs(fill='') + 
  ggtitle("Giudizi sul vino") +
  scale_fill_manual(values = c('gold','gray'))


ggsave(plot = vino_rating_bottle,
       filename = "vino_rating_bottle.png",
       path = plots_dir, 
       width =16,height = 10,
       units = "cm",
       device = "png")

# convenienza ####
convenienza = results %>% 
  group_by(Vino, Tipo) %>% 
  summarise(Prezzo = mean(`Prezzo asta`),
            Giudizio = mean(Giudizio, na.rm = T)) %>% 
  ggplot(aes(x=Prezzo, y=Giudizio)) +
  geom_point(size=5, show.legend = T,shape=21,
             aes(fill=ifelse(Tipo=='Champagne','Champagne','Spumante'))) +
  theme_bw() +
  scale_y_continuous(breaks = c(1:5), limits = c(1,5)) +
  scale_x_continuous(breaks = c(1,3,8,13,20,29)) +
  coord_cartesian(xlim = c(0,40)) +
  geom_text(aes(label=Vino), hjust=0, nudge_x = 1) +
  labs(x="Prezzo (€)", y="Giudizio medio", fill='') +
  ggtitle("Qualitá vs prezzo")+
  scale_fill_manual(values = c('gold','gray'))


ggsave(plot = convenienza,
       filename = "convenienza.png",
       path = plots_dir, 
       width =14,height = 12,
       units = "cm",
       device = "png")


# Giudizi sul vino per sommelier ####
rating_per_sommelier <- results %>%
  ggplot(aes(y=Giudizio, x= Nome)) +
  geom_violin(trim = T, scale = "count") +
  geom_point(position=position_dodge2(width=.5), 
             size=2, shape=21,alpha=1, stat="identity",
             aes(fill=ifelse(Tipo=='Champagne','Champagne','Spumante')), show.legend = T) +
  geom_point(stat="summary", fill="darkred", alpha=.5,shape=21, size=5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(x="", y="Giudizio", fill='') +
  ggtitle("Giudizi sul vino per sommelier") +
  scale_fill_manual(values = c('gold','gray'))

ggsave(plot = rating_per_sommelier,
       filename = "rating_per_sommelier.png",
       path = plots_dir, 
       width =18,height = 12,
       units = "cm",
       device = "png")

# Gara Asta ####
auction_rank <- results %>% 
  mutate(diff = Offerta - `Prezzo asta`,
         Punteggio = abs(diff)
  )  %>% 
  group_by(Nome) %>% 
  summarise(Punteggio = sum(Punteggio)) %>% 
  arrange(Punteggio)

auction_rankplot = auction_rank %>% 
  ggplot(aes(x=reorder(Nome, Punteggio), y=Punteggio)) +
  geom_col(aes(fill=Punteggio), col="black", show.legend = F) +
  theme_bw() +
  scale_fill_continuous(type = "viridis",direction=-1) +
  labs(x="", y='Scarto tra offerta e prezzo reale') +
  ggtitle("Indovina il prezzo", subtitle = "(vince chi fa meno errore)")

ggsave(plot = auction_rankplot,
       filename = "auction_rank.png",
       path = plots_dir, 
       width =16, height = 10,
       units = "cm",
       device = "png")

# clustering ####
heatmap <- results %>%
  select(Nome,Giudizio, Vino) %>%
  remove_missing() %>% 
  unite(Vino, Vino, sep= " ") %>%
  spread(Vino, Giudizio) %>%
  column_to_rownames("Nome") %>%
  as.matrix() %>% #t() %>%
  scale(center = F, scale = F) %>%
  pheatmap::pheatmap(na_col = 'white')

ggsave(plot = heatmap,
       filename = "heatmap.png",
       path = plots_dir, 
       width =10,height = 12,
       units = "cm",
       device = "png")

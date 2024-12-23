library(tidyverse)

wine_info = read_csv("maria_bday2024/WineInfo.csv") %>% 
  rename(Wine = Name) %>% 
  mutate(Bottle = paste(Wine,Letter))

wine_ratings = read_csv("maria_bday2024/WineRatings.csv") %>% 
  rename(Letter=Wine,
         Alcohol_guessed=`Alcohol Percentage`)

results = wine_ratings %>% 
  left_join(wine_info, by = join_by(Letter))

# indovina il doppione ####
paired_wines_rating <- results %>%
  filter(Letter %in% c("F","G")) %>% 
  group_by(Name) %>% 
  summarise(min=min(Rating), max=max(Rating)) %>% 
  ggplot(aes(x= reorder(Name, abs(min-max)), y=min, yend=max, xend=Name)) +
  geom_segment(linewidth=2) +
  geom_point(aes(y=min),
             size=3, stat="identity") +
  geom_point(aes(y=max),
             size=3, stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(y="Rating", x="") +
  ggtitle("Rating on identical wines") +
  scale_y_continuous(limits = c(1,5))


ggsave(plot = paired_wines_rating,
       filename = "paired_wines_rating.png",
       path = "maria_bday2024/plots/", 
       width = 16,height = 12,
       units = "cm",
       device = "png")

# add description letters to dots
min_max_ratings <- results %>%
  filter(Letter %in% c("F", "G")) %>%
  group_by(Name) %>%
  summarize(min_rating = min(Rating), max_rating = max(Rating),
            same_letter = length(unique(Description))==1)

paired_wines_rating2 <- results %>%
  filter(Letter %in% c("F","G")) %>% 
  left_join(min_max_ratings, by = "Name") %>% 
  ggplot(aes(x=Name, xend=Name, y=Rating, fill=Description)) +
  geom_segment(aes(y=min_rating, yend=max_rating),show.legend = F,
               lineend='round', linewidth=2) +
  geom_label(size=3,aes(label=Description), stat="identity", show.legend = F,
             position=position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(y="Rating", x="") +
  ggtitle("Rating on identical wines", 'Correct description: E') +
  scale_y_continuous(limits = c(1,5))

ggsave(plot = paired_wines_rating2,
       filename = "paired_wines_rating2.png",
       path = "maria_bday2024/plots/", 
       width = 18,height = 12,
       units = "cm",
       device = "png")


## correlatione Price Rating ####

costpay1 <- results %>%
  ggplot(aes(y=Offer, x=Rating)) +
  geom_boxplot(aes(group=Rating)) +
  geom_point(shape=21, size=3, position = position_dodge2(width = .5), aes(fill=Wine), show.legend = T, alpha=.5) +
  theme_classic() +
  labs(y="How much you'd pay for it", x="How much you liked it",fill="") +
  geom_smooth(method="lm", col="gray", linetype="dashed", se=F)

costpay2 <- results %>%
  ggplot(aes(y=Price, x=Rating)) +
  geom_boxplot(aes(group=Rating)) +
  geom_point(shape=21, size=3, position = position_dodge2(width = .5),
  aes(fill=Wine), alpha=.5) +
  theme_classic() +
  labs(y="How much it really costs", x="How much you liked it",fill="")+
  geom_smooth(method="lm", col="gray", linetype="dashed", se=F)

pricecorrplot = ggpubr::ggarrange(plotlist = list(A=costpay1, B=costpay2), 
                                  common.legend = T, legend = "bottom")


ggsave(plot = pricecorrplot,
       filename = "pricecorrplot.png",
       path = "maria_bday2024/plots/", 
       width =18, height = 12,
       units = "cm",
       device = "png")


#gara asta ####
auction_rank <- results %>% 
  mutate(diff = (Offer - Price))  %>% 
  group_by(Name) %>% 
  summarise(Score = mean(abs(diff))) %>% 
  arrange(Score)

auction_rankplot = auction_rank %>% 
  ggplot(aes(x=reorder(Name, Score), y=Score)) +
  geom_col(aes(fill=Score), col="black", show.legend = F) +
  theme_bw() +
  scale_fill_continuous(type = "viridis",direction=-1) +
  labs(x="",y="Mean Absolute Error (Kr)") +
  ggtitle("Guess the price!")

ggsave(plot = auction_rankplot,
       filename = "auction_rank.png",
       path = "maria_bday2024/plots/", 
       width =16, height = 10,
       units = "cm",
       device = "png")

# Grado alcolico percepito ####
etohbyname <- results %>%
  select(Name, Alcohol_guessed) %>%
  ggplot(aes(x=Name, y=Alcohol_guessed)) +
  geom_violin() +
  geom_point(size=2, shape=21, position= position_dodge2(width = 0.5)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle("Alcohol, by person") +
  labs(y="Alcohol perc. guessed", x="")

ethobywine <- results %>%
  select(Wine, Alcohol_guessed, `Alcohol Percentage`) %>%
  ggplot(aes(x=Wine, y=Alcohol_guessed)) +
  geom_violin() +
  geom_point(size=2, shape=21, position= position_dodge2(width = 0.5)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(data= wine_info, col="darkred",alpha=.5,size=5, aes(x=Wine, y=`Alcohol Percentage`)) +
  ggtitle("Alcohol, by wine") +
  labs(y="Alcohol perc. guessed", x="")

ggsave(plot = ethobywine,
       filename = "ethobywine.png",
       path = "maria_bday2024/plots/", 
       width =16,height = 10,
       units = "cm",
       device = "png")


## Indovina il grado alcolico!####
ehohguess_rank <- results %>% 
  mutate(diff = abs(Alcohol_guessed - `Alcohol Percentage`)) %>%
  group_by(Name) %>% 
  summarise(Score = mean(diff)) %>% 
  arrange(Score) %>% 
  filter(!is.na(Score))

ehohguess_rankplot = ehohguess_rank %>% 
  ggplot(aes(x=reorder(Name, Score), y=Score)) +
  geom_col(aes(fill=Score), col="black", show.legend = F) +
  theme_bw() +
  scale_fill_continuous(type = "viridis", trans = 'reverse') +
  labs(x="", y="Mean Error") +
  ggtitle("Guess the alcohol percentage!")

# fix missing rosa and Andrea due to NA?

## Classifica dei vini 
Wine_rating_bottle <- results %>%
  ggplot(aes(y=Rating, x = Bottle)) +
  geom_violin() +
  geom_point(position=position_dodge2(width=.2), shape=21, alpha=.5, col="black", size=2) +
  geom_point(size=5,shape=21, stat="summary", fill="darkred", alpha=.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  xlab("") + ylab("Rating") +
  ggtitle("Rating on Wine")

ggsave(plot = Wine_rating_bottle,
       filename = "Wine_rating_bottle.png",
       path = "maria_bday2024/plots/", 
       width =16,height = 10,
       units = "cm",
       device = "png")

classifica <- results %>% 
  group_by(Wine) %>% 
  summarise(Rating=mean(Rating)) %>% 
  arrange(desc(Rating)) %>% 
  arrange(desc(Rating))

# Giudizi sul Wine per sommelier ####
rating_per_sommelier <- results %>%
  ggplot(aes(y=Rating, x= Name)) +
  geom_violin(trim = T, scale = "count") +
  geom_point(position=position_dodge2(width=.5), 
             size=2, shape=21,alpha=1, stat="identity") +
  geom_point(stat="summary", fill="darkred", alpha=.5,shape=21, size=5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(x="", y="Rating") +
  ggtitle("Rating on Wine by person")

ggsave(plot = rating_per_sommelier,
       filename = "rating_per_sommelier.png",
       path = "maria_bday2024/plots/", 
       width =18,height = 12,
       units = "cm",
       device = "png")


# Pregiudice of country

rating_countries <- results %>%
  filter(!is.na(Country.x)) %>%
  ggplot(aes(y=Rating, x= reorder(Country.x, desc(Rating)))) +
  geom_violin(trim = T, scale = "count") +
  geom_point(position=position_dodge2(width=.5), 
             size=2, shape=21,alpha=1, stat="identity") +
  geom_point(stat="summary", fill="darkred", alpha=.5,shape=21, size=5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        text=element_text(size=12)) +
  labs(x="", y="Rating") +
  ggtitle("Rating on Wine by person")

ggsave(plot = rating_countries,
       filename = "rating_countries.png",
       path = "maria_bday2024/plots/", 
       width =18,height = 12,
       units = "cm",
       device = "png")

# Clustering ####

heatmap <- results %>%
  select(Name,Letter,Rating, Wine) %>%
  unite(Wine, Wine, Letter, sep= " ") %>%
  spread(Wine, Rating) %>%
  column_to_rownames("Name") %>%
  as.matrix() %>% #t() %>%
  scale(center = F, scale = F) %>%
  pheatmap::pheatmap()


ggsave(plot = heatmap,
       filename = "heatmap.png",
       path = "maria_bday2024/plots/", 
       width =10,height = 12,
       units = "cm",
       device = "png")

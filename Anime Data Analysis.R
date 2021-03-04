library(tidyverse)
library(ggplot2)
tidy_anime <- read_csv(file = "tidy_anime.csv")
view(tidy_anime)

#Using "-" to deselect unwanted columns

anime <- tidy_anime%>%
  select(-c(title_synonyms,genre,producers,studio,synopsis, background, broadcast, related))
view(anime)

#Removing repetitive data and rows with NA values in title_english column

u_anime <- anime %>% distinct() %>% filter(!is.na(title_english))
view(u_anime)

#Filtering TV shows that have finished airing

tv_anime <- u_anime %>% filter(type == "TV", airing=="FALSE")
view(tv_anime)

#Taking the top 25 TV shows based on popularity(finished airing)

tv_plot <- tv_anime %>% top_n(-25,wt=popularity)
view(tv_plot)

popularity <- ggplot(tv_plot, aes(reorder(title_english, desc(popularity)), popularity)) + 
  geom_point(color="steelblue") + coord_flip() + 
  labs(x="", y = "Popularity", title = "Top 25 Popular Anime", subtitle = "(Finished Airing)") + 
  theme_classic() + geom_text(aes(label = popularity),
                              vjust = "inward", hjust = "outward", size=2.8,
                              show.legend = FALSE)
popularity

#Filtering TV shows that are currently airing
tv_anime_1 <- u_anime %>% filter(type == "TV", airing=="TRUE")

#Taking the top 25 TV shows based on popularity(currently airing)

tv_plot_1 <- tv_anime_1 %>% top_n(-25,wt=popularity)
view(tv_plot_1)

popularity_current <- ggplot(tv_plot_1, aes(reorder(title_english, desc(popularity)), popularity)) + geom_point(color="orange") +
  coord_flip() + labs(x="", y = "Popularity", title = "Top 25 Popular Anime", subtitle = "(Currently Airing)") + 
  theme_classic() + geom_text(aes(label = popularity),
                              vjust = "inward", hjust = "outward", size=2.8,
                              show.legend = FALSE)
popularity_current      



#highest number of episodes

tv_episode <- u_anime %>% filter(type == "TV") %>% top_n(25,wt=episodes)

tv_episode_1 <- ggplot(tv_episode, aes(title_english, episodes)) + geom_point(color="purple") + coord_flip() + 
  geom_text(aes(label = episodes),
            vjust = "inward", hjust = "outward", size=2.8,
            show.legend = FALSE) + theme_classic() + 
  labs(x="", y="episodes", title="Top 25 anime with highest number of episodes")
tv_episode_1       

#Top 25 anime based on rank

tv_rank <- u_anime %>% filter(type == "TV") %>% top_n(-25,wt=rank)
view(tv_rank)
tv_rank_1 <- ggplot(tv_rank, aes(reorder(title_english, desc(rank)), rank, fill=rank)) + 
  geom_bar(stat = 'identity',width=0.5,color='black',show.legend = FALSE) + coord_flip() + 
  theme_classic() + geom_text(aes(label = rank),
                              hjust = 1, size=3.2, color="slategray1", fontface="bold",
                              show.legend = FALSE) + labs(x="", y="rank", title="Top 25 highest ranked anime")
tv_rank_1

#Top 25 anime with the highest scores

score <- u_anime %>% filter(type == "TV") %>% top_n(25,wt=scored_by)

highest_score <- ggplot(score, aes(reorder(title_english, scored_by), scored_by, color=title_english)) + 
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(x=title_english,xend=title_english,y=0,yend=scored_by),show.legend = FALSE) + 
  labs(x="", y="scored", title="Top 25 highest scored") + coord_flip() + theme_classic()
highest_score


#Top 25 anime with highest number of memebrs 

highest_members <- u_anime %>% filter(type == "TV") %>% top_n(25,wt=members)

members <- ggplot(highest_members, aes(reorder(title_english, members), members, color=title_english)) + 
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(x=title_english,xend=title_english,y=0,yend=members),show.legend = FALSE) + 
  labs(x="", y="members", title="Top 25 highest members") + coord_flip() + theme_classic()

members


#Top 25 favorite anime

favorite <- u_anime %>% filter(type == "TV") %>% top_n(25,wt=favorites)

favorites <- ggplot(highest_members, aes(reorder(title_english, favorites), favorites, color=title_english)) + 
  geom_point(show.legend = FALSE) + 
  geom_segment(aes(x=title_english,xend=title_english,y=0,yend=favorites),show.legend = FALSE) + 
  labs(x="", y="favorites", title="Top 25 highest favorites") + coord_flip() + theme_classic()

favorites

#Highest members, favorites, score

members + favorites + highest_score + plot_layout(widths = 3)

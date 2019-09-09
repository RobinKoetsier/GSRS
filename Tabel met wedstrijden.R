RateTable <- function(MatchDay){
# GOAL RATING ACCOUNT!!!!!!
  requestURL = "https://api.twitter.com/oauth/request_token"
  accessURL = "https://api.twitter.com/oauth/access_token"
  authURL = "https://api.twitter.com/oauth/authorize"
  consumerKey = "EMrF9z7lcwbNFrj3CM0lelsR9"
  consumerSecret = "lACU90dZDurcnZfbt6zyLlRqALBisXV0OcfZ3fQtj0rZ8L8Dx0"
  accessToken = "234746783-cLdroBPF86mPFZvWZ1qUEPdF5YjHbAlbdlxtvBzk"
  accessSecret = "rDELanbzRq9jtTMadpIDME6UzNFwqzqhZMcfcOujqWOHe"
  
page <- "https://www.oddschecker.com/football/netherlands/eredivisie"
scraped_page <- read_html(page)


OddsUni <- scraped_page %>% html_nodes(".all-odds-click+ .basket-add .add-to-bet-basket") %>% html_text()
Clubs <- scraped_page %>% html_nodes(".fixtures-bet-name") %>% html_text()
OddsUni<-round((as.integer(word(OddsUni,1,sep = "/")) + as.integer(word(OddsUni,2,sep = "/")))  / as.integer(word(OddsUni,2,sep = "/")),2)


Clubs = list(Clubs)

Clubs2=Clubs[1:length(Clubs)-5] 
Clubs2 <- unlist(Clubs2)

Clubs2 <- substring(Clubs2, 1,3)
Games <- as.data.frame(matrix(ncol=2, nrow=9))

i=1
j=1

while (i < 10) {
  
  Games[i,1] = Clubs2[j]
  i <- i+1
  j = j+2
  
}

i=1
j=2
while (i < 10) {
  Games[i,2] = Clubs2[j]
  i <- i+1
  j = j+2
}



Games[Games=="Twe"]<-"Twente"
Games[Games=="Emm"]<-"FC Emmen"
Games[Games=="AZ "]<-"AZ Alkmaar"
Games[Games=="Den"]<-"Den Haag"
Games[Games=="Aja"]<-"Ajax"
Games[Games=="SBV"]<-"Excelsior"
Games[Games=="Fey"]<-"Feyenoord"
Games[Games=="Spa"]<-"Sparta Rotterdam"
Games[Games=="Her"]<-"Heracles"
Games[Games=="For"]<-"For Sittard"
Games[Games=="Ven"]<-"VVV Venlo"
Games[Games=="PSV"]<-"PSV Eindhoven"
Games[Games=="Waa"]<-"Waalwijk"
Games[Games=="Hee"]<-"Heerenveen"
Games[Games=="Gro"]<-"Groningen"
Games[Games=="Wil"]<-"Willem II"
Games[Games=="Zwo"]<-"Zwolle"
Games[Games=="Utr"]<-"Utrecht"
Games[Games=="Vit"]<-"Vitesse"


#for (i in 1:nrow(test1920)) {
#  if(test1920$Round[i] == 1| test1920$Round[i]==2 | test1920$Round[i] == 3 | test1920$Round[i] == 4) {
#    test1920$GoalScore5[i] = NA
#  }
#}

test1920 <- test1920 %>% group_by(Team) %>% top_n(1, Round)
Rating <- test1920 %>% distinct(Team, .keep_all = TRUE ) %>% select(2,13)

# Tijdelijk weg, tot alles weer gelijk loopt
#Rating = test1920 %>% filter(Round == MatchDay) %>% select(2,11)


#Rating2 = test1920 %>% filter(Round == MatchDay -1) %>%
#  filter(Team %in% c("Ajax", "AZ Alkmaar", "Feyenoord","For Sittard", "Groningen", "PSV Eindhoven")) %>%
#  select(2,11)

#Rating = rbind(Rating1, Rating2)

names(Games)[1] <-"HTeam"
names(Games)[2] <-"ATeam"

rate3 <- Games %>% left_join(Rating, by = c("HTeam" = "Team")) %>% 
  mutate(GSHome = GoalScore5)  

Rate<-  rate3 %>% left_join(Rating, by = c("ATeam" = "Team")) %>%
  mutate(GSAway = GoalScore5.y) %>% select(1,2,4,6) %>% 
  mutate(Rating = GSHome - GSAway) %>%
  mutate(HomeChance = (0.0164*Rating + 0.5132)*100) %>%
  mutate(Odds = round(100/HomeChance,2)) #%>%
  #mutate(HomeChance = paste(HomeChance," %"))

Rate <- Rate %>% mutate(Oddschecker = OddsUni[1:9])
TestRating <- Rate %>% left_join(ja, by = c("Rating" = "Score"))
Rate <- Rate %>% mutate("Sample Size" = TestRating$Som)
Rate <- Rate %>% mutate("Dif" = Oddschecker - Odds)

RateNieuw <- formattable(Rate, list
                         (Dif = FALSE,
                           #`Sample Size` = color_tile('red', 'green'),
                           `HomeChance` = icon_formatter()
                           #`HomeChance` = color_tile('red','green'),
                 `Oddschecker` = formatter("span", 
                  style = ~ style(color = ifelse(`Oddschecker` > `Odds`, "green", "red")),
                                    ~ icontext(sapply(`Dif`, function(x) if (x < 0) "arrow-down" 
                                                      else if (x > 0) "arrow-up" else ""), `Oddschecker`))))

export_formattable(RateNieuw, "formattable.png")

View(Rate)

#for (i in 1:nrow(Rate)) {
#  if((Rate$Oddschecker[i] > Rate$Odds[i]) && (Rate$Rating[i] <= 5) && (Rate$Rating[i] >= -5)){
#    print(Rate$HTeam[i])
#    print(Rate$Rating[i])
#  }
#}

tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:2], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)))


png("tabel.png", height=200, width=700)
p<-tableGrob(Rate, rows = NULL, theme = tt3)
p<-gtable_add_grob(p,
                   grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                   t = 2, b = nrow(p), l = 1, r = ncol(p))
grid.arrange(p)
dev.off()
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

tweet(paste0("Match day: ", MatchDay + 1), mediaPath = "formattable.png")

}




#################################################################################################


TweeCol <- ja%>%select(1,2,6)
TweeCol <- TweeCol %>% mutate(LG = (0.01644*Score + 0.51321))

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}



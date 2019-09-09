VergelijkSeizoenen <- function(SpeelRonde){
# Persoonlijke twitter:
  
  requestURL = "https://api.twitter.com/oauth/request_token"
  accessURL = "https://api.twitter.com/oauth/access_token"
  authURL = "https://api.twitter.com/oauth/authorize"
  consumerKey = "DNXWiJaML40aO1w9hdWaJfB3b"
  consumerSecret = "i1i38JbQGEQiwAX9Tw09q9AMbqsK55sORt2F2pCgowWbiDylVo"
  accessToken = "704398332182044672-coRTM5EkBmggQgkBjvWs2ehhPn8Ha36"
  accessSecret = "rupv0v4qvZPDF65xy9Yy0mV8TKCvFASDCMsu26o2407EE"
  
  setup_twitter_oauth(consumerKey,
                      consumerSecret,
                      accessToken,
                      accessSecret)  
  
Stand1920 <- ere1920_total %>% filter(Round == SpeelRonde) %>% select(9,2,4,6) %>% arrange(Position)
Stand1819 <- ere1819_total %>% filter(Round == SpeelRonde) %>% select(9,2,4,6) %>% arrange(Position)
Stand1819[Stand1819=="NAC Breda"]<-"Twente"
Stand1819[Stand1819=="Graafschap"]<-"Sparta Rotterdam"
Stand1819[Stand1819=="Excelsior"]<-"Waalwijk"

TweeSeizoenen <- Stand1819 %>% left_join(Stand1920, by = "Team")

#ere1819[ere1819=="NAC Breda"]<-"Twente"
#ere1819[ere1819=="Graafschap"]<-"Sparta Rotterdam"
#ere1819[ere1819=="Excelsior"]<-"Waalwijk"



names(TweeSeizoenen)[1] <-"Pos1819"
names(TweeSeizoenen)[3] <-"Points1819"
names(TweeSeizoenen)[4] <-"GD1819"
names(TweeSeizoenen)[5] <-"Pos1920"
names(TweeSeizoenen)[6] <-"Points1920"
names(TweeSeizoenen)[7] <-"GD1920"

TweeSeizoenen <- TweeSeizoenen %>% select(2,1,5,3,6,4,7) %>% 
  mutate(Color = ifelse(TweeSeizoenen$Points1920 > TweeSeizoenen$Points1819, TweeSeizoenen$Color <- "green4","red"))

TweeSeizoenen$Team <- factor(TweeSeizoenen$Team, levels = TweeSeizoenen$Team[order(TweeSeizoenen$Pos1920)])


############## VERSCHIL IN PLEK OP RANGLIJST!!! #################
print("Plot 1")
png( filename = "PosLolly.png",
     units="in", 
     width=15, 
     height=(8), 
     pointsize=12, 
     res=300)

p <- ggplot(TweeSeizoenen) +
  geom_segment( aes(x=Team, xend=Team, y=Pos1920, yend=Pos1819), color=TweeSeizoenen$Color, size = 2.3,alpha = 0.5) +
  geom_point( aes(x=Team, y=Pos1819), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Team, y=Pos1920), color=rgb(0.7,0.2,0.1,0.5), size=4, alpha = 1 ) + theme_light() + 
  theme(legend.position = "none", panel.border = element_blank()) +
  xlab("") +
  theme_tufte() +
  ylab("Points") +
  scale_y_reverse() +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=45)) +

  labs(title = "Position difference with last season",
       subtitle = paste0("Match day: ",SpeelRonde)) +
  geom_text(aes(x=  TweeSeizoenen$Pos1920, y = TweeSeizoenen$Pos1920,label = TweeSeizoenen$Pos1920 ), color = "white", size = 3)
  
print(p)
dev.off()

left_label <- paste(TweeSeizoenen$Team, TweeSeizoenen$Pos1819)
right_label <- paste(TweeSeizoenen$Pos1920, TweeSeizoenen$Team)


TweeSeizoenen$class <- ifelse((TweeSeizoenen$Pos1920 - TweeSeizoenen$Pos1819) < 0,"green3", ifelse((TweeSeizoenen$Pos1920 - TweeSeizoenen$Pos1819)>0, "red2", "orange2"))


##### Segment ding met punten en volgorde van positie
print("Plot 2")
png( filename = "PosSegment.png",
     units="in", 
     width=15, 
     height=(8), 
     pointsize=12, 
     res=300)



p <- ggplot(TweeSeizoenen) + geom_segment( aes(x=1, xend=2, y=Pos1819, yend=Pos1920, size=.05), color = TweeSeizoenen$class, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.05) + 
  geom_vline(xintercept=2, linetype="dashed", size=.05) + 
  
  labs(x="", y="") + 
  
  
  geom_text(aes(label=left_label, y=TweeSeizoenen$Pos1819), x=1, hjust=1, size=3.5) +
  geom_text(aes(label=right_label, y=TweeSeizoenen$Pos1920), x=2, hjust=0, size=3.5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))  + scale_y_reverse() +
  scale_x_continuous(limits = c(0.9,2.1)) +
  geom_text(label=paste("After Match Day",SpeelRonde), x=1.5, y=2, vjust=8, size=4) +
  geom_text(label="18/19", x=1, y=2, vjust=8, size=4) +
  geom_text(label="19/20", x=2, y=2, vjust=8, size=4) + annotate(geom="text", x=2, y=19, label="Made by @RobinWilhelmus in RStudio",
                                                                 color="black", size = 3)

print(p) 
dev.off()
###############################

print("Plot 3")
# Point Segment
png( filename = "PointLolly.png",
     units="in", 
     width=15, 
     height=(8), 
     pointsize=12, 
     res=300)


p <- ggplot(TweeSeizoenen) +
  geom_segment( aes(x=Team, xend=Team, y=Points1920, yend=Points1819), color=TweeSeizoenen$Color, size = 2.3,alpha = 0.5) +
  geom_point( aes(x=Team, y=Points1819), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Team, y=Points1920), color=rgb(0.7,0.2,0.1,0.5), size=4, alpha = 1 ) + theme_light() + 
  theme(legend.position = "none", panel.border = element_blank()) +
  xlab("") +
  theme_economist() +
  ylab("Points") +
  
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=45)) +
  scale_y_continuous(c(0,2,4,6,8,10,12)) +
  
  labs(title = "Points difference with last season",
       subtitle = paste0("Match day: ",SpeelRonde))

print(p)

dev.off()

#################### Zelfde wedstrijden #############

#ere1819$HomeTeam <- gsub('NAC Breda', 'Twente', ere1819$HomeTeam)
#ere1819$AwayTeam <- gsub('NAC Breda', 'Twente', ere1819$AwayTeam)
#ere1819$HomeTeam <- gsub('Graafschap', 'Sparta Rotterdam', ere1819$HomeTeam)
#ere1819$AwayTeam <- gsub('Graafschap', 'Sparta Rotterdam', ere1819$AwayTeam)
#ere1819$HomeTeam <- gsub('Excelsior', 'Waalwijk', ere1819$HomeTeam)
#ere1819$AwayTeam <- gsub('Excelsior', 'Waalwijk', ere1819$AwayTeam)

# Wedstrijden dit seizoen
ere19201 <- ere1920 %>% select(1:7)
ZelfdePotten <- ere19201 %>% right_join(ere1819, by = c("HomeTeam","AwayTeam"))

ZelfdePotten <- na.omit(ZelfdePotten)

ZelfdePotten[sort(unique(ZelfdePotten$HomeTeam))] = NA


for(i in 1:nrow(ZelfdePotten)) {
  if(ZelfdePotten$FTR.x[i] == "H") {
    ZelfdePotten[i, ZelfdePotten$HomeTeam[i]] = 3
    ZelfdePotten[i, ZelfdePotten$AwayTeam[i]] = 0
  } else if(ZelfdePotten$FTR.x[i] == "A") {
    ZelfdePotten[i, ZelfdePotten$AwayTeam[i]] = 3
    ZelfdePotten[i, ZelfdePotten$HomeTeam[i]] = 0
  } else{
    ZelfdePotten[i, c(ZelfdePotten$AwayTeam[i], ZelfdePotten$HomeTeam[i])] = 1
  }
}

ZelfdePotten_points = ZelfdePotten  %>% 
  gather(Team, PointsPerGame, 'Ajax':`Zwolle`) %>% 
  select(Date.x, Team, PointsPerGame) %>% 
  drop_na(PointsPerGame)

head(ZelfdePotten_points)
ZelfdePotten_points[is.na(ZelfdePotten_points)] = 0

ZelfdePotten_points = ZelfdePotten_points %>% 
  group_by(Team) %>% 
  arrange(Date.x) %>% 
  mutate(Points = cumsum(PointsPerGame)) %>% 
  ungroup() 

ZelfdePotten_points[which.max(ZelfdePotten_points$Points),]
#ZelfdePotten_points[ZelfdePotten_points[, .I[Points == max(Points)], by=Team]]
ZelfdePotten_pointsMax <- ZelfdePotten_points %>% group_by(Team) %>% top_n(1, Points)
ZelfdePotten_pointsMax <- ZelfdePotten_pointsMax %>% group_by(unique(Team))
ZelfdePotten_pointsMax <- ZelfdePotten_pointsMax %>% distinct(Team, .keep_all = TRUE )


# Zelfde wedstrijden vorig seizoen
ere19201 <- ere1920 %>% select(1:7)
ZelfdePotten2 <- ere19201 %>% right_join(ere1819, by = c("HomeTeam","AwayTeam"))

ZelfdePotten2 <- na.omit(ZelfdePotten2)

ZelfdePotten2[sort(unique(ZelfdePotten2$HomeTeam))] = NA


for(i in 1:nrow(ZelfdePotten2)) {
  if(ZelfdePotten2$FTR.y[i] == "H") {
    ZelfdePotten2[i, ZelfdePotten2$HomeTeam[i]] = 3
    ZelfdePotten2[i, ZelfdePotten2$AwayTeam[i]] = 0
  } else if(ZelfdePotten2$FTR.y[i] == "A") {
    ZelfdePotten2[i, ZelfdePotten2$AwayTeam[i]] = 3
    ZelfdePotten2[i, ZelfdePotten2$HomeTeam[i]] = 0
  } else{
    ZelfdePotten2[i, c(ZelfdePotten2$AwayTeam[i], ZelfdePotten2$HomeTeam[i])] = 1
  }
}

ZelfdePotten2_points = ZelfdePotten2  %>% 
  gather(Team, PointsPerGame, 'Ajax':`Zwolle`) %>% 
  select(Date.y, Team, PointsPerGame) %>% 
  drop_na(PointsPerGame)

head(ZelfdePotten2_points)
ZelfdePotten2_points[is.na(ZelfdePotten2_points)] = 0

ZelfdePotten2_points = ZelfdePotten2_points %>% 
  group_by(Team) %>% 
  arrange(Date.y) %>% 
  mutate(Points = cumsum(PointsPerGame)) %>% 
  ungroup() 
ZelfdePotten2_points[which.max(as.numeric(ZelfdePotten2_points$Points)),]

#ZelfdePotten2_pointsMax <- ZelfdePotten2_points %>% group_by(Team) %>% top_n(1, Points)

ZelfdePotten2_pointsMax <- ZelfdePotten2_pointsMax %>% distinct(Team, .keep_all = TRUE )



# Samen voegen:
ZelfdePotten2_pointsMax <- ZelfdePotten2_pointsMax[with(ZelfdePotten2_pointsMax, order(Team)), ]
ZelfdePotten_pointsMax <- ZelfdePotten_pointsMax[with(ZelfdePotten_pointsMax, order(Team)), ]
ZelfdeWedstrijden <- cbind(ZelfdePotten_pointsMax,ZelfdePotten2_pointsMax) %>% select(2,4,9)
ZelfdeWedstrijden <- ZelfdeWedstrijden %>% select(2,3,4)
names(ZelfdeWedstrijden)[3] <- "Points1920"
names(ZelfdeWedstrijden)[4] <- "Points1819"


ZelfdeWedstrijden$Team3 <- reorder(ZelfdeWedstrijden$Team, -ZelfdeWedstrijden$Points1920)

#ZelfdeWedstrijden <- ZelfdeWedstrijden %>% mutate(Color = ifelse(ZelfdeWedstrijden$Points1920 > ZelfdeWedstrijden$Points1819, ZelfdeWedstrijden$Color <- "green4","red"))

#ZelfdeWedstrijden <- ZelfdeWedstrijden %>% filter(ZelfdeWedstrijden, Points1819 > Points1920) 

ZelfdeWedstrijden["Color"] <- ifelse(ZelfdeWedstrijden$Points1920 > ZelfdeWedstrijden$Points1819, ZelfdeWedstrijden$Color <- "green4","red")

for (i in nrow(ZelfdeWedstrijden)){
  if (ZelfdeWedstrijden$Points1920[i] < ZelfdeWedstrijden$Points1819[i]) {
    ZelfdeWedstrijden$Color[i] = 'green'
    
    
  } else if 
  (ZelfdeWedstrijden$Points1920[i] > ZelfdeWedstrijden$Points1819[i] ){
    ZelfdeWedstrijden$Color[i] = 'red'
  
  }else{
    ZelfdeWedstrijden$Color[i] = 'orange';
  }}
  


# Plotten en opslaan
print("Plot 4")
png( filename = "PointMatchesLolly.png",
     units="in", 
     width=15, 
     height=(8), 
     pointsize=12, 
     res=300)

p <- ggplot(ZelfdeWedstrijden) + 
  geom_point( aes(x=Team3, y=Points1819), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_segment( aes(x=Team3, xend=Team3, y=Points1819, yend=Points1920), color=ZelfdeWedstrijden$Color, size = 2.3,alpha = 0.5) +
   
  geom_point( aes(x=Team3, y=Points1920), color=rgb(0.7,0.2,0.1,0.5), size=4, alpha = 1 ) + theme_light() + 
  theme(legend.position = "none", panel.border = element_blank()) +
  xlab("") +
  theme_tufte() +
  ylab("Points") + 
  
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=45)) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(title = "points difference with same matches last season",
       subtitle = paste0("Match day: ",SpeelRonde)) 

print(p)

dev.off()
######################


left_label <- paste(TweeSeizoenen$Team, TweeSeizoenen$Points1819)
right_label <- paste(TweeSeizoenen$Points1920, TweeSeizoenen$Team)



##### Lelijke punten segment ding
print("Plot 5")
png( filename = "PointSegment.png",
     units="in", 
     width=15, 
     height=(8), 
     pointsize=12, 
     res=300)

p <-  ggplot(TweeSeizoenen) + geom_segment( aes(x=1, xend=2, y=Points1819, yend=Points1920, size=.05), color = TweeSeizoenen$class, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.05) + 
  geom_vline(xintercept=2, linetype="dashed", size=.05) + 
  
  labs(x="", y="") + 
  
  
  geom_text_repel(aes(label=left_label, y=TweeSeizoenen$Points1819), x=1, hjust=0, size=3.5) +
  geom_text_repel(aes(label=right_label, y=TweeSeizoenen$Points1920), x=2, hjust=0, size=3.5) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))  +
  scale_x_continuous(limits = c(0.9,2.1)) +
  geom_text(label=paste("After Match Day",SpeelRonde), x=1.5, y=12.5, vjust=1, size=4) +
  geom_text(label="18/19", x=1, y=12.5, vjust=1, size=4) +
  geom_text(label="19/20", x=2, y=12.5, vjust=1, size=4) + annotate(geom="text", x=2, y=0, label="Made by @RobinWilhelmus in RStudio",
                                                                    color="black", size = 3)

print(p)  
dev.off()


source_python("ImagesOnTwitter.py")
}



#################################################################







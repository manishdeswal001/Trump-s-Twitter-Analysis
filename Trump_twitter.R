library("rtweet")
library("igraph")
library(rtweet)

key = "YNPZFXzajaDEedq39zWHh6GyN"
secret = "i7wwv3Twb2TZuploRUGG19xWRONCUuOOHiLF2ZaoaF8Q2PACTz"

create_token(
  app = "Social_Medial_Intelligence",
  consumer_key = key,
  consumer_secret = secret)

tweets = search_tweets('Trump', n = 100, lang = "en", include_rts = FALSE)
tweets$text
tweet.words = strsplit(tweets$text, "[^A-Za-z]+")
word.table = table(unlist(tweet.words))
view(word.table)
## show the top 20 most occuring words
sort(word.table, decreasing = TRUE)[1:20]

tweets = tweets$text
tweets = gsub("http[^[:space:]]*", "", tweets)
## remove hashtags (terms that start with #)
tweets = gsub("#[^[:space:]]*", "", tweets)
## remove author names (terms that start with @)
tweets = gsub("@[^[:space:]]*", "", tweets)
tweets = tweets$text
tweets = gsub("http[^[:space:]]*", "", tweets)
## remove hashtags (terms that start with #)
tweets = gsub("#[^[:space:]]*", "", tweets)
## remove author names (terms that start with @)
tweets = gsub("@[^[:space:]]*", "", tweets)

tmls <- get_timelines("realDonaldTrump", n = 320)
tmls
tweets$texttrump = lookup_users("realDonaldTrump")

#search tweets with #trump from these people , 
#if republican then they are postive trump if democrat anti trump
##################################  quesion 1 ##################3

#Total friend count of Trump
trump = lookup_users("realDonaldTrump")
trump$friends_count

names(trump)

trump_fds <- get_friends("realDonaldTrump")
trump_fds_data <- lookup_users(trump_fds$user_id)

trump_fds_data_verified = trump_fds_data[trump_fds_data$verified == "TRUE", ]
nrow(trump_fds_data_verified)

#WhiteHouse, teamtrump non busines

trump_Business = c('WhiteHouse','Trump', 'TrumpGolf', 'TeamTrump','TrumpDoral',
                   'TrumpCharlotte', 'TrumpLasVegas',
                   'TrumpChicago', 'TrumpGolfDC', 'TrumpGolfLA')

`%nin%` = Negate(`%in%`)
trump_fds_people = trump_fds_data_verified[trump_fds_data_verified$screen_name %nin% trump_Business, ]
nrow(trump_fds_people)

tmp = (trump_fds_people[,c('screen_name','followers_count',"user_id")])
top = tmp[order(-tmp$followers_count),]
top_20_frn = top[1:20,]




#################################Question_2#############################
trump = lookup_users("realDonaldTrump")
trump$followers_count


trump_flw <- get_followers("realDonaldTrump", n = 75000 , retryonratelimit = TRUE)

trump_flw_data <- lookup_users(trump_flw$user_id)



tmp = (trump_flw_data[,c('screen_name','followers_count','user_id')])
top_3_frn = top_20_frn[1:3,]

l1 = c(top_3_frn$user_id)

for (p in l1)
{
  fds_1 <- get_friends(p)
  common = intersect(fds_1$user_id,trump$user_id)
  print("Also a follower")

}

tmp = (trump_flw_data[,c('screen_name','followers_count','user_id')])
trump_flw_data_all = rbind(tmp,top_3_frn)

top = trump_flw_data_all[order(-trump_flw_data_all$followers_count),]

top_20_flw = top[1:20,]

#positive or negative relationship with Trump based on their tweets
library(sentimentr)
relationships <- list()

for (person in top_20_frn$screen_name[1:20])
{
  flag = 0
  tweets <- get_timeline(person, n = 200)
  df_tweets = data.frame((sapply(tweets,c)))
  for (i in 1:nrow(df_tweets))  {
    if("realDonaldTrump" %in% df_tweets[i, "mentions_screen_name"]){
      sentiment=sentiment_by(paste(unlist(df_tweets[i, "text"]), collapse=''))
      if((sentiment$ave_sentiment > 0) || (grepl("Republican", paste(unlist(df_tweets[i, "text"]),
                                                                     collapse=''), fixed = TRUE))) {
        flag = 1
        print(tweet)
        print(c((person) ,"Positive"))
        break()
      }
      else if((sentiment$ave_sentiment < 0) || (grepl("Democrat", paste(unlist(df_tweets[i, "text"]),
                                                                        collapse=''), fixed = TRUE))) {
        flag = 1
        print(tweet)
        print(c((person) ,"Negative"))
        break()
        
      }
    }
  }
  if(flag == 0){
    print(c((person) ,"Negative"))
  }
}

relationship_flw = matrix(c(
  "VP","IvankaTrump","DonaldJTrumpJr","dhruv_rathee","agustincreevy",
  "smow_alshaikh","AlionaVilani","SajidChaudhry_",
  "Rajayogiahgtwa1","chefmattmoran","Nawateshita5","segueotorto",
  "_andre_88_","kahraman3406","WorkingBarbie","iamLokko_",
  "Psmithran","FestivalsGifts","martinyeza","thatNAchick",
  "Positive","Positive","Positive","Negative","Negative",
  "Negative","Negative","Negative","Positive","Negative",
  "Negative","Negative","Negative",
  "Negative","Positive","Negative","Positive",
  "Negative","Negative","Positive"
                  ), ncol=2)
colnames(relationship_flw) <- c("screen_name", "Relationship")

##########for frnds ################




for (person in top_20_frn$screen_name[1:20])
{
  flag = 0
  tweets <- get_timeline(person, n = 200)
  for (tweet in tweets$mentions_screen_name)
  {
    if("realDonaldTrump" %in% c(tweet)){
      flag = 1
      print(c((person) ,"Positive"))
      break()
    }
    
  }
  if(flag == 0){
    print(c((person) ,"Negative"))
  }
}

relationship_frn = matrix(c("VP"
                            ,"IvankaTrump"
                            ,"DonaldJTrumpJr"
                            ,"seanhannity"
                            ,"PressSec"
                            ,"Mike_Pence"
                            ,"EricTrump"
                            ,"IngrahamAngle"
                            ,"TuckerCarlson"
                            ,"KellyannePolls"
                            ,"BillOReilly"
                            ,"VinceMcMahon"
                            ,"JudgeJeanine"
                            ,"DiamondandSilk"
                            ,"Jim_Jordan"
                            ,"foxandfriends"
                            ,"JesseBWatters"
                            ,"greta"
                            ,"MELANIATRUMP"
                            ,"ericbolling",
                            "Positive"
                            ,"Positive"
                            ,"Positive"
                            ,"Negative"
                            ,"Positive"
                            ,"Positive"
                            ,"Positive"
                            ,"Positive"
                            ,"Negative"
                            ,"Positive"
                            ,"Negative"
                            ,"Negative"
                            ,"Positive"
                            ,"Positive"
                            ,"Positive"
                            ,"Negative"
                            ,"Positive"
                            ,"Negative"
                            ,"Positive"
                            ,"Positive"

), ncol=2)
colnames(relationship_frn) <- c("screen_name", "Relationship")


##########################quesion3#####################3

# To check if any one of these followers is friends with trump's top friends
l1 = c(top_20_flw$user_id)

for (p in l1[1:20])
{
  fds_1 <- get_friends(p)
  common = intersect(fds_1$user_id,top_20_frn$user_id)
  if(length(common)!= 0) 
  {
    print(c(lookup_users(p)$screen_name))
    print(" ***** is friends with ******* ")
    for (elm in common){
      print(lookup_users(elm)$screen_name)
    }
  }
  print('**********************')
}




common = intersect(top_20_frn$screen_name,top_20_flw$screen_name)

#Plot the graph containing Trump's 20 friends and 20 followers. 

all = as.data.frame(rbind(c('Friends', 'Followers', 'Black')))
names(all) =c('Friends', 'Followers', 'color')
relations1 = merge(data.frame(user='Friends', friend =top_20_frn$screen_name, color = 'blue' ),
                   data.frame(user='Followers', friend =top_20_flw$screen_name, color = 'red' ), 
                   all = T
                   )

g1 =  graph.data.frame(relations1, directed = FALSE)

g1 <- g1 + edges(
  c("IvankaTrump","DonaldJTrumpJr"),
  c("IvankaTrump","PressSec"),
  c("DonaldJTrumpJr","PressSec"),
  c("VP","Mike_Pence"),
  c("Mike_Pence","DonaldJTrumpJr"),
  c("Mike_Pence","IvankaTrump"),
  c("EricTrump","DonaldJTrumpJr"),
  c("IvankaTrump","EricTrump"),
  c("DonaldJTrumpJr","IngrahamAngle"),
  c("IngrahamAngle","IvankaTrump"),
  c("KellyannePolls","VP"),
  c("KellyannePolls","DonaldJTrumpJr"),
  c("KellyannePolls","IvankaTrump"),
  c("BillOReilly","DonaldJTrumpJr"),
  c("JudgeJeanine","DonaldJTrumpJr"),
  c("JudgeJeanine","IvankaTrump"),
  c("DiamondandSilk","IvankaTrump"),
  c("DiamondandSilk","DonaldJTrumpJr"),
  c("IvankaTrump","Jim_Jordan"),
  c("Jim_Jordan","DonaldJTrumpJr"),
  c("foxandfriends","IvankaTrump"),
  c("foxandfriends","DonaldJTrumpJr"),
  c("JesseBWatters","DonaldJTrumpJr"),
  c("JesseBWatters","IvankaTrump"),
  c("greta","DonaldJTrumpJr"),
  c("IvankaTrump","greta"),
  c("MELANIATRUMP","IvankaTrump"),
  c("ericbolling","IvankaTrump"),
  c("DonaldJTrumpJr","ericbolling"), color="green")

g1 <- g1 + edges(
  c("AlionaVilani","seanhannity")
, color="brown")

V(g1)$name
V(g1)$label = V(g1)$name
par(mar = c(0,0,0,0)) 
V(g1)$label.cex = 0.7


plot(g1, layout = layout.fruchterman.reingold, vertex_size = 5, edge.color = E(g1)$color,
     edge.label.font=5,vertex.shape="circle", vertex.color="skyblue",edge.label.color="pink",
     vertex.arrow.size=0.5,  edge.curved=F,
     vertex.label.color= "black",vertex.label.font= 15,asp = 0.99,
     )

##########################quesion4#####################3

graph.density(g1)

diameter(g1)

gn = neighborhood(g1, order = 1)

## get pair of nodes that are at the end of each edge.
g1.ends = ends(g1, E(g1),names= FALSE)

# number of edges
N = nrow(g1.ends)
# make space for neighbourhood overlap score
NO = rep(0, N)

for (a in 1:N) {
  ## for every edge
  
  x = g1.ends[a,1] # x is the node at one end of the edge
  y = g1.ends[a,2] # y is the node at the other end of the edge
  
  ## compute the intersection of the neighbourhoods of x and y
  i = length(intersect(gn[[x]], gn[[y]])) - 2
  ## compute the union of the neighbourhoods of x and y
  u = length(union(gn[[x]], gn[[y]])) - 2
  
  ## Note that we subtract 2 since each neighbourhood includes x and y.
  ## we don't want to include x and y in the counts.
  
  ## the neighbourhood overlap is the intersection/union
  NO[a] = i/u
}


#5 lab 3 
#####################question 5###########################

V(g1)$label = c("S","S","N", "N","N","N","N","S",
                "N","N","S","N","N","N","S","S",
                "N","N","N","S","S","S","N","S","S",
                "S","N","N","S","S","S","S","S","S","S",
                "S","N","N","N")

plot(g1, layout = layout.fruchterman.reingold, vertex_size = 20, edge.color = E(g1)$color,
     edge.label.font=5,vertex.shape="circle", vertex.color="skyblue",edge.label.color="pink",
     vertex.arrow.size=0.5,  edge.curved=F,
     vertex.label.color= "black",vertex.label.font= 15,asp = 0.99,
)


class = c("S","S","N", "N","N","N","N","S",
          "N","N","S","N","N","N","S","S",
          "N","N","N","S","S","S","N","S","S",
          "S","N","N","S","S","S","S","S","S","S",
          "S","N","N","N")
## extract the adjacency matrix
A = get.adjacency(g1)

## repeat 1000 times
R = 10000

permuteAndCountCrossEdges = function(A, class) {
  ### permute the row and column labels
  permutedClass = sample(class)
  ### record the number of cross S-N edges
  sPos = which(permutedClass == "S")
  nPos = which(permutedClass == "N")
  return(sum(A[sPos, nPos]))
}

crossEdgeCount = replicate(R, permuteAndCountCrossEdges(A, class))


## examine the cross edge distribution
par(mar = c(2, 2, 3, 2))
hist(crossEdgeCount,10)

## compare the number of cross S-N edges from the data to the 
## random distribution
sPos = which(class == "S")
nPos = which(class == "N")
dataCrossEdgeCount = sum(A[sPos, nPos])

## If the they look the same we cannot reject H0
## If they look different, we can reject, meaning there is homophily
pVal = mean(crossEdgeCount < dataCrossEdgeCount)
## p value is very small, so reject H0: there is no homophily,
## therefore there must be homophily

#####################question 6###########################

M = get.adjacency(g1)
M["seanhannity","Friends"] = -1
M["Friends","seanhannity"] = -1

M["TuckerCarlson","Friends"] = -1
M["Friends","TuckerCarlson"] = -1

M["BillOReilly","Friends"] = -1
M["Friends","BillOReilly"] = -1

M["VinceMcMahon","Friends"] = -1
M["Friends","VinceMcMahon"] = -1

M["foxandfriends","Friends"] = -1
M["Friends","foxandfriends"] = -1

M["greta","Friends"] = -1
M["Friends","greta"] = -1

M["dhruv_rathee","Friends"] = -1
M["Friends","dhruv_rathee"] = -1

M["agustincreevy","Friends"] = -1
M["Friends","agustincreevy"] = -1

M["smow_alshaikh","Friends"] = -1
M["Friends","smow_alshaikh"] = -1

M["AlionaVilani","Friends"] = -1
M["Friends","AlionaVilani"] = -1

M["SajidChaudhry_","Friends"] = -1
M["Friends","SajidChaudhry_"] = -1

M["chefmattmoran","Friends"] = -1
M["Friends","chefmattmoran"] = -1

M["Nawateshita5","Friends"] = -1
M["Friends","Nawateshita5"] = -1

M["segueotorto","Friends"] = -1
M["Friends","segueotorto"] = -1

M["_andre_88_","Friends"] = -1
M["Friends","_andre_88_"] = -1

M["kahraman3406","Friends"] = -1
M["Friends","kahraman3406"] = -1

M["iamLokko_","Friends"] = -1
M["Friends","iamLokko_"] = -1

M["FestivalsGifts","Friends"] = -1
M["Friends","FestivalsGifts"] = -1

M["martinyeza","Friends"] = -1
M["Friends","martinyeza"] = -1

M["greta","DonaldJTrumpJr"] = -1
M["DonaldJTrumpJr","greta"] = -1

M["greta","IvankaTrump"] = -1
M["IvankaTrump","greta"] = -1

graph_from_adjacency_matrix 


D = as.matrix(1 - M) ## create a matrix of distances
image(D) ## visualise the distances
## Notice the block structure
h = hclust(as.dist(D), method = "single")
plot(h)
image(D[h$order, h$order])


## posEdges to deal with missing edges
posEdges = function(X) {
  X = as.matrix(X)
  ## check of off diagonal zeros
  z = sum(X == 0) - sum(diag(X) == 0)
  if (z > 0) {
    return(NA)
  }
  ## count diagonal
  d = sum(diag(X) == 1)
  ## count all
  a = sum(X == 1)
  ## remove diagonal and divide by two to count the upper triangle
  return((a - d)/2)
}

positiveEdgeCount = function(M) {
  ## Compute the number of positive edges for each set of three nodes. M is an
  ## adjacency matrix (symmetric) containing only +1 and -1.
  n = nrow(M)
  p = choose(n, 3)
  counter = 0
  for (a in 1:(n - 2)) {
    for (b in (a + 1):(n - 1)) {
      for (d in (b + 1):n) {
        counter = counter + 1
        m = M[c(a, b, d), c(a, b, d)]
        p[counter] = posEdges(m)
      }
    }
  }
  return(p)
}

## table does not report NA, so we must add it
c(sum(is.na(positiveEdgeCount(M))), table(positiveEdgeCount(M)))










langbiglatin <- NULL
langbiglatin$name <- c("English","Frisian","Dutch","Flemish","Afrikaans","Plattdeutsch",
                       "Mod Std German","Icelandic","Faroese","Norwegian",
                       "Danish","Swedish","Welsh","Breton","Scots Gaelic","Irish Gaelic",
                       "Portuguese","Spanish","French","Italian","Romanian","Albanian",
                       "Latvian","Lithuanian","Slovenian","Polish","Czech","Slovak")
langbiglatin$one <- c("one","iinj","één","èèn","een","een","eins","einn","ein","én","en","en",
                      "un","unan","aon","aon","um","uno","un","uno","un","një",
                      "viens","vienas","ena","jeden","jeden","jeden")
langbiglatin$two <- c("two","tou","twee","tweje","twee","twee","zwei","tveir","tveir","to","to","två",
                      "dau","daou","dà","dó","dois","dos","deux","due","doi","dy",
                      "divi","du","dve","dwa","dva","dva")
langbiglatin$three <- c("three","tri","drie","drieje","drie","dree","drei","þrír","tríggir","tre","tre","tre",
                        "tri","tri","trì","trí","três","tres","trois","tre","trei","tre",
                        "trīs","trys","tri","trzy","tři","tri")
langbiglatin$four <- c("four","fjouer","vier","viere","vier","veer","vier","fjórir","fýra","fire","fire","fyra",
                       "pedwar","pevar","ceithir","ceithair","quatro","cuatro","quatre","quattro","patru","katër",
                       "četri","keturi","štiri","cztery","čtyři","štyri")
langbiglatin$five <- c("five","fiiw","vijf","vuve","vyf","fief","fünf","fimm","fimm","fem","fem","fem",
                       "pum","pemp","còig","cúig","cinco","cinco","cinq","cinque","cinci","pesë",
                       "pieci","penki","pet","pięć","pĕt","päť")
langbiglatin$six <- c("six","seeks","zes","zesse","ses","söss","sechs","sex","seks","seks","seks","sex",
                      "chwe","c'hwec'h","sia","sé","seis","seis","six","sei","șase","gjashtë",
                      "seši","šeši","šest","sześć","šest","šesť")
langbiglatin$seven <- c("seven","soowen","zeven","zeevn","sewe","söben","sieben","sjö","sjey","sju","syv","sju",
                        "saith","seizh","seachd","seacht","sete","siete","sept","sette","șapte","shtatë",
                        "septiņi","septyni","sedem","siedem","sedm","sedem")
langbiglatin$eight <- c("eight","oocht","acht","achte","ag","acht","acht","átta","átta","åtte","otte","åtta",
                        "wyth","eizh","ochd","ocht","oito","ocho","huit","otto","opt","tetë",
                        "astoņi","aštuoni","osem","osiem","osm","osem")
langbiglatin$nine <- c("nine","nüügen","negen","neegn","nege","negen","neun","níu","níggju","ni","ni","nio",
                       "naw","nav","naoi","naoi","nove","nueve","neuf","nove","nouă","nëntë",
                       "deviņi","devyni","devet","dziewięć","devĕt","deväť")
langbiglatin$ten <- c("ten","tiin","tien","tienne","tien","teihn","zehn","tíu","tíggju","ti","ti","tio",
                      "deg","dek","deich","deich","dez","diez","dix","dieci","zece","dhjetë",
                      "desmit","dešimt","deset","dziesięć","deset","desať")
langd_big <- data.frame(langbiglatin[2:11],row.names=langbiglatin$name)
#find the max number of characters in each column?
#in this example it is 70, but figure out to do that
#then use this in the distance calculations to scale
#how to calculate all these distances...
langbigdist <- matrix(0,nrow=28,ncol=28)
n <- 0
for(i in 1:28){
  for(k in 0:(28-i)){
    for(j in 1:10){
      n <- n + stringdist(langd_big[i,j],langd_big[i+k,j],method='lv')/70*100
    }
    langbigdist[i+k,i] <- n
    n <- 0
  }
}
colnames(langbigdist) <- langbiglatin$name
rownames(langbigdist) <- langbiglatin$name
lcd_big <- as.dist(langbigdist)

singclustbig <- hclust(lcd_big,method="single")
compclustbig <- hclust(lcd_big,method="complete")
avgclustbig <- hclust(lcd_big,method="average")
par(mfrow=c(1,1))
plot(singclustbig,hang=-0.01)
plot(compclustbig,hang=-0.01)
plot(avgclustbig,hang=-0.01)


string1 <- "narrow"
string2 <- "étroit"
stringdist(string1, string2, method = 'lv')

string3 <- "heart"
string4 <- "cœur"
string5 <- "herz"
string6 <- "се́рдце"
string7 <- "се́рце"
string8 <- "καρδιά"
string9 <- "kardia"
string10 <- "sérdc"
stringdist(string3,string4,method='lv')
stringdist(string3,string5,method='lv')
stringdist(string3,string6,method='lv')
stringdist(string4,string6,method='lv')
stringdist(string4,string5,method='lv')
stringdist(string6,string7,method='lv')
stringdist(string3,string8,method='lv')
stringdist(string6,string8,method='lv')
stringdist(string3,string9,method='lv')
stringdist(string3,string10,method='lv')
stringdist(string10,string3,method='lv')

swadesh3 <- NULL
swadesh3$English = c("English","i", "you", "he","we", "you", "they", "this", "that", "here", "there", "who", "what", "where", "when", "how", "not", "all", "many", "some", "few", "other", "one", "two", "three", "four", "five", "big", "long", "wide", "thick", "heavy", "small", "short", "narrow", "thin", "woman", "man", "man", "child", "wife", "husband", "mother", "father", "animal", "fish", "bird", "dog", "louse", "snake", "worm", "tree", "forest", "stick", "fruit", "seed", "leaf", "root", "bark", "flower", "grass", "rope", "skin", "meat", "blood", "bone", "fat", "egg", "horn", "tail", "feather", "hair", "head", "ear", "eye", "nose", "mouth", "tooth", "tongue", "fingernail", "foot", "leg", "knee", "hand", "wing", "belly", "guts", "neck", "back", "breast", "heart", "liver", "drink", "eat", "bite", "suck", "spit", "vomit", "blow", "breathe", "laugh", "see", "hear", "know", "think", "smell", "fear", "sleep", "live", "die", "kill", "fight", "hunt", "hit", "cut", "split", "stab", "scratch", "dig", "swim", "fly", "walk", "come", "lie", "sit", "stand", "turn", "fall", "give", "hold", "squeeze", "rub", "wash", "wipe", "pull", "push", "throw", "tie", "sew", "count", "say", "sing", "play", "float", "flow", "freeze", "swell", "sun", "moon", "star", "water", "rain", "river", "lake", "sea", "salt", "stone", "sand", "dust", "earth", "cloud", "fog", "sky", "wind", "snow", "ice", "smoke", "fire", "ash", "burn", "road", "mountain", "red", "green", "yellow", "white", "black", "night", "day", "year", "warm", "cold", "full", "new", "old", "good", "bad", "rotten", "dirty", "straight", "round", "sharp", "dull", "smooth", "wet", "dry", "correct", "near", "far", "right", "left", "at", "in", "with", "and", "if", "because", "name")

swadesh3$French = c("French","je","tu","il","nous","vous","ils","ceci","cela","ici","là","qui","quoi","où","quand","comment","ne pas","tout", "beaucoup","qualques","peu","autre","un","deux","trois","quatre","cinq","grand","long","large","épais","lourd","petit","court","étroit","mince","femme","homme","homme","enfant","femme","mari","mère","père","animal","poisson","oiseau","chien","pou","serpent","ver","arbre","forêt","bâton","fruit","graine","feuille","racine","écorce","fleur","herbe","corde","peau","viande","sang","os","graisse","œuf","corne","queue","plume","cheveux","tête","oreille","œil","nez","bouche","dent","langue","ongle","pied","jambe","genou","main","aile","ventre","entrailles","cou","dos","sein","cœur","foie","boire","manger","mordre","sucer","cracher","vomir","souffler","respirer","rire","voir","entendre","savoir","penser","sentir","craindre","dormir","vivre","mourir","tuer","se battre","chasser","frapper","couper","fendre","poignarder","gratter","creuser","nager","voler","marcher","venir","s'étendre","s'asseoir","se lever","tourner","tomber","donner","tenir","serrer","frotter","laver","essuyer","tirer","pousser","jeter","lier","coudre","compter","dire","chanter","jouer","flotter","couler","geler","gonfler","soleil","lune","étoile","eau","pluie","rivière","lac","mer","sel","pierre","sable","poussière","terre","nuage","brouillard","ciel","vent","neige","glace","fumée","feu","cendre","brûler","route","montagne","rouge","vert","jaune","blanc","noir","nuit","jour","an","chaud","froid","plein","nouveau","vieux","bon","mauvais","pourri","sale","droit","rond","tranchant","émoussé","lisse","mouillé","sec","juste","près","loin","droite","gauche","à","dans","avec","et","si","parce que","nom")
swadesh3$German = c("German","ich","du","er","wir","ihr","sie","dieser","der","hier","da","wer","was","wo","wann","wie","nicht","alle","viele","einige","wenig","anderer","eins","zwei","drei","vier","fünf","groß","lang","breit","dick","schwer","klein","kurz","eng","dünn","frau","mann","mensch","kind","frau","mann","mutter","vater","tier","fisch","vogel","hund","laus","schlange","wurm","baum","forst","stock","frucht","samen","blatt","wurzel","borke","blume","gras","leine","haut","fleisch","blut","knochen","fett","ei","horn","schwanz","feder","haar","haupt","ohr","auge","nase","mund","zahn","zunge","fingernagel","fuß","bein","knie","hand","fittich","bauch","eingeweide","hals","rücken","brust","herz","leber","trinken","essen","beißen","lutschen","speien","brechen","blasen","atmen","lachen","sehen","hören","kennen","denken","riechen","fürchten","schlafen","leben","sterben","töten","fechten","jagen","schlagen","schneiden","spalten","stechen","kratzen","graben","schwimmen","fliegen","gehen","kommen","liegen","sitzen","stehen","drehen","fallen","geben","halten","quetschen","reiben","waschen","wischen","ziehen","drücken","werfen","binden","nähen","rechnen","sagen","singen","spielen","schweben","fließen","frieren","schwellen","sonne","mond","stern","wasser","regen","fluss","see","meer","salz","stein","sand","staub","erde","wolke","nebel","himmel","wind","schnee","eis","rauch","feuer","asche","breenen","straße","berg","rot","grün","gelb","weiß","schwarz","nacht","tag","jahr","warm","kalt","voll","neu","alt","gut","schlecht","faul","schmutzig","gerade","rund","scharf","stumpf","glatt","nass","dürr","richtig","nah","fern","rechst","links","an","in","mit","und","ob","weil","name")
#create the data frames
langswad3 <- data.frame(swadesh3[1:3])
langswad3$E1 <- substr(langswad3$English,1,1)
langswad3$F1 <- substr(langswad3$French,1,1)
langswad3$G1 <- substr(langswad3$German,1,1)
langswad3$EF[langswad3$E1==langswad3$F1] <- 0
langswad3$EF[langswad3$E1!=langswad3$F1] <- 1
langswad3$EG[langswad3$E1==langswad3$G1] <- 0
langswad3$EG[langswad3$E1!=langswad3$G1] <- 1
langswad3$FG[langswad3$F1==langswad3$G1] <- 0
langswad3$FG[langswad3$F1!=langswad3$G1] <- 1
langswad3 = langswad3[-1,]
#randomize order of words
set.seed(1107231) #date 11/1/23 tree 1
langswad3$rand <- runif(nrow(langswad3),0,1)
langswad3sort <-langswad3[order(langswad3$rand),]
#split into 2 different populations
ls1 <- langswad3sort[1:105,]
ls2 <- langswad3sort[106:207,]
#assign random numbers
ls1$rand <- runif(nrow(ls1),0,1)
ls2$rand <- runif(nrow(ls2),0,1)
#sort smallest to largest
ls1sort <- ls1[order(ls1$rand),]
ls2sort <- ls2[order(ls2$rand),]
#create samples of size 3 and sum the distances
ls1d <- matrix(0,nrow = nrow(ls1sort)/3,ncol = 3)
ls2d <- matrix(0,nrow = nrow(ls2sort)/3,ncol = 3)
n=1
for(i in 1:(nrow(ls1sort)/3)){ 
	ls1d[i,1] <- sum(ls1sort$EF[n:(n+2)]) 
	ls1d[i,2] <- sum(ls1sort$EG[n:(n+2)]) 
	ls1d[i,3] <- sum(ls1sort$FG[n:(n+2)]) 
	n <- n+3 
	}
n=1
for(i in 1:(nrow(ls2sort)/3)){
  ls2d[i,1] <- sum(ls2sort$EF[n:(n+2)])
  ls2d[i,2] <- sum(ls2sort$EG[n:(n+2)])
  ls2d[i,3] <- sum(ls2sort$FG[n:(n+2)])
  n <- n+3
}
ls1df <- data.frame(ls1d)
colnames(ls1df) <- c("EF","EG","FG")
ls2df <- data.frame(ls2d)
colnames(ls2df) <- c("EF","EG","FG")
ls1da <- matrix(0,nrow = nrow(ls1df),ncol = 3)
ls2da <- matrix(0,nrow = nrow(ls2df),ncol = 3)
ls1da <- data.frame(ls1da)
colnames(ls1da) <- c("EFd","EGd","FGd")
ls2da <- data.frame(ls2da)
colnames(ls2da) <- c("EFd","EGd","FGd")
ls1df <- cbind(ls1df,ls1da)
ls2df <- cbind(ls2df,ls2da)
#distances
#origin then EF min, EG min, FG min, no unique min: EF=EG, EF=FG,EG=FG
n=1
for(i in 1:nrow(ls1df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls1df[i,1],ls1df[i,2],ls1df[i,3]))==0){
    ls1df[i,4] <- 0
    ls1df[i,5] <- 0
    ls1df[i,6] <- 0
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]<min(ls1df[i,2:3]))){
    ls1df[i,4] <- min(ls1df[i,2:3])-ls1df[i,1]
    ls1df[i,5] <- -(min(ls1df[i,2:3])-ls1df[i,1])
    ls1df[i,6] <- -(min(ls1df[i,2:3])-ls1df[i,1])
  }
  else if((ls1df[i,2]==min(ls1df[i,1:3]))&(ls1df[i,2]<min(c(ls1df[i,1],ls1df[i,3])))){
    ls1df[i,4] <- -(min(c(ls1df[i,1],ls1df[i,3]))-ls1df[i,2])
    ls1df[i,5] <- min(c(ls1df[i,1],ls1df[i,3]))-ls1df[i,2]
    ls1df[i,6] <- -(min(c(ls1df[i,1],ls1df[i,3]))-ls1df[i,2])
  }
  else if((ls1df[i,3]==min(ls1df[i,1:3]))&(ls1df[i,3]<min(ls1df[i,1:2]))){
    ls1df[i,4] <- -(min(ls1df[i,1:2])-ls1df[i,3])
    ls1df[i,5] <- -(min(ls1df[i,1:2])-ls1df[i,3])
    ls1df[i,6] <- min(ls1df[i,1:2])-ls1df[i,3]
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,2])&r<0.5){
    ls1df[i,4] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,2])&r>0.5){
    ls1df[i,4] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,3])&r<0.5){
    ls1df[i,4] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,3])&r>0.5){
    ls1df[i,4] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
  else if(r<0.5){
    ls1df[i,4] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
  else{
    ls1df[i,4] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,5] <- -(max(ls1df[i,1:3])-min(ls1df[i,1:3]))
    ls1df[i,6] <- (max(ls1df[i,1:3])-min(ls1df[i,1:3]))
  }
}

for(i in 1:nrow(ls2df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls2df[i,1],ls2df[i,2],ls2df[i,3]))==0){
    ls2df[i,4] <- 0
    ls2df[i,5] <- 0
    ls2df[i,6] <- 0
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]<min(ls2df[i,2:3]))){
    ls2df[i,4] <- min(ls2df[i,2:3])-ls2df[i,1]
    ls2df[i,5] <- -(min(ls2df[i,2:3])-ls2df[i,1])
    ls2df[i,6] <- -(min(ls2df[i,2:3])-ls2df[i,1])
  }
  else if((ls2df[i,2]==min(ls2df[i,1:3]))&(ls2df[i,2]<min(c(ls2df[i,1],ls2df[i,3])))){
    ls2df[i,4] <- -(min(c(ls2df[i,1],ls2df[i,3]))-ls2df[i,2])
    ls2df[i,5] <- min(c(ls2df[i,1],ls2df[i,3]))-ls2df[i,2]
    ls2df[i,6] <- -(min(c(ls2df[i,1],ls2df[i,3]))-ls2df[i,2])
  }
  else if((ls2df[i,3]==min(ls2df[i,1:3]))&(ls2df[i,3]<min(ls2df[i,1:2]))){
    ls2df[i,4] <- -(min(ls2df[i,1:2])-ls2df[i,3])
    ls2df[i,5] <- -(min(ls2df[i,1:2])-ls2df[i,3])
    ls2df[i,6] <- min(ls2df[i,1:2])-ls2df[i,3]
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,2])&r<0.5){
    ls2df[i,4] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,5] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
    ls2df[i,6] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,2])&r>0.5){
    ls2df[i,4] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
    ls2df[i,5] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,6] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,3])&r<0.5){
    ls2df[i,4] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
    ls2df[i,5] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,6] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,3])&r>0.5){
    ls2df[i,4] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,5] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,6] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
  }
  else if(r<0.5){
    ls2df[i,4] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,5] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
    ls2df[i,6] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
  }
  else{
    ls2df[i,4] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,5] <- -(max(ls2df[i,1:3])-min(ls2df[i,1:3]))
    ls2df[i,6] <- max(ls2df[i,1:3])-min(ls2df[i,1:3])
  }
}
colMeans(ls1df[,4:6])
colMeans(ls2df[,4:6])
#use this for sample variance if non-sticky
max(diag(var(ls1df[,4:6])))
max(diag(var(ls2df[,4:6])))
#use these for sample variance if sticky
sum(ls1df[,4]^2)/(nrow(ls1df)-1)
sum(ls1df[,5]^2)/(nrow(ls1df)-1)
sum(ls1df[,6]^2)/(nrow(ls1df)-1)

library(boot)
meanlang.boot <- function(data,f)  mean(data*f)
english1.boot <- boot(ls1df[,6],meanlang.boot,R=999,stype = "f")
french1.boot <- boot(ls1df[,5],meanlang.boot,R=999,stype="f")
german1.boot <- boot(ls1df[,4],meanlang.boot,R=999,stype="f")
english2.boot <- boot(ls2df[,6],meanlang.boot,R=999,stype = "f")
french2.boot <- boot(ls2df[,5],meanlang.boot,R=999,stype="f")
german2.boot <- boot(ls2df[,4],meanlang.boot,R=999,stype="f")


boot.ci(english1.boot,conf=0.95)
boot.ci(french1.boot,conf=0.95)
boot.ci(german1.boot,conf=0.95)
boot.ci(english2.boot,conf=0.95)
boot.ci(french2.boot,conf=0.95)
boot.ci(german2.boot,conf=0.95)

#different 3 languages
swadesh4 <- NULL
swadesh4$Spanish = c("Spanish","yo", "tú", "él","nosotros", 
                     "vosotros", "ellos", "esta", "ese", "aquí", "ahí", 
                     "quién", "qué", "dónde", "cuándo", "cómo", "no", 
                     "todo", "mucho", "alguno", "poco", "otro", "uno", 
                     "dos", "tres", "cuatro", "cinco", "grande", "largo", 
                     "ancho", "grueso", "pesado", "pequeño", "corto", "estrecho", 
                     "delgado", "mujer", "varón", "hombre", "niño", "esposa", 
                     "esposo", "madre", "padre", "animal", "pez", 
                     "pájaro", "perro", "piojo", "serpiente", "gusano", "árbol", 
                     "bosque", "palo", "fruta", "semilla", "hoja", "raíz", 
                     "corteza", "flor", "hierba", "cuerda", "piel", "carne", 
                     "sangre", "hueso", "grasa", "huevo", "cuerno", "cola", 
                     "pluma", "pelo", "cabeza", "oreja", "ojo", "nariz", 
                     "boca", "dienta", "lengua", "uña", "pie", 
                     "pierna", "rodilla", "mano", "alo", "barriga", "tripas", 
                     "cuello", "espalda", "pecho", "corazón", "hígado", "beber", 
                     "comer", "morder", "chupar", "escupir", "vomitar", 
                     "soplar", "respirar", "reír", "ver", "oír", 
                     "saber", "pensar", "oler", "temer", "dormir", 
                     "vivir", "morir", "matar", "pelear", "cazar", 
                     "golpear", "cortar", "partir", "apuñalar", "arañar", 
                     "cavar", "nadar", "volar", "caminar", "venir", 
                     "echarse", "sentarse", "levantarse", 
                     "girar", "caer", "dar", "tener", 
                     "apretar", "frotar", "lavar", "limpiar", "tirar", 
                     "empujar", "tirar", "atar", "coser", "contar", 
                     "decir", "cantar", "jugar", "flotar", "fluir", 
                     "congelarse", "hincharse", "sol", "luna", "estrella", 
                     "agua", "lluvia", "río", "lago", "mar", "sal", 
                     "piedra", "arena", "polvo", "tierra", "nube", "niebla", 
                     "cielo", "viento", "nieve", "hielo", "humo", "fuego", 
                     "cenizas", "quemar", "camino", "montaño", "rojo", "verde", 
                     "amarillo", "blanco", "negro", "noche", "día", "año", 
                     "caliente", "frío", "lleno", "nuevo", "viejo", "bueno", 
                     "malo", "podrido", "sucio", "recto", "redondo", 
                     "agudo", "desafilado", "liso", "mojado", 
                     "seco", "correcto", "cerca", "lejos", "derecha", "izquierda", 
                     "a", "en", "con", "y", "si", "porque", "nombre")

swadesh4$French = c("French","je","tu","il","nous","vous","ils","ceci","cela",
                    "ici","là","qui","quoi","où","quand","comment","ne pas","tout",
                    "beaucoup","qualques","peu","autre","un","deux","trois","quatre",
                    "cinq","grand","long","large","épais","lourd","petit","court",
                    "étroit","mince","femme","homme","homme","enfant","femme",
                    "mari","mère","père","animal","poisson","oiseau","chien",
                    "pou","serpent","ver","arbre","forêt","bâton","fruit",
                    "graine","feuille","racine","écorce","fleur","herbe","corde",
                    "peau","viande","sang","os","graisse","œuf","corne","queue",
                    "plume","cheveux","tête","oreille","œil","nez","bouche",
                    "dent","langue","ongle","pied","jambe","genou","main","aile",
                    "ventre","entrailles","cou","dos","sein","cœur","foie","boire",
                    "manger","mordre","sucer","cracher","vomir","souffler",
                    "respirer","rire","voir","entendre","savoir","penser",
                    "sentir","craindre","dormir","vivre","mourir","tuer",
                    "se battre","chasser","frapper","couper","fendre","poignarder",
                    "gratter","creuser","nager","voler","marcher","venir",
                    "s'étendre","s'asseoir","se lever","tourner","tomber","donner",
                    "tenir","serrer","frotter","laver","essuyer","tirer","pousser",
                    "jeter","lier","coudre","compter","dire","chanter","jouer",
                    "flotter","couler","geler","gonfler","soleil","lune","étoile",
                    "eau","pluie","rivière","lac","mer","sel","pierre","sable",
                    "poussière","terre","nuage","brouillard","ciel","vent",
                    "neige","glace","fumée","feu","cendre","brûler","route",
                    "montagne","rouge","vert","jaune","blanc","noir","nuit",
                    "jour","an","chaud","froid","plein","nouveau","vieux",
                    "bon","mauvais","pourri","sale","droit","rond","tranchant",
                    "émoussé","lisse","mouillé","sec","juste","près","loin",
                    "droite","gauche","à","dans","avec","et","si","parce que",
                    "nom")
swadesh4$Italian = c("Italian","io","tu","egli","noi","voi","loro","questo","quello",
                     "qui","lì","chi","che","dove","quando","come","non","tutto",
                     "tanto","alcuni","poco","altro","un","due","tre","quattro",
                     "cinque","grande","lungo","largo","spesso","pesante","piccolo","corto",
                     "stretto","sottile","donna","uomo","persona","bambino","moglie","marito",
                     "madre","padre","animale","pesce","uccello","cane","pidocchio","serpente",
                     "verme","albero","foresta","bastone","frutta","seme","foglia","radice","corteccia",
                     "fiore","erbe","corda","pelle","carne","sangue","osso","grasso",
                     "uovo","corno","coda","penna","capello","testa","orecchio","occhio",
                     "naso","bocca","dente","lingua","unghia","piede","gamba",
                     "ginocchio","mano","ala","pancia","intestino","collo",
                     "schiena","petto","cuore","fegato","bere","mangiare","mordere",
                     "succhiare","sputare","vomitare","soffiare","respirare","ridere","vedere",
                     "sentire","sapere","pensare","odorare","temere","dormire",
                     "vivere","morire","uccidere","combattere","cacciare","colpire",
                     "tagliare","dividere","pugnalare","graffiare","scavare",
                     "nuotare","volare","camminare","venire","distendersi","sedersi",
                     "stare","girare","cadere","dare","tenere","spremere",
                     "strofinare","lavare","asciugare","tirare","spingere","lanciare",
                     "legare","cucire","contare","dire","cantare","giocare",
                     "galleggiare","fluire","gelare","gonfiare","sole","luna",
                     "stella","acqua","pioggia","fiume","lago","mare","sale",
                     "sasso","sabbia","polvere","terra","nuvola","nebbia","cielo","vento",
                     "neve","ghiaccio","fumo","fuoco","cenere","bruciare","strada",
                     "montagna","rosso","verde","giallo","bianco","nero","notto","giorno",
                     "anno","caldo","freddo","pieno","nuovo","vecchio","buono","cattivo",
                     "marcio","sporco","dritto","rotondo","affilato","smussato","liscio",
                     "bagnato","secco","guisto","vicino","lontano","destra","sinistra","a",
                     "in","con","e","se","perché","nome")
#create the data frames
langswad4 <- data.frame(swadesh4[1:3])
langswad4$S1 <- substr(langswad4$Spanish,1,1)
langswad4$F1 <- substr(langswad4$French,1,1)
langswad4$I1 <- substr(langswad4$Italian,1,1)
langswad4$SF[langswad4$S1==langswad4$F1] <- 0
langswad4$SF[langswad4$S1!=langswad4$F1] <- 1
langswad4$SI[langswad4$S1==langswad4$I1] <- 0
langswad4$SI[langswad4$S1!=langswad4$I1] <- 1
langswad4$FI[langswad4$F1==langswad4$I1] <- 0
langswad4$FI[langswad4$F1!=langswad4$I1] <- 1
langswad4 = langswad4[-1,]
#randomize order of words
set.seed(1107232) #date 11/1/23 tree 2
langswad4$rand <- runif(nrow(langswad4),0,1)
langswad4sort <-langswad4[order(langswad4$rand),]
#split into 2 different populations
ls3 <- langswad4sort[1:105,]
ls4 <- langswad4sort[106:207,]
#assign random numbers
ls3$rand <- runif(nrow(ls3),0,1)
ls4$rand <- runif(nrow(ls4),0,1)
#sort smallest to largest
ls3sort <- ls3[order(ls3$rand),]
ls4sort <- ls4[order(ls4$rand),]
#create samples of size 3 and sum the distances
ls3d <- matrix(0,nrow = nrow(ls3sort)/3,ncol = 3)
ls4d <- matrix(0,nrow = nrow(ls4sort)/3,ncol = 3)
n=1
for(i in 1:(nrow(ls3sort)/3)){
  ls3d[i,1] <- sum(ls3sort$SF[n:(n+2)])
  ls3d[i,2] <- sum(ls3sort$SI[n:(n+2)])
  ls3d[i,3] <- sum(ls3sort$FI[n:(n+2)])
  n <- n+3
}
n=1
for(i in 1:(nrow(ls4sort)/3)){
  ls4d[i,1] <- sum(ls4sort$SF[n:(n+2)])
  ls4d[i,2] <- sum(ls4sort$SI[n:(n+2)])
  ls4d[i,3] <- sum(ls4sort$FI[n:(n+2)])
  n <- n+3
}
ls3df <- data.frame(ls3d)
colnames(ls3df) <- c("SF","SI","FI")
ls4df <- data.frame(ls4d)
colnames(ls4df) <- c("SF","SI","FI")
ls3da <- matrix(0,nrow = nrow(ls3df),ncol = 3)
ls4da <- matrix(0,nrow = nrow(ls4df),ncol = 3)
ls3da <- data.frame(ls3da)
colnames(ls3da) <- c("SFd","SId","FId")
ls4da <- data.frame(ls4da)
colnames(ls4da) <- c("SFd","SId","FId")
ls3df <- cbind(ls3df,ls3da)
ls4df <- cbind(ls4df,ls4da)
#distances
#origin then EF min, EG min, FG min, no unique min: EF=EG, EF=FG,EG=FG
n=1
for(i in 1:nrow(ls3df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls3df[i,1],ls3df[i,2],ls3df[i,3]))==0){
    ls3df[i,4] <- 0
    ls3df[i,5] <- 0
    ls3df[i,6] <- 0
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]<min(ls3df[i,2:3]))){
    ls3df[i,4] <- min(ls3df[i,2:3])-ls3df[i,1]
    ls3df[i,5] <- -(min(ls3df[i,2:3])-ls3df[i,1])
    ls3df[i,6] <- -(min(ls3df[i,2:3])-ls3df[i,1])
  }
  else if((ls3df[i,2]==min(ls3df[i,1:3]))&(ls3df[i,2]<min(c(ls3df[i,1],ls3df[i,3])))){
    ls3df[i,4] <- -(min(c(ls3df[i,1],ls3df[i,3]))-ls3df[i,2])
    ls3df[i,5] <- min(c(ls3df[i,1],ls3df[i,3]))-ls3df[i,2]
    ls3df[i,6] <- -(min(c(ls3df[i,1],ls3df[i,3]))-ls3df[i,2])
  }
  else if((ls3df[i,3]==min(ls3df[i,1:3]))&(ls3df[i,3]<min(ls3df[i,1:2]))){
    ls3df[i,4] <- -(min(ls3df[i,1:2])-ls3df[i,3])
    ls3df[i,5] <- -(min(ls3df[i,1:2])-ls3df[i,3])
    ls3df[i,6] <- min(ls3df[i,1:2])-ls3df[i,3]
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,2])&r<0.5){
    ls3df[i,4] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,5] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
    ls3df[i,6] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,2])&r>0.5){
    ls3df[i,4] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
    ls3df[i,5] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,6] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,3])&r<0.5){
    ls3df[i,4] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
    ls3df[i,5] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,6] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,3])&r>0.5){
    ls3df[i,4] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,5] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,6] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
  }
  else if(r<0.5){
    ls3df[i,4] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,5] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
    ls3df[i,6] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
  }
  else{
    ls3df[i,4] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,5] <- -(max(ls3df[i,1:3])-min(ls3df[i,1:3]))
    ls3df[i,6] <- max(ls3df[i,1:3])-min(ls3df[i,1:3])
  }
}

for(i in 1:nrow(ls4df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls4df[i,1],ls4df[i,2],ls4df[i,3]))==0){
    ls4df[i,4] <- 0
    ls4df[i,5] <- 0
    ls4df[i,6] <- 0
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]<min(ls4df[i,2:3]))){
    ls4df[i,4] <- min(ls4df[i,2:3])-ls4df[i,1]
    ls4df[i,5] <- -(min(ls4df[i,2:3])-ls4df[i,1])
    ls4df[i,6] <- -(min(ls4df[i,2:3])-ls4df[i,1])
  }
  else if((ls4df[i,2]==min(ls4df[i,1:3]))&(ls4df[i,2]<min(c(ls4df[i,1],ls4df[i,3])))){
    ls4df[i,4] <- -(min(c(ls4df[i,1],ls4df[i,3]))-ls4df[i,2])
    ls4df[i,5] <- min(c(ls4df[i,1],ls4df[i,3]))-ls4df[i,2]
    ls4df[i,6] <- -(min(c(ls4df[i,1],ls4df[i,3]))-ls4df[i,2])
  }
  else if((ls4df[i,3]==min(ls4df[i,1:3]))&(ls4df[i,3]<min(ls4df[i,1:2]))){
    ls4df[i,4] <- -(min(ls4df[i,1:2])-ls4df[i,3])
    ls4df[i,5] <- -(min(ls4df[i,1:2])-ls4df[i,3])
    ls4df[i,6] <- min(ls4df[i,1:2])-ls4df[i,3]
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,2])&r<0.5){
    ls4df[i,4] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,5] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
    ls4df[i,6] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,2])&r>0.5){
    ls4df[i,4] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
    ls4df[i,5] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,6] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,3])&r<0.5){
    ls4df[i,4] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
    ls4df[i,5] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,6] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,3])&r>0.5){
    ls4df[i,4] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,5] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,6] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
  }
  else if(r<0.5){
    ls4df[i,4] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,5] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
    ls4df[i,6] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
  }
  else{
    ls4df[i,4] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,5] <- -(max(ls4df[i,1:3])-min(ls4df[i,1:3]))
    ls4df[i,6] <- max(ls4df[i,1:3])-min(ls4df[i,1:3])
  }
}
colMeans(ls3df[,4:6])
colMeans(ls4df[,4:6])
#use this for sample variance if non-sticky
max(diag(var(ls3df[,4:6])))
max(diag(var(ls4df[,4:6])))
sum(ls3df[,4]^2)/(nrow(ls3df)-1)
sum(ls3df[,5]^2)/(nrow(ls3df)-1)
sum(ls3df[,6]^2)/(nrow(ls3df)-1)
sum(ls4df[,4]^2)/(nrow(ls4df)-1)
sum(ls4df[,5]^2)/(nrow(ls4df)-1)
sum(ls4df[,6]^2)/(nrow(ls4df)-1)

spanish1.boot <- boot(ls3df[,6],meanlang.boot,R=999,stype = "f")
french3.boot <- boot(ls3df[,5],meanlang.boot,R=999,stype="f")
italian1.boot <- boot(ls3df[,4],meanlang.boot,R=999,stype="f")
spanish2.boot <- boot(ls4df[,6],meanlang.boot,R=999,stype = "f")
french4.boot <- boot(ls4df[,5],meanlang.boot,R=999,stype="f")
italian2.boot <- boot(ls4df[,4],meanlang.boot,R=999,stype="f")

boot.ci(spanish1.boot,conf=0.95)
boot.ci(french3.boot,conf=0.95)
boot.ci(italian1.boot,conf=0.95)
boot.ci(spanish2.boot,conf=0.95)
boot.ci(french4.boot,conf=0.95)
boot.ci(italian2.boot,conf=0.95)


E1 <- c(0,3,3)
F1 <- c(3,0,3)
G1 <- c(3,3,0)
L1 <- cbind(E1,F1,G1)
L1d <- as.dist(L1)
cL1d <- hclust(L1d,method="single")
plot(as.dendrogram(cL1d))
E2 <- c(0,3,1)
F2 <- c(3,0,3)
G2 <- c(1,3,0)
L2 <- cbind(E2,F2,G2)
L2d <- as.dist(L2)
cL2d <- hclust(L2d,method="single")
plot(as.dendrogram(cL2d))
E3 <- c(0,2,1)
F3 <- c(2,0,3)
G3 <- c(1,3,0)
L3 <- cbind(E3,F3,G3)
L3d <- as.dist(L3)
cL3d <- hclust(L3d,method="single")
plot(as.dendrogram(cL3d))

swadesh5 <- NULL
swadesh5$Irish = c("Irish","mé","tú","sé","sinn","sibh","siad","seo","sin","anseo","ansin","cé","cad","cá","cathain","conas","ní","uile","a lán","roinnt","beagán","eile","aon","dó","trí","ceathair","cúig","mór","fada","leathan","tiubh","trom","beag","gearr","cúng","tanaí","bean","fear","duine","leanbh","bean chéile","fear céile","máthair","athair","ainmhi","iasc","éan","madra","míol cnis","nathair","péist","crann","coill","bata","toradh","síol","duilleog","fréamh","rúsc","bláth","féar","téad","craiceann","feoil","fuil","cnámh","saill","ubh","adharc","eireaball","cleite","gruaig","ceann","cluas","súil","srón","béal","fiacail","teanga","ionga","cos","cos","glúin","lámh","sciathán","bolg","inní","muineál","droim","cíoch","croí","ae","ól","ith","bain greim as","súigh","caith seile","aisig","séid","análaigh","déan gáire","feic","cluin","a fhios a bheith agat","smaoinigh","bolaigh","eagla a bheith ort","codail","mair","faigh bás","maraigh","troid","seilg","buail","gearr","scoilt","rop","tochais","tochail","snámh","eitil","siúil","tar","luigh","suigh","seas","cas","tit","tabhair","coinnigh","fáisc","cuimil","nigh","ciumil","tarraing","brúigh","caith","ceangail","fuaigh","áirigh","abair","can","imir","snámh","snigh","reoigh","at","grian","gealach","réalta","uisce","báisteach","abhainn","loch","muir","salann","cloch","gaineamh","deannach","cré","scamall","ceo","spéir","gaoth","sneachta","oighear","deatach","tine","luaith","dóigh","bóthar","sliabh","rua","glas","buí","bán","dubh","oíche","lá","bliain","te","fuar","lán","nua","sean","maith","olc","lofa","salach","díreach","cruinn","géar","maol","mín","fliuch","tirim","ceart","gar","i bhfad","deis","clé","ag","i","le","agus","má","mar","ainm")
swadesh5$French = c("French","je","tu","il","nous","vous","ils","ceci","cela","ici","là","qui","quoi","où","quand","comment","ne pas","tout", "beaucoup","qualques","peu","autre","un","deux","trois","quatre","cinq","grand","long","large","épais","lourd","petit","court","étroit","mince","femme","homme","homme","enfant","femme","mari","mère","père","animal","poisson","oiseau","chien","pou","serpent","ver","arbre","forêt","bâton","fruit","graine","feuille","racine","écorce","fleur","herbe","corde","peau","viande","sang","os","graisse","œuf","corne","queue","plume","cheveux","tête","oreille","œil","nez","bouche","dent","langue","ongle","pied","jambe","genou","main","aile","ventre","entrailles","cou","dos","sein","cœur","foie","boire","manger","mordre","sucer","cracher","vomir","souffler","respirer","rire","voir","entendre","savoir","penser","sentir","craindre","dormir","vivre","mourir","tuer","se battre","chasser","frapper","couper","fendre","poignarder","gratter","creuser","nager","voler","marcher","venir","s'étendre","s'asseoir","se lever","tourner","tomber","donner","tenir","serrer","frotter","laver","essuyer","tirer","pousser","jeter","lier","coudre","compter","dire","chanter","jouer","flotter","couler","geler","gonfler","soleil","lune","étoile","eau","pluie","rivière","lac","mer","sel","pierre","sable","poussière","terre","nuage","brouillard","ciel","vent","neige","glace","fumée","feu","cendre","brûler","route","montagne","rouge","vert","jaune","blanc","noir","nuit","jour","an","chaud","froid","plein","nouveau","vieux","bon","mauvais","pourri","sale","droit","rond","tranchant","émoussé","lisse","mouillé","sec","juste","près","loin","droite","gauche","à","dans","avec","et","si","parce que","nom")
swadesh5$German = c("German","ich","du","er","wir","ihr","sie","dieser","der","hier","da","wer","was","wo","wann","wie","nicht","alle","viele","einige","wenig","anderer","eins","zwei","drei","vier","fünf","groß","lang","breit","dick","schwer","klein","kurz","eng","dünn","frau","mann","mensch","kind","frau","mann","mutter","vater","tier","fisch","vogel","hund","laus","schlange","wurm","baum","forst","stock","frucht","samen","blatt","wurzel","borke","blume","gras","leine","haut","fleisch","blut","knochen","fett","ei","horn","schwanz","feder","haar","haupt","ohr","auge","nase","mund","zahn","zunge","fingernagel","fuß","bein","knie","hand","fittich","bauch","eingeweide","hals","rücken","brust","herz","leber","trinken","essen","beißen","lutschen","speien","brechen","blasen","atmen","lachen","sehen","hören","kennen","denken","riechen","fürchten","schlafen","leben","sterben","töten","fechten","jagen","schlagen","schneiden","spalten","stechen","kratzen","graben","schwimmen","fliegen","gehen","kommen","liegen","sitzen","stehen","drehen","fallen","geben","halten","quetschen","reiben","waschen","wischen","ziehen","drücken","werfen","binden","nähen","rechnen","sagen","singen","spielen","schweben","fließen","frieren","schwellen","sonne","mond","stern","wasser","regen","fluss","see","meer","salz","stein","sand","staub","erde","wolke","nebel","himmel","wind","schnee","eis","rauch","feuer","asche","breenen","straße","berg","rot","grün","gelb","weiß","schwarz","nacht","tag","jahr","warm","kalt","voll","neu","alt","gut","schlecht","faul","schmutzig","gerade","rund","scharf","stumpf","glatt","nass","dürr","richtig","nah","fern","rechst","links","an","in","mit","und","ob","weil","name")
swadesh5$English = c("English","i", "you", "he","we", "you", "they", "this", "that", "here", "there", "who", "what", "where", "when", "how", "not", "all", "many", "some", "few", "other", "one", "two", "three", "four", "five", "big", "long", "wide", "thick", "heavy", "small", "short", "narrow", "thin", "woman", "man", "man", "child", "wife", "husband", "mother", "father", "animal", "fish", "bird", "dog", "louse", "snake", "worm", "tree", "forest", "stick", "fruit", "seed", "leaf", "root", "bark", "flower", "grass", "rope", "skin", "meat", "blood", "bone", "fat", "egg", "horn", "tail", "feather", "hair", "head", "ear", "eye", "nose", "mouth", "tooth", "tongue", "fingernail", "foot", "leg", "knee", "hand", "wing", "belly", "guts", "neck", "back", "breast", "heart", "liver", "drink", "eat", "bite", "suck", "spit", "vomit", "blow", "breathe", "laugh", "see", "hear", "know", "think", "smell", "fear", "sleep", "live", "die", "kill", "fight", "hunt", "hit", "cut", "split", "stab", "scratch", "dig", "swim", "fly", "walk", "come", "lie", "sit", "stand", "turn", "fall", "give", "hold", "squeeze", "rub", "wash", "wipe", "pull", "push", "throw", "tie", "sew", "count", "say", "sing", "play", "float", "flow", "freeze", "swell", "sun", "moon", "star", "water", "rain", "river", "lake", "sea", "salt", "stone", "sand", "dust", "earth", "cloud", "fog", "sky", "wind", "snow", "ice", "smoke", "fire", "ash", "burn", "road", "mountain", "red", "green", "yellow", "white", "black", "night", "day", "year", "warm", "cold", "full", "new", "old", "good", "bad", "rotten", "dirty", "straight", "round", "sharp", "dull", "smooth", "wet", "dry", "correct", "near", "far", "right", "left", "at", "in", "with", "and", "if", "because", "name")

#create the data frames
langswad5 <- data.frame(swadesh5[1:4])
langswad5$I1 <- substr(langswad5$Irish,1,1)
langswad5$F1 <- substr(langswad5$French,1,1)
langswad5$G1 <- substr(langswad5$German,1,1)
langswad5$E1 <- substr(langswad5$English,1,1)
langswad5$IF[langswad5$I1==langswad5$F1] <- 0
langswad5$IF[langswad5$I1!=langswad5$F1] <- 1
langswad5$IG[langswad5$I1==langswad5$G1] <- 0
langswad5$IG[langswad5$I1!=langswad5$G1] <- 1
langswad5$FG[langswad5$F1==langswad5$G1] <- 0
langswad5$FG[langswad5$F1!=langswad5$G1] <- 1
langswad5 = langswad5[-1,]
#randomize order of words
set.seed(1107231) #date 11/1/23 tree 1
langswad5$rand <- runif(nrow(langswad5),0,1)
langswad5sort <-langswad5[order(langswad5$rand),]
#split into 2 different populations
ls5 <- langswad5sort[1:105,]
ls6 <- langswad5sort[106:207,]
#assign random numbers
ls5$rand <- runif(nrow(ls5),0,1)
ls6$rand <- runif(nrow(ls6),0,1)
#sort smallest to largest
ls5sort <- ls5[order(ls5$rand),]
ls6sort <- ls6[order(ls6$rand),]
#create samples of size 3 and sum the distances
ls5d <- matrix(0,nrow = nrow(ls5sort)/3,ncol = 3)
ls6d <- matrix(0,nrow = nrow(ls6sort)/3,ncol = 3)
n=1
for(i in 1:(nrow(ls5sort)/3)){ 
  ls5d[i,1] <- sum(ls5sort$IF[n:(n+2)]) 
  ls5d[i,2] <- sum(ls5sort$IG[n:(n+2)]) 
  ls5d[i,3] <- sum(ls5sort$FG[n:(n+2)]) 
  n <- n+3 
}
n=1
for(i in 1:(nrow(ls6sort)/3)){
  ls6d[i,1] <- sum(ls6sort$IF[n:(n+2)])
  ls6d[i,2] <- sum(ls6sort$IG[n:(n+2)])
  ls6d[i,3] <- sum(ls6sort$FG[n:(n+2)])
  n <- n+3
}
ls5df <- data.frame(ls5d)
colnames(ls5df) <- c("IF","IG","FG")
ls6df <- data.frame(ls6d)
colnames(ls6df) <- c("IF","IG","FG")
ls5da <- matrix(0,nrow = nrow(ls5df),ncol = 3)
ls6da <- matrix(0,nrow = nrow(ls6df),ncol = 3)
ls5da <- data.frame(ls5da)
colnames(ls5da) <- c("IFd","IGd","FGd")
ls6da <- data.frame(ls6da)
colnames(ls6da) <- c("IFd","IGd","FGd")
ls5df <- cbind(ls5df,ls5da)
ls6df <- cbind(ls6df,ls6da)
#distances
#origin then EF min, EG min, FG min, no unique min: EF=EG, EF=FG,EG=FG
n=1
for(i in 1:nrow(ls5df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls5df[i,1],ls5df[i,2],ls5df[i,3]))==0){
    ls5df[i,4] <- 0
    ls5df[i,5] <- 0
    ls5df[i,6] <- 0
  }
  else if((ls5df[i,1]==min(ls5df[i,1:3]))&(ls5df[i,1]<min(ls5df[i,2:3]))){
    ls5df[i,4] <- min(ls5df[i,2:3])-ls5df[i,1]
    ls5df[i,5] <- -(min(ls5df[i,2:3])-ls5df[i,1])
    ls5df[i,6] <- -(min(ls5df[i,2:3])-ls5df[i,1])
  }
  else if((ls5df[i,2]==min(ls5df[i,1:3]))&(ls5df[i,2]<min(c(ls5df[i,1],ls5df[i,3])))){
    ls5df[i,4] <- -(min(c(ls5df[i,1],ls5df[i,3]))-ls5df[i,2])
    ls5df[i,5] <- min(c(ls5df[i,1],ls5df[i,3]))-ls5df[i,2]
    ls5df[i,6] <- -(min(c(ls5df[i,1],ls5df[i,3]))-ls5df[i,2])
  }
  else if((ls5df[i,3]==min(ls5df[i,1:3]))&(ls5df[i,3]<min(ls5df[i,1:2]))){
    ls5df[i,4] <- -(min(ls5df[i,1:2])-ls5df[i,3])
    ls5df[i,5] <- -(min(ls5df[i,1:2])-ls5df[i,3])
    ls5df[i,6] <- min(ls5df[i,1:2])-ls5df[i,3]
  }
  else if((ls5df[i,1]==min(ls5df[i,1:3]))&(ls5df[i,1]==ls5df[i,2])&r<0.5){
    ls5df[i,4] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,5] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
    ls5df[i,6] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
  }
  else if((ls5df[i,1]==min(ls5df[i,1:3]))&(ls5df[i,1]==ls5df[i,2])&r>0.5){
    ls5df[i,4] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
    ls5df[i,5] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,6] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
  }
  else if((ls5df[i,1]==min(ls5df[i,1:3]))&(ls5df[i,1]==ls5df[i,3])&r<0.5){
    ls5df[i,4] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
    ls5df[i,5] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,6] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
  }
  else if((ls5df[i,1]==min(ls5df[i,1:3]))&(ls5df[i,1]==ls5df[i,3])&r>0.5){
    ls5df[i,4] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,5] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,6] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
  }
  else if(r<0.5){
    ls5df[i,4] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,5] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
    ls5df[i,6] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
  }
  else{
    ls5df[i,4] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,5] <- -(max(ls5df[i,1:3])-min(ls5df[i,1:3]))
    ls5df[i,6] <- max(ls5df[i,1:3])-min(ls5df[i,1:3])
  }
}

for(i in 1:nrow(ls6df)){
  set.seed(i)
  r=runif(1)
  if(var(c(ls6df[i,1],ls6df[i,2],ls6df[i,3]))==0){
    ls6df[i,4] <- 0
    ls6df[i,5] <- 0
    ls6df[i,6] <- 0
  }
  else if((ls6df[i,1]==min(ls6df[i,1:3]))&(ls6df[i,1]<min(ls6df[i,2:3]))){
    ls6df[i,4] <- min(ls6df[i,2:3])-ls6df[i,1]
    ls6df[i,5] <- -(min(ls6df[i,2:3])-ls6df[i,1])
    ls6df[i,6] <- -(min(ls6df[i,2:3])-ls6df[i,1])
  }
  else if((ls6df[i,2]==min(ls6df[i,1:3]))&(ls6df[i,2]<min(c(ls6df[i,1],ls6df[i,3])))){
    ls6df[i,4] <- -(min(c(ls6df[i,1],ls6df[i,3]))-ls6df[i,2])
    ls6df[i,5] <- min(c(ls6df[i,1],ls6df[i,3]))-ls6df[i,2]
    ls6df[i,6] <- -(min(c(ls6df[i,1],ls6df[i,3]))-ls6df[i,2])
  }
  else if((ls6df[i,3]==min(ls6df[i,1:3]))&(ls6df[i,3]<min(ls6df[i,1:2]))){
    ls6df[i,4] <- -(min(ls6df[i,1:2])-ls6df[i,3])
    ls6df[i,5] <- -(min(ls6df[i,1:2])-ls6df[i,3])
    ls6df[i,6] <- min(ls6df[i,1:2])-ls6df[i,3]
  }
  else if((ls6df[i,1]==min(ls6df[i,1:3]))&(ls6df[i,1]==ls6df[i,2])&r<0.5){
    ls6df[i,4] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
  else if((ls6df[i,1]==min(ls6df[i,1:3]))&(ls6df[i,1]==ls6df[i,2])&r>0.5){
    ls6df[i,4] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
  else if((ls6df[i,1]==min(ls6df[i,1:3]))&(ls6df[i,1]==ls6df[i,3])&r<0.5){
    ls6df[i,4] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
  else if((ls6df[i,1]==min(ls6df[i,1:3]))&(ls6df[i,1]==ls6df[i,3])&r>0.5){
    ls6df[i,4] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
  else if(r<0.5){
    ls6df[i,4] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
  else{
    ls6df[i,4] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,5] <- -(max(ls6df[i,1:3])-min(ls6df[i,1:3]))
    ls6df[i,6] <- (max(ls6df[i,1:3])-min(ls6df[i,1:3]))
  }
}
colMeans(ls5df[,4:6])
colMeans(ls6df[,4:6])
#use this for sample variance if non-sticky
max(diag(var(ls5df[,4:6])))
max(diag(var(ls6df[,4:6])))
sum(ls5df[,4]^2)/(nrow(ls5df)-1)
sum(ls5df[,5]^2)/(nrow(ls5df)-1)
sum(ls5df[,6]^2)/(nrow(ls5df)-1)
sum(ls6df[,4]^2)/(nrow(ls6df)-1)
sum(ls6df[,5]^2)/(nrow(ls6df)-1)
sum(ls6df[,6]^2)/(nrow(ls6df)-1)

irish1.boot <- boot(ls5df[,6],meanlang.boot,R=999,stype = "f")
french5.boot <- boot(ls5df[,5],meanlang.boot,R=999,stype="f")
german3.boot <- boot(ls5df[,4],meanlang.boot,R=999,stype="f")
irish2.boot <- boot(ls6df[,6],meanlang.boot,R=999,stype = "f")
french6.boot <- boot(ls6df[,5],meanlang.boot,R=999,stype="f")
german4.boot <- boot(ls6df[,4],meanlang.boot,R=999,stype="f")

boot.ci(irish1.boot,conf=0.95)
boot.ci(french5.boot,conf=0.95)
boot.ci(german3.boot,conf=0.95)
boot.ci(irish2.boot,conf=0.95)
boot.ci(french6.boot,conf=0.95)
boot.ci(german4.boot,conf=0.95)


swadesh5 <- NULL
swadesh5$Irish = c("Irish","mé","tú","sé","sinn","sibh","siad","seo","sin","anseo","ansin","cé","cad","cá","cathain","conas","ní","uile","a lán","roinnt","beagán","eile","aon","dó","trí","ceathair","cúig","mór","fada","leathan","tiubh","trom","beag","gearr","cúng","tanaí","bean","fear","duine","leanbh","bean chéile","fear céile","máthair","athair","ainmhi","iasc","éan","madra","míol cnis","nathair","péist","crann","coill","bata","toradh","síol","duilleog","fréamh","rúsc","bláth","féar","téad","craiceann","feoil","fuil","cnámh","saill","ubh","adharc","eireaball","cleite","gruaig","ceann","cluas","súil","srón","béal","fiacail","teanga","ionga","cos","cos","glúin","lámh","sciathán","bolg","inní","muineál","droim","cíoch","croí","ae","ól","ith","bain greim as","súigh","caith seile","aisig","séid","análaigh","déan gáire","feic","cluin","a fhios a bheith agat","smaoinigh","bolaigh","eagla a bheith ort","codail","mair","faigh bás","maraigh","troid","seilg","buail","gearr","scoilt","rop","tochais","tochail","snámh","eitil","siúil","tar","luigh","suigh","seas","cas","tit","tabhair","coinnigh","fáisc","cuimil","nigh","ciumil","tarraing","brúigh","caith","ceangail","fuaigh","áirigh","abair","can","imir","snámh","snigh","reoigh","at","grian","gealach","réalta","uisce","báisteach","abhainn","loch","muir","salann","cloch","gaineamh","deannach","cré","scamall","ceo","spéir","gaoth","sneachta","oighear","deatach","tine","luaith","dóigh","bóthar","sliabh","rua","glas","buí","bán","dubh","oíche","lá","bliain","te","fuar","lán","nua","sean","maith","olc","lofa","salach","díreach","cruinn","géar","maol","mín","fliuch","tirim","ceart","gar","i bhfad","deis","clé","ag","i","le","agus","má","mar","ainm")
swadesh5$French = c("French","je","tu","il","nous","vous","ils","ceci","cela","ici","là","qui","quoi","où","quand","comment","ne pas","tout", "beaucoup","qualques","peu","autre","un","deux","trois","quatre","cinq","grand","long","large","épais","lourd","petit","court","étroit","mince","femme","homme","homme","enfant","femme","mari","mère","père","animal","poisson","oiseau","chien","pou","serpent","ver","arbre","forêt","bâton","fruit","graine","feuille","racine","écorce","fleur","herbe","corde","peau","viande","sang","os","graisse","œuf","corne","queue","plume","cheveux","tête","oreille","œil","nez","bouche","dent","langue","ongle","pied","jambe","genou","main","aile","ventre","entrailles","cou","dos","sein","cœur","foie","boire","manger","mordre","sucer","cracher","vomir","souffler","respirer","rire","voir","entendre","savoir","penser","sentir","craindre","dormir","vivre","mourir","tuer","se battre","chasser","frapper","couper","fendre","poignarder","gratter","creuser","nager","voler","marcher","venir","s'étendre","s'asseoir","se lever","tourner","tomber","donner","tenir","serrer","frotter","laver","essuyer","tirer","pousser","jeter","lier","coudre","compter","dire","chanter","jouer","flotter","couler","geler","gonfler","soleil","lune","étoile","eau","pluie","rivière","lac","mer","sel","pierre","sable","poussière","terre","nuage","brouillard","ciel","vent","neige","glace","fumée","feu","cendre","brûler","route","montagne","rouge","vert","jaune","blanc","noir","nuit","jour","an","chaud","froid","plein","nouveau","vieux","bon","mauvais","pourri","sale","droit","rond","tranchant","émoussé","lisse","mouillé","sec","juste","près","loin","droite","gauche","à","dans","avec","et","si","parce que","nom")
swadesh5$German = c("German","ich","du","er","wir","ihr","sie","dieser","der","hier","da","wer","was","wo","wann","wie","nicht","alle","viele","einige","wenig","anderer","eins","zwei","drei","vier","fünf","groß","lang","breit","dick","schwer","klein","kurz","eng","dünn","frau","mann","mensch","kind","frau","mann","mutter","vater","tier","fisch","vogel","hund","laus","schlange","wurm","baum","forst","stock","frucht","samen","blatt","wurzel","borke","blume","gras","leine","haut","fleisch","blut","knochen","fett","ei","horn","schwanz","feder","haar","haupt","ohr","auge","nase","mund","zahn","zunge","fingernagel","fuß","bein","knie","hand","fittich","bauch","eingeweide","hals","rücken","brust","herz","leber","trinken","essen","beißen","lutschen","speien","brechen","blasen","atmen","lachen","sehen","hören","kennen","denken","riechen","fürchten","schlafen","leben","sterben","töten","fechten","jagen","schlagen","schneiden","spalten","stechen","kratzen","graben","schwimmen","fliegen","gehen","kommen","liegen","sitzen","stehen","drehen","fallen","geben","halten","quetschen","reiben","waschen","wischen","ziehen","drücken","werfen","binden","nähen","rechnen","sagen","singen","spielen","schweben","fließen","frieren","schwellen","sonne","mond","stern","wasser","regen","fluss","see","meer","salz","stein","sand","staub","erde","wolke","nebel","himmel","wind","schnee","eis","rauch","feuer","asche","breenen","straße","berg","rot","grün","gelb","weiß","schwarz","nacht","tag","jahr","warm","kalt","voll","neu","alt","gut","schlecht","faul","schmutzig","gerade","rund","scharf","stumpf","glatt","nass","dürr","richtig","nah","fern","rechst","links","an","in","mit","und","ob","weil","name")
swadesh5$English = c("English","i", "you", "he","we", "you", "they", "this", "that", "here", "there", "who", "what", "where", "when", "how", "not", "all", "many", "some", "few", "other", "one", "two", "three", "four", "five", "big", "long", "wide", "thick", "heavy", "small", "short", "narrow", "thin", "woman", "man", "man", "child", "wife", "husband", "mother", "father", "animal", "fish", "bird", "dog", "louse", "snake", "worm", "tree", "forest", "stick", "fruit", "seed", "leaf", "root", "bark", "flower", "grass", "rope", "skin", "meat", "blood", "bone", "fat", "egg", "horn", "tail", "feather", "hair", "head", "ear", "eye", "nose", "mouth", "tooth", "tongue", "fingernail", "foot", "leg", "knee", "hand", "wing", "belly", "guts", "neck", "back", "breast", "heart", "liver", "drink", "eat", "bite", "suck", "spit", "vomit", "blow", "breathe", "laugh", "see", "hear", "know", "think", "smell", "fear", "sleep", "live", "die", "kill", "fight", "hunt", "hit", "cut", "split", "stab", "scratch", "dig", "swim", "fly", "walk", "come", "lie", "sit", "stand", "turn", "fall", "give", "hold", "squeeze", "rub", "wash", "wipe", "pull", "push", "throw", "tie", "sew", "count", "say", "sing", "play", "float", "flow", "freeze", "swell", "sun", "moon", "star", "water", "rain", "river", "lake", "sea", "salt", "stone", "sand", "dust", "earth", "cloud", "fog", "sky", "wind", "snow", "ice", "smoke", "fire", "ash", "burn", "road", "mountain", "red", "green", "yellow", "white", "black", "night", "day", "year", "warm", "cold", "full", "new", "old", "good", "bad", "rotten", "dirty", "straight", "round", "sharp", "dull", "smooth", "wet", "dry", "correct", "near", "far", "right", "left", "at", "in", "with", "and", "if", "because", "name")

#create the data frames
langt4_1 <- data.frame(swadesh5[1:4])
langt4_1$I1 <- substr(langt4_1$Irish,1,1)
langt4_1$F1 <- substr(langt4_1$French,1,1)
langt4_1$G1 <- substr(langt4_1$German,1,1)
langt4_1$E1 <- substr(langt4_1$English,1,1)
langt4_1$EI[langt4_1$E1==langt4_1$I1] <- 0
langt4_1$EI[langt4_1$E1!=langt4_1$I1] <- 1
langt4_1$EF[langt4_1$E1==langt4_1$F1] <- 0
langt4_1$EF[langt4_1$E1!=langt4_1$F1] <- 1
langt4_1$EG[langt4_1$E1==langt4_1$G1] <- 0
langt4_1$EG[langt4_1$E1!=langt4_1$G1] <- 1
langt4_1$IF[langt4_1$I1==langt4_1$F1] <- 0
langt4_1$IF[langt4_1$I1!=langt4_1$F1] <- 1
langt4_1$IG[langt4_1$I1==langt4_1$G1] <- 0
langt4_1$IG[langt4_1$I1!=langt4_1$G1] <- 1
langt4_1$FG[langt4_1$F1==langt4_1$G1] <- 0
langt4_1$FG[langt4_1$F1!=langt4_1$G1] <- 1
langt4_1 = langt4_1[-1,]
#randomize order of words
set.seed(220425) #date 22/04/25
langt4_1$rand <- runif(nrow(langt4_1),0,1)
langt4_1sort <-langt4_1[order(langt4_1$rand),]
lt41 <- langt4_1[1:4,]
lt42 <- langt4_1[5:8,]
lt43 <- langt4_1[9:12,]
lt44 <- langt4_1[13:16,]
lt45 <- langt4_1[17:20,]
lt46 <- langt4_1[21:24,]
lt47 <- langt4_1[25:28,]
lt48 <- langt4_1[29:32,]
lt49 <- langt4_1[33:36,]
lt410 <- langt4_1[37:40,]
lt411 <- langt4_1[41:44,]
lt412 <- langt4_1[45:48,]
lt413 <- langt4_1[49:52,]
lt414 <- langt4_1[53:56,]
lt415 <- langt4_1[57:60,]
lt416 <- langt4_1[61:64,]
lt417 <- langt4_1[65:68,]
lt418 <- langt4_1[69:72,]
lt419 <- langt4_1[73:76,]
lt420 <- langt4_1[77:80,]
lt421 <- langt4_1[81:84,]
lt422 <- langt4_1[85:88,]
lt423 <- langt4_1[89:92,]
lt424 <- langt4_1[93:96,]
lt425 <- langt4_1[97:100,]
lt426 <- langt4_1[101:104,]
lt427 <- langt4_1[105:108,]
lt428 <- langt4_1[109:112,]
lt429 <- langt4_1[113:116,]
lt430 <- langt4_1[117:120,]
lt431 <- langt4_1[121:124,]
lt432 <- langt4_1[125:128,]
lt433 <- langt4_1[129:132,]
lt434 <- langt4_1[133:136,]
lt435 <- langt4_1[137:140,]
lt436 <- langt4_1[141:144,]
lt437 <- langt4_1[145:148,]
lt438 <- langt4_1[149:152,]
lt439 <- langt4_1[153:156,]
lt440 <- langt4_1[157:160,]
lt441 <- langt4_1[161:164,]
lt442 <- langt4_1[165:168,]
lt443 <- langt4_1[169:172,]
lt444 <- langt4_1[173:176,]
lt445 <- langt4_1[177:180,]
lt446 <- langt4_1[181:184,]
lt447 <- langt4_1[185:188,]
lt448 <- langt4_1[189:192,]
lt449 <- langt4_1[193:196,]
lt450 <- langt4_1[197:200,]

Et4_1 <- c(0,sum(lt41$EI),sum(lt41$EF),sum(lt41$EG))
It4_1 <- c(sum(lt41$EI),0,sum(lt41$IF),sum(lt41$IG))
Ft4_1 <- c(sum(lt41$EF),sum(lt41$IF),0,sum(lt41$FG))
Gt4_1 <- c(sum(lt41$EG),sum(lt41$IG),sum(lt41$FG),0)
Langt4_1_d1 <- cbind(Et4_1,It4_1,Ft4_1,Gt4_1)
Lt41d <- as.dist(Langt4_1_d1)
cLt41d <- hclust(Lt41d,method="single")
plot(as.dendrogram(cLt41d),horiz = TRUE,cex.axis=2)
Et4_2 <- c(0,sum(lt42$EI),sum(lt42$EF),sum(lt42$EG))
It4_2 <- c(sum(lt42$EI),0,sum(lt42$IF),sum(lt42$IG))
Ft4_2 <- c(sum(lt42$EF),sum(lt42$IF),0,sum(lt42$FG))
Gt4_2 <- c(sum(lt42$EG),sum(lt42$IG),sum(lt42$FG),0)
Langt4_1_d2 <- cbind(Et4_2,It4_2,Ft4_2,Gt4_2)
Lt42d <- as.dist(Langt4_1_d2)
cLt42d <- hclust(Lt42d,method="single")
plot(as.dendrogram(cLt42d),horiz = TRUE,cex.axis=2)
Et4_3 <- c(0,sum(lt43$EI),sum(lt43$EF),sum(lt43$EG))
It4_3 <- c(sum(lt43$EI),0,sum(lt43$IF),sum(lt43$IG))
Ft4_3 <- c(sum(lt43$EF),sum(lt43$IF),0,sum(lt43$FG))
Gt4_3 <- c(sum(lt43$EG),sum(lt43$IG),sum(lt43$FG),0)
Langt4_1_d3 <- cbind(Et4_3,It4_3,Ft4_3,Gt4_3)
Lt43d <- as.dist(Langt4_1_d3)
cLt43d <- hclust(Lt43d,method="single")
plot(as.dendrogram(cLt43d),horiz = TRUE,cex.axis=2)
Et4_4 <- c(0,sum(lt44$EI),sum(lt44$EF),sum(lt44$EG))
It4_4 <- c(sum(lt44$EI),0,sum(lt44$IF),sum(lt44$IG))
Ft4_4 <- c(sum(lt44$EF),sum(lt44$IF),0,sum(lt44$FG))
Gt4_4 <- c(sum(lt44$EG),sum(lt44$IG),sum(lt44$FG),0)
Langt4_1_d4 <- cbind(Et4_4,It4_4,Ft4_4,Gt4_4)
Lt44d <- as.dist(Langt4_1_d4)
cLt44d <- hclust(Lt44d,method="single")
plot(as.dendrogram(cLt44d),horiz = TRUE,cex.axis=2)
Et4_5 <- c(0,sum(lt45$EI),sum(lt45$EF),sum(lt45$EG))
It4_5 <- c(sum(lt45$EI),0,sum(lt45$IF),sum(lt45$IG))
Ft4_5 <- c(sum(lt45$EF),sum(lt45$IF),0,sum(lt45$FG))
Gt4_5 <- c(sum(lt45$EG),sum(lt45$IG),sum(lt45$FG),0)
Langt4_1_d5 <- cbind(Et4_5,It4_5,Ft4_5,Gt4_5)
Lt45d <- as.dist(Langt4_1_d5)
cLt45d <- hclust(Lt45d,method="single")
plot(as.dendrogram(cLt45d),horiz = TRUE,cex.axis=2)
Et4_6 <- c(0,sum(lt46$EI),sum(lt46$EF),sum(lt46$EG))
It4_6 <- c(sum(lt46$EI),0,sum(lt46$IF),sum(lt46$IG))
Ft4_6 <- c(sum(lt46$EF),sum(lt46$IF),0,sum(lt46$FG))
Gt4_6 <- c(sum(lt46$EG),sum(lt46$IG),sum(lt46$FG),0)
Langt4_1_d6 <- cbind(Et4_6,It4_6,Ft4_6,Gt4_6)
Lt46d <- as.dist(Langt4_1_d6)
cLt46d <- hclust(Lt46d,method="single")
plot(as.dendrogram(cLt46d),horiz = TRUE,cex.axis=2)
Et4_7 <- c(0,sum(lt47$EI),sum(lt47$EF),sum(lt47$EG))
It4_7 <- c(sum(lt47$EI),0,sum(lt47$IF),sum(lt47$IG))
Ft4_7 <- c(sum(lt47$EF),sum(lt47$IF),0,sum(lt47$FG))
Gt4_7 <- c(sum(lt47$EG),sum(lt47$IG),sum(lt47$FG),0)
Langt4_1_d7 <- cbind(Et4_7,It4_7,Ft4_7,Gt4_7)
Lt47d <- as.dist(Langt4_1_d7)
cLt47d <- hclust(Lt47d,method="single")
plot(as.dendrogram(cLt47d),horiz = TRUE,cex.axis=2)
#7 is on an edge
Et4_8 <- c(0,sum(lt48$EI),sum(lt48$EF),sum(lt48$EG))
It4_8 <- c(sum(lt48$EI),0,sum(lt48$IF),sum(lt48$IG))
Ft4_8 <- c(sum(lt48$EF),sum(lt48$IF),0,sum(lt48$FG))
Gt4_8 <- c(sum(lt48$EG),sum(lt48$IG),sum(lt48$FG),0)
Langt4_1_d8 <- cbind(Et4_8,It4_8,Ft4_8,Gt4_8)
Lt48d <- as.dist(Langt4_1_d8)
cLt48d <- hclust(Lt48d,method="single")
plot(as.dendrogram(cLt48d),horiz = TRUE,cex.axis=2)
Et4_9 <- c(0,sum(lt49$EI),sum(lt49$EF),sum(lt49$EG))
It4_9 <- c(sum(lt49$EI),0,sum(lt49$IF),sum(lt49$IG))
Ft4_9 <- c(sum(lt49$EF),sum(lt49$IF),0,sum(lt49$FG))
Gt4_9 <- c(sum(lt49$EG),sum(lt49$IG),sum(lt49$FG),0)
Langt4_1_d9 <- cbind(Et4_9,It4_9,Ft4_9,Gt4_9)
Lt49d <- as.dist(Langt4_1_d9)
cLt49d <- hclust(Lt49d,method="single")
plot(as.dendrogram(cLt49d),horiz = TRUE,cex.axis=2)
Et4_10 <- c(0,sum(lt410$EI),sum(lt410$EF),sum(lt410$EG))
It4_10 <- c(sum(lt410$EI),0,sum(lt410$IF),sum(lt410$IG))
Ft4_10 <- c(sum(lt410$EF),sum(lt410$IF),0,sum(lt410$FG))
Gt4_10 <- c(sum(lt410$EG),sum(lt410$IG),sum(lt410$FG),0)
Langt4_1_d10 <- cbind(Et4_10,It4_10,Ft4_10,Gt4_10)
Lt410d <- as.dist(Langt4_1_d10)
cLt410d <- hclust(Lt410d,method="single")
plot(as.dendrogram(cLt410d),horiz = TRUE,cex.axis=2)
Et4_11 <- c(0,sum(lt411$EI),sum(lt411$EF),sum(lt411$EG))
It4_11 <- c(sum(lt411$EI),0,sum(lt411$IF),sum(lt411$IG))
Ft4_11 <- c(sum(lt411$EF),sum(lt411$IF),0,sum(lt411$FG))
Gt4_11 <- c(sum(lt411$EG),sum(lt411$IG),sum(lt411$FG),0)
Langt4_1_d11 <- cbind(Et4_11,It4_11,Ft4_11,Gt4_11)
Lt411d <- as.dist(Langt4_1_d11)
cLt411d <- hclust(Lt411d,method="single")
plot(as.dendrogram(cLt411d),horiz = TRUE,cex.axis=2) #origin
Et4_12 <- c(0,sum(lt412$EI),sum(lt412$EF),sum(lt412$EG))
It4_12 <- c(sum(lt412$EI),0,sum(lt412$IF),sum(lt412$IG))
Ft4_12 <- c(sum(lt412$EF),sum(lt412$IF),0,sum(lt412$FG))
Gt4_12 <- c(sum(lt412$EG),sum(lt412$IG),sum(lt412$FG),0)
Langt4_1_d12 <- cbind(Et4_12,It4_12,Ft4_12,Gt4_12)
Lt412d <- as.dist(Langt4_1_d12)
cLt412d <- hclust(Lt412d,method="single")
plot(as.dendrogram(cLt412d),horiz = TRUE,cex.axis=2)
Et4_13 <- c(0,sum(lt413$EI),sum(lt413$EF),sum(lt413$EG))
It4_13 <- c(sum(lt413$EI),0,sum(lt413$IF),sum(lt413$IG))
Ft4_13 <- c(sum(lt413$EF),sum(lt413$IF),0,sum(lt413$FG))
Gt4_13 <- c(sum(lt413$EG),sum(lt413$IG),sum(lt413$FG),0)
Langt4_1_d13 <- cbind(Et4_13,It4_13,Ft4_13,Gt4_13)
Lt413d <- as.dist(Langt4_1_d13)
cLt413d <- hclust(Lt413d,method="single")
plot(as.dendrogram(cLt413d),horiz = TRUE,cex.axis=2)
Et4_14 <- c(0,sum(lt414$EI),sum(lt414$EF),sum(lt414$EG))
It4_14 <- c(sum(lt414$EI),0,sum(lt414$IF),sum(lt414$IG))
Ft4_14 <- c(sum(lt414$EF),sum(lt414$IF),0,sum(lt414$FG))
Gt4_14 <- c(sum(lt414$EG),sum(lt414$IG),sum(lt414$FG),0)
Langt4_1_d14 <- cbind(Et4_14,It4_14,Ft4_14,Gt4_14)
Lt414d <- as.dist(Langt4_1_d14)
cLt414d <- hclust(Lt414d,method="single")
plot(as.dendrogram(cLt414d),horiz = TRUE,cex.axis=2)
Et4_15 <- c(0,sum(lt415$EI),sum(lt415$EF),sum(lt415$EG))
It4_15 <- c(sum(lt415$EI),0,sum(lt415$IF),sum(lt415$IG))
Ft4_15 <- c(sum(lt415$EF),sum(lt415$IF),0,sum(lt415$FG))
Gt4_15 <- c(sum(lt415$EG),sum(lt415$IG),sum(lt415$FG),0)
Langt4_1_d15 <- cbind(Et4_15,It4_15,Ft4_15,Gt4_15)
Lt415d <- as.dist(Langt4_1_d15)
cLt415d <- hclust(Lt415d,method="single")
plot(as.dendrogram(cLt415d),horiz = TRUE,cex.axis=2)
Et4_16 <- c(0,sum(lt416$EI),sum(lt416$EF),sum(lt416$EG))
It4_16 <- c(sum(lt416$EI),0,sum(lt416$IF),sum(lt416$IG))
Ft4_16 <- c(sum(lt416$EF),sum(lt416$IF),0,sum(lt416$FG))
Gt4_16 <- c(sum(lt416$EG),sum(lt416$IG),sum(lt416$FG),0)
Langt4_1_d16 <- cbind(Et4_16,It4_16,Ft4_16,Gt4_16)
Lt416d <- as.dist(Langt4_1_d16)
cLt416d <- hclust(Lt416d,method="single")
plot(as.dendrogram(cLt416d),horiz = TRUE,cex.axis=2)
Et4_17 <- c(0,sum(lt417$EI),sum(lt417$EF),sum(lt417$EG))
It4_17 <- c(sum(lt417$EI),0,sum(lt417$IF),sum(lt417$IG))
Ft4_17 <- c(sum(lt417$EF),sum(lt417$IF),0,sum(lt417$FG))
Gt4_17 <- c(sum(lt417$EG),sum(lt417$IG),sum(lt417$FG),0)
Langt4_1_d17 <- cbind(Et4_17,It4_17,Ft4_17,Gt4_17)
Lt417d <- as.dist(Langt4_1_d17)
cLt417d <- hclust(Lt417d,method="single")
plot(as.dendrogram(cLt417d),horiz = TRUE,cex.axis=2)
Et4_18 <- c(0,sum(lt418$EI),sum(lt418$EF),sum(lt418$EG))
It4_18 <- c(sum(lt418$EI),0,sum(lt418$IF),sum(lt418$IG))
Ft4_18 <- c(sum(lt418$EF),sum(lt418$IF),0,sum(lt418$FG))
Gt4_18 <- c(sum(lt418$EG),sum(lt418$IG),sum(lt418$FG),0)
Langt4_1_d18 <- cbind(Et4_18,It4_18,Ft4_18,Gt4_18)
Lt418d <- as.dist(Langt4_1_d18)
cLt418d <- hclust(Lt418d,method="single")
plot(as.dendrogram(cLt418d),horiz = TRUE,cex.axis=2)
Et4_19 <- c(0,sum(lt419$EI),sum(lt419$EF),sum(lt419$EG))
It4_19 <- c(sum(lt419$EI),0,sum(lt419$IF),sum(lt419$IG))
Ft4_19 <- c(sum(lt419$EF),sum(lt419$IF),0,sum(lt419$FG))
Gt4_19 <- c(sum(lt419$EG),sum(lt419$IG),sum(lt419$FG),0)
Langt4_1_d19 <- cbind(Et4_19,It4_19,Ft4_19,Gt4_19)
Lt419d <- as.dist(Langt4_1_d19)
cLt419d <- hclust(Lt419d,method="single")
plot(as.dendrogram(cLt419d),horiz = TRUE,cex.axis=2)
Et4_20 <- c(0,sum(lt420$EI),sum(lt420$EF),sum(lt420$EG))
It4_20 <- c(sum(lt420$EI),0,sum(lt420$IF),sum(lt420$IG))
Ft4_20 <- c(sum(lt420$EF),sum(lt420$IF),0,sum(lt420$FG))
Gt4_20 <- c(sum(lt420$EG),sum(lt420$IG),sum(lt420$FG),0)
Langt4_1_d20 <- cbind(Et4_20,It4_20,Ft4_20,Gt4_20)
Lt420d <- as.dist(Langt4_1_d20)
cLt420d <- hclust(Lt420d,method="single")
plot(as.dendrogram(cLt420d),horiz = TRUE,cex.axis=2)
Et4_21 <- c(0,sum(lt421$EI),sum(lt421$EF),sum(lt421$EG))
It4_21 <- c(sum(lt421$EI),0,sum(lt421$IF),sum(lt421$IG))
Ft4_21 <- c(sum(lt421$EF),sum(lt421$IF),0,sum(lt421$FG))
Gt4_21 <- c(sum(lt421$EG),sum(lt421$IG),sum(lt421$FG),0)
Langt4_1_d21 <- cbind(Et4_21,It4_21,Ft4_21,Gt4_21)
Lt421d <- as.dist(Langt4_1_d21)
cLt421d <- hclust(Lt421d,method="single")
plot(as.dendrogram(cLt421d),horiz = TRUE,cex.axis=2)
Et4_22 <- c(0,sum(lt422$EI),sum(lt422$EF),sum(lt422$EG))
It4_22 <- c(sum(lt422$EI),0,sum(lt422$IF),sum(lt422$IG))
Ft4_22 <- c(sum(lt422$EF),sum(lt422$IF),0,sum(lt422$FG))
Gt4_22 <- c(sum(lt422$EG),sum(lt422$IG),sum(lt422$FG),0)
Langt4_1_d22 <- cbind(Et4_22,It4_22,Ft4_22,Gt4_22)
Lt422d <- as.dist(Langt4_1_d22)
cLt422d <- hclust(Lt422d,method="single")
plot(as.dendrogram(cLt422d),horiz = TRUE,cex.axis=2)
#origin
Et4_23 <- c(0,sum(lt423$EI),sum(lt423$EF),sum(lt423$EG))
It4_23 <- c(sum(lt423$EI),0,sum(lt423$IF),sum(lt423$IG))
Ft4_23 <- c(sum(lt423$EF),sum(lt423$IF),0,sum(lt423$FG))
Gt4_23 <- c(sum(lt423$EG),sum(lt423$IG),sum(lt423$FG),0)
Langt4_1_d23 <- cbind(Et4_23,It4_23,Ft4_23,Gt4_23)
Lt423d <- as.dist(Langt4_1_d23)
cLt423d <- hclust(Lt423d,method="single")
plot(as.dendrogram(cLt423d),horiz = TRUE,cex.axis=2)
Et4_24 <- c(0,sum(lt424$EI),sum(lt424$EF),sum(lt424$EG))
It4_24 <- c(sum(lt424$EI),0,sum(lt424$IF),sum(lt424$IG))
Ft4_24 <- c(sum(lt424$EF),sum(lt424$IF),0,sum(lt424$FG))
Gt4_24 <- c(sum(lt424$EG),sum(lt424$IG),sum(lt424$FG),0)
Langt4_1_d24 <- cbind(Et4_24,It4_24,Ft4_24,Gt4_24)
Lt424d <- as.dist(Langt4_1_d24)
cLt424d <- hclust(Lt424d,method="single")
plot(as.dendrogram(cLt424d),horiz = TRUE,cex.axis=2)
Et4_25 <- c(0,sum(lt425$EI),sum(lt425$EF),sum(lt425$EG))
It4_25 <- c(sum(lt425$EI),0,sum(lt425$IF),sum(lt425$IG))
Ft4_25 <- c(sum(lt425$EF),sum(lt425$IF),0,sum(lt425$FG))
Gt4_25 <- c(sum(lt425$EG),sum(lt425$IG),sum(lt425$FG),0)
Langt4_1_d25 <- cbind(Et4_25,It4_25,Ft4_25,Gt4_25)
Lt425d <- as.dist(Langt4_1_d25)
cLt425d <- hclust(Lt425d,method="single")
plot(as.dendrogram(cLt425d),horiz = TRUE,cex.axis=2)
Et4_26 <- c(0,sum(lt426$EI),sum(lt426$EF),sum(lt426$EG))
It4_26 <- c(sum(lt426$EI),0,sum(lt426$IF),sum(lt426$IG))
Ft4_26 <- c(sum(lt426$EF),sum(lt426$IF),0,sum(lt426$FG))
Gt4_26 <- c(sum(lt426$EG),sum(lt426$IG),sum(lt426$FG),0)
Langt4_1_d26 <- cbind(Et4_26,It4_26,Ft4_26,Gt4_26)
Lt426d <- as.dist(Langt4_1_d26)
cLt426d <- hclust(Lt426d,method="single")
plot(as.dendrogram(cLt426d),horiz = TRUE,cex.axis=2)
Et4_27 <- c(0,sum(lt427$EI),sum(lt427$EF),sum(lt427$EG))
It4_27 <- c(sum(lt427$EI),0,sum(lt427$IF),sum(lt427$IG))
Ft4_27 <- c(sum(lt427$EF),sum(lt427$IF),0,sum(lt427$FG))
Gt4_27 <- c(sum(lt427$EG),sum(lt427$IG),sum(lt427$FG),0)
Langt4_1_d27 <- cbind(Et4_27,It4_27,Ft4_27,Gt4_27)
Lt427d <- as.dist(Langt4_1_d27)
cLt427d <- hclust(Lt427d,method="single")
plot(as.dendrogram(cLt427d),horiz = TRUE,cex.axis=2)
Et4_28 <- c(0,sum(lt428$EI),sum(lt428$EF),sum(lt428$EG))
It4_28 <- c(sum(lt428$EI),0,sum(lt428$IF),sum(lt428$IG))
Ft4_28 <- c(sum(lt428$EF),sum(lt428$IF),0,sum(lt428$FG))
Gt4_28 <- c(sum(lt428$EG),sum(lt428$IG),sum(lt428$FG),0)
Langt4_1_d28 <- cbind(Et4_28,It4_28,Ft4_28,Gt4_28)
Lt428d <- as.dist(Langt4_1_d28)
cLt428d <- hclust(Lt428d,method="single")
plot(as.dendrogram(cLt428d),horiz = TRUE,cex.axis=2)
Et4_29 <- c(0,sum(lt429$EI),sum(lt429$EF),sum(lt429$EG))
It4_29 <- c(sum(lt429$EI),0,sum(lt429$IF),sum(lt429$IG))
Ft4_29 <- c(sum(lt429$EF),sum(lt429$IF),0,sum(lt429$FG))
Gt4_29 <- c(sum(lt429$EG),sum(lt429$IG),sum(lt429$FG),0)
Langt4_1_d29 <- cbind(Et4_29,It4_29,Ft4_29,Gt4_29)
Lt429d <- as.dist(Langt4_1_d29)
cLt429d <- hclust(Lt429d,method="single")
plot(as.dendrogram(cLt429d),horiz = TRUE,cex.axis=2)
Et4_30 <- c(0,sum(lt430$EI),sum(lt430$EF),sum(lt430$EG))
It4_30 <- c(sum(lt430$EI),0,sum(lt430$IF),sum(lt430$IG))
Ft4_30 <- c(sum(lt430$EF),sum(lt430$IF),0,sum(lt430$FG))
Gt4_30 <- c(sum(lt430$EG),sum(lt430$IG),sum(lt430$FG),0)
Langt4_1_d30 <- cbind(Et4_30,It4_30,Ft4_30,Gt4_30)
Lt430d <- as.dist(Langt4_1_d30)
cLt430d <- hclust(Lt430d,method="single")
plot(as.dendrogram(cLt430d),horiz = TRUE,cex.axis=2)
Et4_31 <- c(0,sum(lt431$EI),sum(lt431$EF),sum(lt431$EG))
It4_31 <- c(sum(lt431$EI),0,sum(lt431$IF),sum(lt431$IG))
Ft4_31 <- c(sum(lt431$EF),sum(lt431$IF),0,sum(lt431$FG))
Gt4_31 <- c(sum(lt431$EG),sum(lt431$IG),sum(lt431$FG),0)
Langt4_1_d31 <- cbind(Et4_31,It4_31,Ft4_31,Gt4_31)
Lt431d <- as.dist(Langt4_1_d31)
cLt431d <- hclust(Lt431d,method="single")
plot(as.dendrogram(cLt431d),horiz = TRUE,cex.axis=2)
Et4_32 <- c(0,sum(lt432$EI),sum(lt432$EF),sum(lt432$EG))
It4_32 <- c(sum(lt432$EI),0,sum(lt432$IF),sum(lt432$IG))
Ft4_32 <- c(sum(lt432$EF),sum(lt432$IF),0,sum(lt432$FG))
Gt4_32 <- c(sum(lt432$EG),sum(lt432$IG),sum(lt432$FG),0)
Langt4_1_d32 <- cbind(Et4_32,It4_32,Ft4_32,Gt4_32)
Lt432d <- as.dist(Langt4_1_d32)
cLt432d <- hclust(Lt432d,method="single")
plot(as.dendrogram(cLt432d),horiz = TRUE,cex.axis=2)
Et4_33 <- c(0,sum(lt433$EI),sum(lt433$EF),sum(lt433$EG))
It4_33 <- c(sum(lt433$EI),0,sum(lt433$IF),sum(lt433$IG))
Ft4_33 <- c(sum(lt433$EF),sum(lt433$IF),0,sum(lt433$FG))
Gt4_33 <- c(sum(lt433$EG),sum(lt433$IG),sum(lt433$FG),0)
Langt4_1_d33 <- cbind(Et4_33,It4_33,Ft4_33,Gt4_33)
Lt433d <- as.dist(Langt4_1_d33)
cLt433d <- hclust(Lt433d,method="single")
plot(as.dendrogram(cLt433d),horiz = TRUE,cex.axis=2)
Et4_34 <- c(0,sum(lt434$EI),sum(lt434$EF),sum(lt434$EG))
It4_34 <- c(sum(lt434$EI),0,sum(lt434$IF),sum(lt434$IG))
Ft4_34 <- c(sum(lt434$EF),sum(lt434$IF),0,sum(lt434$FG))
Gt4_34 <- c(sum(lt434$EG),sum(lt434$IG),sum(lt434$FG),0)
Langt4_1_d34 <- cbind(Et4_34,It4_34,Ft4_34,Gt4_34)
Lt434d <- as.dist(Langt4_1_d34)
cLt434d <- hclust(Lt434d,method="single")
plot(as.dendrogram(cLt434d),horiz = TRUE,cex.axis=2)
Et4_35 <- c(0,sum(lt435$EI),sum(lt435$EF),sum(lt435$EG))
It4_35 <- c(sum(lt435$EI),0,sum(lt435$IF),sum(lt435$IG))
Ft4_35 <- c(sum(lt435$EF),sum(lt435$IF),0,sum(lt435$FG))
Gt4_35 <- c(sum(lt435$EG),sum(lt435$IG),sum(lt435$FG),0)
Langt4_1_d35 <- cbind(Et4_35,It4_35,Ft4_35,Gt4_35)
Lt435d <- as.dist(Langt4_1_d35)
cLt435d <- hclust(Lt435d,method="single")
plot(as.dendrogram(cLt435d),horiz = TRUE,cex.axis=2)
Et4_36 <- c(0,sum(lt436$EI),sum(lt436$EF),sum(lt436$EG))
It4_36 <- c(sum(lt436$EI),0,sum(lt436$IF),sum(lt436$IG))
Ft4_36 <- c(sum(lt436$EF),sum(lt436$IF),0,sum(lt436$FG))
Gt4_36 <- c(sum(lt436$EG),sum(lt436$IG),sum(lt436$FG),0)
Langt4_1_d36 <- cbind(Et4_36,It4_36,Ft4_36,Gt4_36)
Lt436d <- as.dist(Langt4_1_d36)
cLt436d <- hclust(Lt436d,method="single")
plot(as.dendrogram(cLt436d),horiz = TRUE,cex.axis=2)
Et4_37 <- c(0,sum(lt437$EI),sum(lt437$EF),sum(lt437$EG))
It4_37 <- c(sum(lt437$EI),0,sum(lt437$IF),sum(lt437$IG))
Ft4_37 <- c(sum(lt437$EF),sum(lt437$IF),0,sum(lt437$FG))
Gt4_37 <- c(sum(lt437$EG),sum(lt437$IG),sum(lt437$FG),0)
Langt4_1_d37 <- cbind(Et4_37,It4_37,Ft4_37,Gt4_37)
Lt437d <- as.dist(Langt4_1_d37)
cLt437d <- hclust(Lt437d,method="single")
plot(as.dendrogram(cLt437d),horiz = TRUE,cex.axis=2)
Et4_38 <- c(0,sum(lt438$EI),sum(lt438$EF),sum(lt438$EG))
It4_38 <- c(sum(lt438$EI),0,sum(lt438$IF),sum(lt438$IG))
Ft4_38 <- c(sum(lt438$EF),sum(lt438$IF),0,sum(lt438$FG))
Gt4_38 <- c(sum(lt438$EG),sum(lt438$IG),sum(lt438$FG),0)
Langt4_1_d38 <- cbind(Et4_38,It4_38,Ft4_38,Gt4_38)
Lt438d <- as.dist(Langt4_1_d38)
cLt438d <- hclust(Lt438d,method="single")
plot(as.dendrogram(cLt438d),horiz = TRUE,cex.axis=2)
Et4_39 <- c(0,sum(lt439$EI),sum(lt439$EF),sum(lt439$EG))
It4_39 <- c(sum(lt439$EI),0,sum(lt439$IF),sum(lt439$IG))
Ft4_39 <- c(sum(lt439$EF),sum(lt439$IF),0,sum(lt439$FG))
Gt4_39 <- c(sum(lt439$EG),sum(lt439$IG),sum(lt439$FG),0)
Langt4_1_d39 <- cbind(Et4_39,It4_39,Ft4_39,Gt4_39)
Lt439d <- as.dist(Langt4_1_d39)
cLt439d <- hclust(Lt439d,method="single")
plot(as.dendrogram(cLt439d),horiz = TRUE,cex.axis=2)
Et4_40 <- c(0,sum(lt440$EI),sum(lt440$EF),sum(lt440$EG))
It4_40 <- c(sum(lt440$EI),0,sum(lt440$IF),sum(lt440$IG))
Ft4_40 <- c(sum(lt440$EF),sum(lt440$IF),0,sum(lt440$FG))
Gt4_40 <- c(sum(lt440$EG),sum(lt440$IG),sum(lt440$FG),0)
Langt4_1_d40 <- cbind(Et4_40,It4_40,Ft4_40,Gt4_40)
Lt440d <- as.dist(Langt4_1_d40)
cLt440d <- hclust(Lt440d,method="single")
plot(as.dendrogram(cLt440d),horiz = TRUE,cex.axis=2)
Et4_41 <- c(0,sum(lt441$EI),sum(lt441$EF),sum(lt441$EG))
It4_41 <- c(sum(lt441$EI),0,sum(lt441$IF),sum(lt441$IG))
Ft4_41 <- c(sum(lt441$EF),sum(lt441$IF),0,sum(lt441$FG))
Gt4_41 <- c(sum(lt441$EG),sum(lt441$IG),sum(lt441$FG),0)
Langt4_1_d41 <- cbind(Et4_41,It4_41,Ft4_41,Gt4_41)
Lt441d <- as.dist(Langt4_1_d41)
cLt441d <- hclust(Lt441d,method="single")
plot(as.dendrogram(cLt441d),horiz = TRUE,cex.axis=2)
Et4_42 <- c(0,sum(lt442$EI),sum(lt442$EF),sum(lt442$EG))
It4_42 <- c(sum(lt442$EI),0,sum(lt442$IF),sum(lt442$IG))
Ft4_42 <- c(sum(lt442$EF),sum(lt442$IF),0,sum(lt442$FG))
Gt4_42 <- c(sum(lt442$EG),sum(lt442$IG),sum(lt442$FG),0)
Langt4_1_d42 <- cbind(Et4_42,It4_42,Ft4_42,Gt4_42)
Lt442d <- as.dist(Langt4_1_d42)
cLt442d <- hclust(Lt442d,method="single")
plot(as.dendrogram(cLt442d),horiz = TRUE,cex.axis=2)
Et4_43 <- c(0,sum(lt443$EI),sum(lt443$EF),sum(lt443$EG))
It4_43 <- c(sum(lt443$EI),0,sum(lt443$IF),sum(lt443$IG))
Ft4_43 <- c(sum(lt443$EF),sum(lt443$IF),0,sum(lt443$FG))
Gt4_43 <- c(sum(lt443$EG),sum(lt443$IG),sum(lt443$FG),0)
Langt4_1_d43 <- cbind(Et4_43,It4_43,Ft4_43,Gt4_43)
Lt443d <- as.dist(Langt4_1_d43)
cLt443d <- hclust(Lt443d,method="single")
plot(as.dendrogram(cLt443d),horiz = TRUE,cex.axis=2)
Et4_44 <- c(0,sum(lt444$EI),sum(lt444$EF),sum(lt444$EG))
It4_44 <- c(sum(lt444$EI),0,sum(lt444$IF),sum(lt444$IG))
Ft4_44 <- c(sum(lt444$EF),sum(lt444$IF),0,sum(lt444$FG))
Gt4_44 <- c(sum(lt444$EG),sum(lt444$IG),sum(lt444$FG),0)
Langt4_1_d44 <- cbind(Et4_44,It4_44,Ft4_44,Gt4_44)
Lt444d <- as.dist(Langt4_1_d44)
cLt444d <- hclust(Lt444d,method="single")
plot(as.dendrogram(cLt444d),horiz = TRUE,cex.axis=2)
Et4_45 <- c(0,sum(lt445$EI),sum(lt445$EF),sum(lt445$EG))
It4_45 <- c(sum(lt445$EI),0,sum(lt445$IF),sum(lt445$IG))
Ft4_45 <- c(sum(lt445$EF),sum(lt445$IF),0,sum(lt445$FG))
Gt4_45 <- c(sum(lt445$EG),sum(lt445$IG),sum(lt445$FG),0)
Langt4_1_d45 <- cbind(Et4_45,It4_45,Ft4_45,Gt4_45)
Lt445d <- as.dist(Langt4_1_d45)
cLt445d <- hclust(Lt445d,method="single")
plot(as.dendrogram(cLt445d),horiz = TRUE,cex.axis=2)
Et4_46 <- c(0,sum(lt446$EI),sum(lt446$EF),sum(lt446$EG))
It4_46 <- c(sum(lt446$EI),0,sum(lt446$IF),sum(lt446$IG))
Ft4_46 <- c(sum(lt446$EF),sum(lt446$IF),0,sum(lt446$FG))
Gt4_46 <- c(sum(lt446$EG),sum(lt446$IG),sum(lt446$FG),0)
Langt4_1_d46 <- cbind(Et4_46,It4_46,Ft4_46,Gt4_46)
Lt446d <- as.dist(Langt4_1_d46)
cLt446d <- hclust(Lt446d,method="single")
plot(as.dendrogram(cLt446d),horiz = TRUE,cex.axis=2)
Et4_47 <- c(0,sum(lt447$EI),sum(lt447$EF),sum(lt447$EG))
It4_47 <- c(sum(lt447$EI),0,sum(lt447$IF),sum(lt447$IG))
Ft4_47 <- c(sum(lt447$EF),sum(lt447$IF),0,sum(lt447$FG))
Gt4_47 <- c(sum(lt447$EG),sum(lt447$IG),sum(lt447$FG),0)
Langt4_1_d47 <- cbind(Et4_47,It4_47,Ft4_47,Gt4_47)
Lt447d <- as.dist(Langt4_1_d47)
cLt447d <- hclust(Lt447d,method="single")
plot(as.dendrogram(cLt447d),horiz = TRUE,cex.axis=2)
Et4_48 <- c(0,sum(lt448$EI),sum(lt448$EF),sum(lt448$EG))
It4_48 <- c(sum(lt448$EI),0,sum(lt448$IF),sum(lt448$IG))
Ft4_48 <- c(sum(lt448$EF),sum(lt448$IF),0,sum(lt448$FG))
Gt4_48 <- c(sum(lt448$EG),sum(lt448$IG),sum(lt448$FG),0)
Langt4_1_d48 <- cbind(Et4_48,It4_48,Ft4_48,Gt4_48)
Lt448d <- as.dist(Langt4_1_d48)
cLt448d <- hclust(Lt448d,method="single")
plot(as.dendrogram(cLt448d),horiz = TRUE,cex.axis=2)
Et4_49 <- c(0,sum(lt449$EI),sum(lt449$EF),sum(lt449$EG))
It4_49 <- c(sum(lt449$EI),0,sum(lt449$IF),sum(lt449$IG))
Ft4_49 <- c(sum(lt449$EF),sum(lt449$IF),0,sum(lt449$FG))
Gt4_49 <- c(sum(lt449$EG),sum(lt449$IG),sum(lt449$FG),0)
Langt4_1_d49 <- cbind(Et4_49,It4_49,Ft4_49,Gt4_49)
Lt449d <- as.dist(Langt4_1_d49)
cLt449d <- hclust(Lt449d,method="single")
plot(as.dendrogram(cLt449d),horiz = TRUE,cex.axis=2)
Et4_50 <- c(0,sum(lt450$EI),sum(lt450$EF),sum(lt450$EG))
It4_50 <- c(sum(lt450$EI),0,sum(lt450$IF),sum(lt450$IG))
Ft4_50 <- c(sum(lt450$EF),sum(lt450$IF),0,sum(lt450$FG))
Gt4_50 <- c(sum(lt450$EG),sum(lt450$IG),sum(lt450$FG),0)
Langt4_1_d50 <- cbind(Et4_50,It4_50,Ft4_50,Gt4_50)
Lt450d <- as.dist(Langt4_1_d50)
cLt450d <- hclust(Lt450d,method="single")
plot(as.dendrogram(cLt450d),horiz = TRUE,cex.axis=2)



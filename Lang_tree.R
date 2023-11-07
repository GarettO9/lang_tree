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
swadesh3$English = c("English","i", "you", "he","we", 
                     "you", "they", "this", "that", "here", "there", 
                     "who", "what", "where", "when", "how", "not", 
                     "all", "many", "some", "few", "other", "one", 
                     "two", "three", "four", "five", "big", "long", 
                     "wide", "thick", "heavy", "small", "short", "narrow", 
                     "thin", "woman", "man", "man", "child", "wife", 
                     "husband", "mother", "father", "animal", "fish", 
                     "bird", "dog", "louse", "snake", "worm", "tree", 
                     "forest", "stick", "fruit", "seed", "leaf", "root", 
                     "bark", "flower", "grass", "rope", "skin", "meat", 
                     "blood", "bone", "fat", "egg", "horn", "tail", 
                     "feather", "hair", "head", "ear", "eye", "nose", 
                     "mouth", "tooth", "tongue", "fingernail", "foot", 
                     "leg", "knee", "hand", "wing", "belly", "guts", 
                     "neck", "back", "breast", "heart", "liver", "drink", 
                     "eat", "bite", "suck", "spit", "vomit", 
                     "blow", "breathe", "laugh", "see", "hear", 
                     "know", "think", "smell", "fear", "sleep", 
                     "live", "die", "kill", "fight", "hunt", 
                     "hit", "cut", "split", "stab", "scratch", 
                     "dig", "swim", "fly", "walk", "come", 
                     "lie", "sit", "stand", 
                     "turn", "fall", "give", "hold", 
                     "squeeze", "rub", "wash", "wipe", "pull", 
                     "push", "throw", "tie", "sew", "count", 
                     "say", "sing", "play", "float", "flow", 
                     "freeze", "swell", "sun", "moon", "star", 
                     "water", "rain", "river", "lake", "sea", "salt", 
                     "stone", "sand", "dust", "earth", "cloud", "fog", 
                     "sky", "wind", "snow", "ice", "smoke", "fire", 
                     "ash", "burn", "road", "mountain", "red", "green", 
                     "yellow", "white", "black", "night", "day", "year", 
                     "warm", "cold", "full", "new", "old", "good", 
                     "bad", "rotten", "dirty", "straight", "round", 
                     "sharp", "dull", "smooth", "wet", 
                     "dry", "correct", "near", "far", "right", "left", 
                     "at", "in", "with", "and", "if", "because", "name")

swadesh3$French = c("French","je","tu","il","nous","vous","ils","ceci","cela",
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
swadesh3$German = c("German","ich","du","er","wir","ihr","sie","dieser","der",
                    "hier","da","wer","was","wo","wann","wie","nicht","alle",
                    "viele","einige","wenig","anderer","eins","zwei","drei","vier",
                    "fünf","groß","lang","breit","dick","schwer","klein","kurz",
                    "eng","dünn","frau","mann","mensch","kind","frau","mann",
                    "mutter","vater","tier","fisch","vogel","hund","laus","schlange",
                    "wurm","baum","forst","stock","frucht","samen","blatt","wurzel","borke",
                    "blume","gras","leine","haut","fleisch","blut","knochen","fett",
                    "ei","horn","schwanz","feder","haar","haupt","ohr","auge",
                    "nase","mund","zahn","zunge","fingernagel","fuß","bein",
                    "knie","hand","fittich","bauch","eingeweide","hals",
                    "rücken","brust","herz","leber","trinken","essen","beißen",
                    "lutschen","speien","brechen","blasen","atmen","lachen","sehen",
                    "hören","kennen","denken","riechen","fürchten","schlafen",
                    "leben","sterben","töten","fechten","jagen","schlagen",
                    "schneiden","spalten","stechen","kratzen","graben",
                    "schwimmen","fliegen","gehen","kommen","liegen","sitzen",
                    "stehen","drehen","fallen","geben","halten","quetschen",
                    "reiben","waschen","wischen","ziehen","drücken","werfen",
                    "binden","nähen","rechnen","sagen","singen","spielen",
                    "schweben","fließen","frieren","schwellen","sonne","mond",
                    "stern","wasser","regen","fluss","see","meer","salz",
                    "stein","sand","staub","erde","wolke","nebel","himmel","wind",
                    "schnee","eis","rauch","feuer","asche","breenen","straße",
                    "berg","rot","grün","gelb","weiß","schwarz","nacht","tag",
                    "jahr","warm","kalt","voll","neu","alt","gut","schlecht",
                    "faul","schmutzig","gerade","rund","scharf","stumpf","glatt",
                    "nass","dürr","richtig","nah","fern","rechst","links","an",
                    "in","mit","und","ob","weil","name")
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
    ls1df[i,4] <- min(ls1df[i,2:3])
    ls1df[i,5] <- -min(ls1df[i,2:3])
    ls1df[i,6] <- -min(ls1df[i,2:3])
  }
  else if((ls1df[i,2]==min(ls1df[i,1:3]))&(ls1df[i,2]<min(c(ls1df[i,1],ls1df[i,3])))){
    ls1df[i,4] <- -min(c(ls1df[i,1],ls1df[i,3]))
    ls1df[i,5] <- min(c(ls1df[i,1],ls1df[i,3]))
    ls1df[i,6] <- -min(c(ls1df[i,1],ls1df[i,3]))
  }
  else if((ls1df[i,3]==min(ls1df[i,1:3]))&(ls1df[i,3]<min(ls1df[i,1:2]))){
    ls1df[i,4] <- -min(ls1df[i,1:2])
    ls1df[i,5] <- -min(ls1df[i,1:2])
    ls1df[i,6] <- min(ls1df[i,1:2])
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,2])&r<0.5){
    ls1df[i,4] <- -max(ls1df[i,1:3])
    ls1df[i,5] <- max(ls1df[i,1:3])
    ls1df[i,6] <- -max(ls1df[i,1:3])
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,2])&r>0.5){
    ls1df[i,4] <- max(ls1df[i,1:3])
    ls1df[i,5] <- -max(ls1df[i,1:3])
    ls1df[i,6] <- -max(ls1df[i,1:3])
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,3])&r<0.5){
    ls1df[i,4] <- max(ls1df[i,1:3])
    ls1df[i,5] <- -max(ls1df[i,1:3])
    ls1df[i,6] <- -max(ls1df[i,1:3])
  }
  else if((ls1df[i,1]==min(ls1df[i,1:3]))&(ls1df[i,1]==ls1df[i,3])&r>0.5){
    ls1df[i,4] <- -max(ls1df[i,1:3])
    ls1df[i,5] <- -max(ls1df[i,1:3])
    ls1df[i,6] <- max(ls1df[i,1:3])
  }
  else if(r<0.5){
    ls1df[i,4] <- -max(ls1df[i,1:3])
    ls1df[i,5] <- max(ls1df[i,1:3])
    ls1df[i,6] <- -max(ls1df[i,1:3])
  }
  else{
    ls1df[i,4] <- -max(ls1df[i,1:3])
    ls1df[i,5] <- -max(ls1df[i,1:3])
    ls1df[i,6] <- max(ls1df[i,1:3])
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
    ls2df[i,4] <- min(ls2df[i,2:3])
    ls2df[i,5] <- -min(ls2df[i,2:3])
    ls2df[i,6] <- -min(ls2df[i,2:3])
  }
  else if((ls2df[i,2]==min(ls2df[i,1:3]))&(ls2df[i,2]<min(c(ls2df[i,1],ls2df[i,3])))){
    ls2df[i,4] <- -min(c(ls2df[i,1],ls2df[i,3]))
    ls2df[i,5] <- min(c(ls2df[i,1],ls2df[i,3]))
    ls2df[i,6] <- -min(c(ls2df[i,1],ls2df[i,3]))
  }
  else if((ls2df[i,3]==min(ls2df[i,1:3]))&(ls2df[i,3]<min(ls2df[i,1:2]))){
    ls2df[i,4] <- -min(ls2df[i,1:2])
    ls2df[i,5] <- -min(ls2df[i,1:2])
    ls2df[i,6] <- min(ls2df[i,1:2])
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,2])&r<0.5){
    ls2df[i,4] <- -max(ls2df[i,1:3])
    ls2df[i,5] <- max(ls2df[i,1:3])
    ls2df[i,6] <- -max(ls2df[i,1:3])
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,2])&r>0.5){
    ls2df[i,4] <- max(ls2df[i,1:3])
    ls2df[i,5] <- -max(ls2df[i,1:3])
    ls2df[i,6] <- -max(ls2df[i,1:3])
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,3])&r<0.5){
    ls2df[i,4] <- max(ls2df[i,1:3])
    ls2df[i,5] <- -max(ls2df[i,1:3])
    ls2df[i,6] <- -max(ls2df[i,1:3])
  }
  else if((ls2df[i,1]==min(ls2df[i,1:3]))&(ls2df[i,1]==ls2df[i,3])&r>0.5){
    ls2df[i,4] <- -max(ls2df[i,1:3])
    ls2df[i,5] <- -max(ls2df[i,1:3])
    ls2df[i,6] <- max(ls2df[i,1:3])
  }
  else if(r<0.5){
    ls2df[i,4] <- -max(ls2df[i,1:3])
    ls2df[i,5] <- max(ls2df[i,1:3])
    ls2df[i,6] <- -max(ls2df[i,1:3])
  }
  else{
    ls2df[i,4] <- -max(ls2df[i,1:3])
    ls2df[i,5] <- -max(ls2df[i,1:3])
    ls2df[i,6] <- max(ls2df[i,1:3])
  }
}
colMeans(ls1df[,4:6])
colMeans(ls2df[,4:6])
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
    ls3df[i,4] <- min(ls3df[i,2:3])
    ls3df[i,5] <- -min(ls3df[i,2:3])
    ls3df[i,6] <- -min(ls3df[i,2:3])
  }
  else if((ls3df[i,2]==min(ls3df[i,1:3]))&(ls3df[i,2]<min(c(ls3df[i,1],ls3df[i,3])))){
    ls3df[i,4] <- -min(c(ls3df[i,1],ls3df[i,3]))
    ls3df[i,5] <- min(c(ls3df[i,1],ls3df[i,3]))
    ls3df[i,6] <- -min(c(ls3df[i,1],ls3df[i,3]))
  }
  else if((ls3df[i,3]==min(ls3df[i,1:3]))&(ls3df[i,3]<min(ls3df[i,1:2]))){
    ls3df[i,4] <- -min(ls3df[i,1:2])
    ls3df[i,5] <- -min(ls3df[i,1:2])
    ls3df[i,6] <- min(ls3df[i,1:2])
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,2])&r<0.5){
    ls3df[i,4] <- -max(ls3df[i,1:3])
    ls3df[i,5] <- max(ls3df[i,1:3])
    ls3df[i,6] <- -max(ls3df[i,1:3])
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,2])&r>0.5){
    ls3df[i,4] <- max(ls3df[i,1:3])
    ls3df[i,5] <- -max(ls3df[i,1:3])
    ls3df[i,6] <- -max(ls3df[i,1:3])
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,3])&r<0.5){
    ls3df[i,4] <- max(ls3df[i,1:3])
    ls3df[i,5] <- -max(ls3df[i,1:3])
    ls3df[i,6] <- -max(ls3df[i,1:3])
  }
  else if((ls3df[i,1]==min(ls3df[i,1:3]))&(ls3df[i,1]==ls3df[i,3])&r>0.5){
    ls3df[i,4] <- -max(ls3df[i,1:3])
    ls3df[i,5] <- -max(ls3df[i,1:3])
    ls3df[i,6] <- max(ls3df[i,1:3])
  }
  else if(r<0.5){
    ls3df[i,4] <- -max(ls3df[i,1:3])
    ls3df[i,5] <- max(ls3df[i,1:3])
    ls3df[i,6] <- -max(ls3df[i,1:3])
  }
  else{
    ls3df[i,4] <- -max(ls3df[i,1:3])
    ls3df[i,5] <- -max(ls3df[i,1:3])
    ls3df[i,6] <- max(ls3df[i,1:3])
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
    ls4df[i,4] <- min(ls4df[i,2:3])
    ls4df[i,5] <- -min(ls4df[i,2:3])
    ls4df[i,6] <- -min(ls4df[i,2:3])
  }
  else if((ls4df[i,2]==min(ls4df[i,1:3]))&(ls4df[i,2]<min(c(ls4df[i,1],ls4df[i,3])))){
    ls4df[i,4] <- -min(c(ls4df[i,1],ls4df[i,3]))
    ls4df[i,5] <- min(c(ls4df[i,1],ls4df[i,3]))
    ls4df[i,6] <- -min(c(ls4df[i,1],ls4df[i,3]))
  }
  else if((ls4df[i,3]==min(ls4df[i,1:3]))&(ls4df[i,3]<min(ls4df[i,1:2]))){
    ls4df[i,4] <- -min(ls4df[i,1:2])
    ls4df[i,5] <- -min(ls4df[i,1:2])
    ls4df[i,6] <- min(ls4df[i,1:2])
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,2])&r<0.5){
    ls4df[i,4] <- -max(ls4df[i,1:3])
    ls4df[i,5] <- max(ls4df[i,1:3])
    ls4df[i,6] <- -max(ls4df[i,1:3])
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,2])&r>0.5){
    ls4df[i,4] <- max(ls4df[i,1:3])
    ls4df[i,5] <- -max(ls4df[i,1:3])
    ls4df[i,6] <- -max(ls4df[i,1:3])
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,3])&r<0.5){
    ls4df[i,4] <- max(ls4df[i,1:3])
    ls4df[i,5] <- -max(ls4df[i,1:3])
    ls4df[i,6] <- -max(ls4df[i,1:3])
  }
  else if((ls4df[i,1]==min(ls4df[i,1:3]))&(ls4df[i,1]==ls4df[i,3])&r>0.5){
    ls4df[i,4] <- -max(ls4df[i,1:3])
    ls4df[i,5] <- -max(ls4df[i,1:3])
    ls4df[i,6] <- max(ls4df[i,1:3])
  }
  else if(r<0.5){
    ls4df[i,4] <- -max(ls4df[i,1:3])
    ls4df[i,5] <- max(ls4df[i,1:3])
    ls4df[i,6] <- -max(ls4df[i,1:3])
  }
  else{
    ls4df[i,4] <- -max(ls4df[i,1:3])
    ls4df[i,5] <- -max(ls4df[i,1:3])
    ls4df[i,6] <- max(ls4df[i,1:3])
  }
}
colMeans(ls3df[,4:6])
colMeans(ls4df[,4:6])


library(nnet);
nb_data_appr = 500;
nb_data_test = 1000;

# On teste des echantillons de base pour les reseaux de neurones

MAppr <- matrix(runif(2*nb_data_appr,1,10), ncol = 2, dimnames=list(1:nb_data_appr,c("x", "y")));
plot(MAppr);
MSep <- (MAppr[,1]<6)*(MAppr[,2]<6);
Donnees <- data.frame(MAppr, MSep);
Noms <- c("x", "y", "Classe");
names(Donnees) <- Noms;
# On classifie les echantillons de telle manière que les points sont affectes de true si il on une abscisse 
# superieure à 5

Donnees <- Donnees[order(Donnees$Classe),];
Donnees$Classe <- c(1,0)[as.factor(Donnees$Classe)];
plot(Donnees$x, Donnees$y, col = c("blue", "red")[as.factor(Donnees$Classe)]);

# On peut maintenant s'interesser aux reseaux de neurones
# On cree un resau puis on effectue son apprentissage

Input <- data.frame(Donnees$x, Donnees$y);
Output <- data.frame(Donnees$Classe);
model <- nnet(Input,Output,size = 10);

#On cree maintenant un jeu de données test pour voir les performances de notre réseau
MTest <- matrix(runif(2*nb_data_test,1,10), ncol = 2, dimnames=list(1:nb_data_test,c("x", "y")));
DonneesExit <- data.frame(MTest, rep(0, times = dim(MTest)[1]));
names(DonneesExit) <- c('x', 'y', 'Classe');
#Et maintenant l'heure de verite ou on voit si notre reseau de neurone nous ressort la bonne classification
DonneesExit$Classe <- predict(model, newdata = MTest);
DonneesExit$Classe <- round(DonneesExit$Classe);
names(DonneesExit) <- c('x', 'y', 'Classe');
plot(DonneesExit$x, DonneesExit$y, col = c("blue", "red")[as.factor(DonneesExit$Classe)]);

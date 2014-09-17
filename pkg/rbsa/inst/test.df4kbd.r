#
# 14_02_14 14_03_02 14_03_03 14_03_05 14_03_06
# 14_03_26 14_03_28 14_04_02 14_06_02 14_08_29
#
for (ff in Sys.glob("*.code.r")) {
  cat("<<<",ff,">>>\n");
  source(ff);
}
#
ndf <- 0;
#
ndf <- ndf+1;
defi <- list(
         Sexe=list(
              question="genre ?",
              a7type="character",
              a7possibilities=c("F","G")
                ),
         Age=list(
              question="Nombre d'années ?",
              a7type="numeric"
                ),
         Prenom=list(
              question="prénom ?",
              a7type="character"
                ),
         Nom=list(
              question="nom ?",
              a7type="character"
                )
            );
dafr <- df4kbd(defi);
print(dafr);
pause(paste("It was data.frame number",ndf));
#
ndf <- ndf+1;
defi <- list(
         q1=list(
              question="un nombre",
              a7type="numeric"
                )
            );
dafr <- df4kbd(defi);
print(dafr);
pause(paste("It was data.frame number",ndf));
#
nqu <- 0;
#
nqu <- nqu+1;
rep <- inquiry(question="Hungry ?",
               help="Si 'oui', je mets à cuire le repas",
               a7possibility="O",
               a7default="non",
               a7possibilities=c("oui","non"),format=2
              );
if (is.null(rep)) {
  form3title("NULL answer");
} else {
  form3title(rep);
}
pause(paste("It was question number",nqu));
#
nqu <- nqu+1;
rep <- inquiry(question="Un vecteur de quatre entiers positifs",
               a7type="integer",
               a7length=4,
               a7default=NULL,
               a7possibilities=c(1,Inf),format=2
              );
if (is.null(rep)) {
  form3title("NULL answer !");
} else {
  form3title(as.character(rep));
}
pause(paste("It was question number",nqu));
#
nqu <- nqu+1;
rep <- inquiry(question="J'attends DEUX paragraphes !",
               help="Il faut donc mettre '//' dans l'ensemble de la réponse",
               a7length=2,
               a7possibilities=NULL,
               a7default=NULL,
               format=2
              );
if (is.null(rep)) {
  form3title("NULL answer");
} else {
  form3title(rep[1]);
  form3title(rep[2]);
}
pause(paste("It was question number",nqu));
#
nqu <- nqu+1;
form3title("mettre entre \" pour lancer les possibilités",7);
rep <- inquiry(question="Hungry ?",
               help=c("Si 'oui', je mets à cuire le repas",
                      "mais avant 19h car je dois d'abord faire les courses"),
               a7possibility="quoi ?",
               a7default="o",
               a7possibilities=c(o="C'est plutôt d'accord",
                                 O="Oui car j'ai très faim",
                                 y="peut-être",
                                 n="je préfère attendre demain",
                                 N="JAMAIS !"),
               format=2
              );
if (is.null(rep)) {
  form3title("NULL answer");
} else {
  form3title(rep);
}
pause(paste("It was question number",nqu));
#
nqu <- nqu+1;
rep <- inquiry(question="Combien de calories ?",
               help=c("à consommer sur l'ensemble du menu",
                      "il faut manger mais pas trop non plus!",
                      "Nous vous souhaitons BON APPÉTIT !"),
               a7type="numeric",
               a7default=2000,
               a7possibilities=c(500,3500),format=2
              );
if (is.null(rep)) {
  form3title("NULL answer !");
} else {
  form3title(as.character(rep));
}
pause(paste("It was question number",nqu));
#
form3title("'test.df4kbd.r' finished its job!",8);

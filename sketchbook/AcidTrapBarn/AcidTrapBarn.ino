#include <Time.h>
#include <TimeLib.h>


//ce code permet la mesure de pression (kilo pascal) et la résitance de sondes a température

char analogPinName[6][2] = {"T","1","2","3","4","A"}; // définie le nom des poste mesures
int analogPinDP_P[]= {0,1,2,3,4,5}; // définie pin analogiques pour les mesures de pression
int analogPinTemp[]= {10,11,12,13,14,15}; // définie pin analogiques pour les mesures de pression

float RR[]={10000,10000,10000,10000,10000,10000}; // Résistance de référence (ohm)

float T_I[]={0,0,0,0,0,0};
float DP_P_I[]={0,0,0,0,0,0};
float T_INT[]={0,0,0,0,0,0};
float DP_P_INT[]={0,0,0,0,0,0};

int NbPoint=6; // nombre de points de mesure (DP et T sont toujours liés par paire à un point de mesure) au départ de A01 pour la pression différentielle et A11 pout la température
int Delais=50; // Temps entre deux séries de mesures, doit être inférieur à 800 milièmes de secondes
float TimeINT=0; // Temps intermédiare pour le la sélection des données en secondes
int INT= 2; // temps d'integration en secondes pas plus petit que 1 seconde, pas de nombre avec decimales 30
int DelaisDepart=0; // délais avant la première boucle (en millième de secondes), c est à dire l etemps nécessaire au démarrage de script denregistrement dans R
int FinMes=24*365; // Duree des mesures en heure
int NbMes=0; // Nombre de mesures sur un temps d'intgération
int NbBoucles=0; // Compteur du nombre de boucles
int i=1; // Compteur du nombre de points
int rawDP_P=0;// Lu en analog pour Pression différentielle et pression
int rawT= 0; //Lu en analog pour température

float DP_P=0; // Variable temporaire calculée, tension (volt)
float T=0; // Variable temporaire calculée, température (°C)

//////////////////////////////////////////////////////////////////////////////
void setup() {
  Serial.begin(9600);// Ouvre la communication avec les le port COM
}

//////////////////////////////////////////////////////////////////////////////
void loop(){

if(NbBoucles<1){
                delay(DelaisDepart);
                }
        
TimeINT=now()+INT;
 
NbBoucles++;
NbMes=0;

while(now()<TimeINT){
  NbMes=NbMes+1;
  //Boucle point de mesure
  i=1;
  while (i<=NbPoint){
                    DP_P_I[i-1]=analogRead(i-1);
                    DP_P_I[i-1]=-(DP_P_I[i-1]-34.816706)/0.847368/100*9806.6;
                    delay(Delais);
                    T_I[i-1]= (float) (1024.0/ (float) analogRead(i+10-1)-1)*RR[i-1];
                    T_I[i-1]=(1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log(T_I[i-1])+8.758753*pow(10,-8)*pow(log(T_I[i-1]),3))-273.15);
                    delay(Delais);
                    DP_P_INT[i-1]=DP_P_INT[i-1]+DP_P_I[i-1];
                    T_INT[i-1]=T_INT[i-1]+T_I[i-1];
                    i++;
                    }
}

//Serial.print("<Jour=");
Serial.print("<");
//Serial.print(";");
//Serial.print(day()-1);
//Serial.print(";");
//Serial.print("Heure=");
//Serial.print(";");
//Serial.print(hour());
//Serial.print(";");
//Serial.print("Minute=");
//Serial.print(";");
//Serial.print(minute());
//Serial.print(";");
//Serial.print("Secondes=");
//Serial.print(";");
//Serial.print(second());
//Serial.print(";");
//Serial.print("Horaire=");
//Serial.print(";");
Serial.print(now());
Serial.print(";");
i=1;
while (i<=NbPoint){
                  DP_P_INT[i-1]=DP_P_INT[i-1]/NbMes;
                  if(analogPinName[i-1]==analogPinName[NbPoint-1]){
                    //Serial.print("P_");
                    //Serial.print(analogPinName[i-1]);
                    //Serial.print("=");
                    //Serial.print(";");
                    Serial.print(DP_P_I[i-1]);
                    Serial.print(";");
                                             }
                  else{
                       //Serial.print("DP_");
                       //Serial.print(analogPinName[i-1]);
                       //Serial.print("=");
                       //Serial.print(";");
                       Serial.print(DP_P_I[i-1]);
                       Serial.print(";");
                        }
                  DP_P_INT[i-1]=0;
                  i++;
                  }

i=1;
while (i<=NbPoint){
                  T_INT[i-1]=T_INT[i-1]/NbMes;
                  //Serial.print("Temp_");
                  //Serial.print(analogPinName[i-1]);
                  //Serial.print("=");
                  //Serial.print(";");
                  Serial.print(T_INT[i-1]);
                  Serial.print(";");
                  T_INT[i-1]=0;
                  i++;
                  }

//Serial.print("Nombre_Mesure=");
Serial.print(NbMes);
Serial.println(">");
}
  



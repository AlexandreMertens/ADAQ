#include "RunningMedian.h"

float resRef[]= {10000.0,10000.0,10000.0,10000.0,10000.0}; // Valeur de resistance des résitances de référence 10000 ohm 6 valeurs car potentielleme 6 sondes par cellules

int state;
const byte buffSize = 40;
char inputBuffer[buffSize];
const char startMarker = '<';
const char endMarker = '>';
byte bytesRecvd = 0;
boolean readInProgress = false;
boolean newDataFromPC = false;
int newValue = 0;

const byte interruptPins[5] = {2, 3, 4, 5, 1}; // lecture du 1 est le CVP de référence (lu en 5e position dans la liste de lecture et écriture)

double waveLength[5] = {0, 0, 0, 0, 0};
double tStart[5] = {0, 0, 0, 0, 0};

RunningMedian psample[5] = RunningMedian(101);

double t = 0.0;
double p = 0.0;

int it = 0;

void setup() {
  // put your setup code here, to run once:

  state = 1;
  pinMode(LED_BUILTIN, OUTPUT);
  
  attachInterrupt(interruptPins[0], freqMeasure0, RISING);
  attachInterrupt(interruptPins[1], freqMeasure1, RISING);
  attachInterrupt(interruptPins[2], freqMeasure2, RISING);
  attachInterrupt(interruptPins[3], freqMeasure3, RISING);
  attachInterrupt(interruptPins[4], freqMeasure4, RISING);

  Serial.begin(9600);

  delay(200);
}

void loop() {
  // put your main code here, to run repeatedly:

  if (it > 10) {

    Serial.print("<state,"); Serial.print(state);
    for (int i = 0; i < 5; i++){
      
      float t, p, resMeas;
      double sum_t = 0;
      double sum_p = 0;
      int n = 10;
      
      for (int it = 0; it < n; it++){
        delay(10);
        resMeas = (float) (1024.0/ (float) analogRead(i+8)-1)*resRef[i]; 
        sum_t += (1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log(resMeas)+8.758753*pow(10,-8)*pow(log(resMeas),3))-273.15);
        }
      t = sum_t/n;
      Serial.print(","); Serial.print(t);  Serial.print(","); Serial.print(1000000/(waveLength[i])); 
     
    }
    Serial.println(">");   
    Serial.flush();
    it = 0;
    
  }
  delay(100);
  it++;

}

//=============
 

void freqMeasure0() {freqMeasure(0);}
void freqMeasure1() {freqMeasure(1);}
void freqMeasure2() {freqMeasure(2);}
void freqMeasure3() {freqMeasure(3);}
void freqMeasure4() {freqMeasure(4);}

void freqMeasure(int i){
    
  psample[i].add(micros()-tStart[i]);
  waveLength[i] = psample[i].getAverage();
  tStart[i] = micros(); 
}
 
void parseData() {

  newValue = atoi(inputBuffer);     // convert to an integer

}

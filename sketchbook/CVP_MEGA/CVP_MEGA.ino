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

const byte interruptPins[4] = {2, 3, 4, 5};

double waveLength[4] = {0, 0, 0, 0};
double tStart[4] = {0, 0, 0, 0};

RunningMedian psample[4] = RunningMedian(11);

double t = 0.0;
double p = 0.0;

int it = 0;

void setup() {
  // put your setup code here, to run once:

  state = 0;
  pinMode(LED_BUILTIN, OUTPUT);
  
  attachInterrupt(interruptPins[0], freqMeasure0, RISING);
  attachInterrupt(interruptPins[1], freqMeasure1, RISING);
  attachInterrupt(interruptPins[2], freqMeasure2, RISING);
  attachInterrupt(interruptPins[3], freqMeasure3, RISING);


  Serial.begin(9600);

  delay(200);

  Serial.println("<Ready>");

}

void loop() {
  // put your main code here, to run repeatedly:

  getDataFromPC();

  if (not readInProgress and newDataFromPC){
  
    state = newValue;
    newDataFromPC = false;

    if (state == 1) {
      Serial.println("<Running>");
    }
    else if (state == 0) {
      Serial.println("<Ready>");
    }
    else {
      Serial.println("<Stopped>");
    }
  }


  if (not readInProgress and state == 1 and it > 10) {

    Serial.print("<state,"); Serial.print(state);
    for (int i = 0; i < 5; i++){
      
      float t, p, resMeas;
      double sum_t = 0;
      double sum_p = 0;
      int n = 10;
      
      for (int it = 0; it < n; it++){
        delay(10);
        resMeas = (float) (1024.0/ (float) analogRead(i)-1)*resRef[i]; 
        sum_t += (1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log(resMeas)+8.758753*pow(10,-8)*pow(log(resMeas),3))-273.15);
        }
      t = sum_t/n;
      Serial.print(","); Serial.print(t);  Serial.print(","); Serial.print(waveLength[i]); 
    }
    Serial.println(">");   
    Serial.flush();
    it = 0;
    
  }
  delay(5000);
  it++;

}

//=============

void getDataFromPC() {

    // receive data from PC and save it into inputBuffer
    
  if(Serial.available() > 0) {

    char x = Serial.read();

      // the order of these IF clauses is significant
      
    if (x == endMarker) {
      readInProgress = false;
      newDataFromPC = true;
      inputBuffer[bytesRecvd] = 0;
      parseData();
    }
    
    if(readInProgress) {
      inputBuffer[bytesRecvd] = x;
      bytesRecvd ++;
      if (bytesRecvd == buffSize) {
        bytesRecvd = buffSize - 1;
      }
    }

    if (x == startMarker) { 
      bytesRecvd = 0; 
      readInProgress = true;
    }
  }
}

//=============
 

void freqMeasure0() {freqMeasure(0);}
void freqMeasure1() {freqMeasure(1);}
void freqMeasure2() {freqMeasure(2);}
void freqMeasure3() {freqMeasure(3);}


void freqMeasure(int i){
    
  psample[i].add(micros()-tStart[i]);
  waveLength[i] = psample[i].getAverage();
  tStart[i] = micros(); 
}
 
void parseData() {

  newValue = atoi(inputBuffer);     // convert to an integer

}

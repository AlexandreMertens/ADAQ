// Library
//#include <QuickStats.h>;
//#include <SPI.h>;
//#include <SD.h>;
//#include <Wire.h>;
//#include <RTClib.h>;
//#include <LiquidCrystal_I2C.h>

extern volatile unsigned long timer0_millis;
unsigned long new_value = 0;
int Site=1; // if Barn 2 if Store
char* SiteName[]={"Barn","Store"}; //Line names to be print
char* PositionName[]={"1","2","3","4","Ext"}; //Line names to be print
char* LineName[]={"1","2","3","4","5"}; //Line names to be print
//char* VanneName[]={"Line1_1","Line2_2","Line3_3","Line4_4","Line5_Ext"}; //names to be print

int DigitalPin[]={32,34,36,38,40};//pins one for each sampling point/vanne
char* VanneName[]={"Line1_1","Line2_2","Line3_3","Line4_4","Line5_Ext"}; //names to be print
bool RecoveryTestActivated=false; //true if recovery type of measurment. Then all lines exepted the ext have to be attached to the chamber were recovry test is performes
char* RecoveryTestSite="1";//  // Select this vanne name in the vanne name vector for all sampling line supposing that all sampling lines excepted the last one are racorded to this point. The last one is external 
char* ModeName[]={"Sampling","Recovery","DP_Control"};
int Mode=3;//1 for Sampling, 2 for Recovery",3 for Flow_Control"


int DigitalPinPosition=-1;//Position in the DigitalPin vector 0 to (SamplingPointNumber-1) leav -1 as default as condition to print the header line
int DigitalPinToHigh; // Next digital pin position to set to high
int DigitalPinToLow;// Next digital pin position to set to low
int SamplingPointNumber=0;// Number of sampling point
int AnalogPin[]={0,1,2,3};//pins one for each analog read
int AnalogPinNumber=0;// Number of analog read
int AnalogPinPosition;//Position in the AnalogPin vector 0 to (AnalogPinNumber-1)
long SamplingDuration=(60/(5*2.1))*60000;//
int DelayAnalogRead=1000;//
int PointCounter=0;//
int WashingSecurityDigitalPin=30;
int AnalysingSecurityDigitalPin=42;
int AnalogPinFequency=1000; // approxymate Frequency of analog pin measurment frequncy ha to be lower than SamplingDuration for recording values

float AnalogRaw[4];// object for recording raw analog read
int RawMeasureCounter=0;// object for counting number of raw analog read
int State=0;// object defining the state in the script
int EmergencySwitch=400; // object defining the limit value of raw analog read of pressure for moving to emergency state 
int resRef=10000; // referential resisitance for temperature convertion to °Celsius

char* Activity[]={"Washing","Analysing"};
char* AnalogicType[]={"Washing line","Analysing line"};
char* AnalogicParam[]={"Pressure","Pressure","Temperature","Temperature"};
char* Warnings[]={"Washing line blocked","Analysing Line blocked"};

float AnalogConverted[]={0,0,0,0};//first is last read, second is integrated Data
float AnalogConvertedNull[]={0,0,0,0};//first is last read, second is integrated Data
bool WarningFlag=false;
bool PrintCase=false;

unsigned long timer;

void setup(){
  Serial.begin(9600);
  if(PrintCase){
     Serial.println("In setup");   
  }
  SamplingPointNumber=(int) sizeof(DigitalPin)/sizeof(DigitalPin[0]);
  AnalogPinNumber=(int) sizeof(AnalogPin)/sizeof(AnalogPin[0]);
  WarningFlag=false;
  pinMode(AnalysingSecurityDigitalPin,OUTPUT);
  pinMode(WashingSecurityDigitalPin,OUTPUT);
  digitalWrite(AnalysingSecurityDigitalPin, HIGH);
  digitalWrite(WashingSecurityDigitalPin, HIGH);    
  
  if(ModeName[Mode-1]=="DP_Control"){
    EmergencySwitch=EmergencySwitch*1.5; // automatic increase of DP maximum value when DP tested to allow testing of etancheity of the system when in full depression level
  }
  
  for (int j=0; j < SamplingPointNumber; j++){
      pinMode(DigitalPin[j], OUTPUT);
      digitalWrite(DigitalPin[j], HIGH);
  }  
  delay(10);
}

void setMillis(unsigned long new_value){
  uint8_t oldSREG = SREG;
  cli();
  timer0_millis = new_value;
  SREG = oldSREG;
}

void loop(){
switch(State) {
   case 0: 
      if(PrintCase){
        Serial.println("In case 0");   
      }
      State=1;
      delay(10);
      break;
    
   case 1:
          if(PrintCase){
              Serial.println("In case 1");   
          }
          if(ModeName[Mode-1]=="Recovery"){
            for (int j=1; j < SamplingPointNumber; j++){// do not modify the RecoveryTestSite item nor the last one supposed to be the external
              if(PositionName[j]!=RecoveryTestSite){
               PositionName[j]=RecoveryTestSite;
              }
            }
          }
         if(DigitalPinPosition>-1){ 
            PointCounter=0;
           for (int i=0; i < AnalogPinNumber; i++){
             AnalogConverted[i]=0;
           }          
           while(millis()<SamplingDuration){
            delay(100);
            PointCounter=PointCounter+1;
            for (int i=0; i < AnalogPinNumber; i++){
              AnalogPinPosition=i;
              AnalogRaw[i]=0;
              AnalogRaw[i]=analogRead(AnalogPin[i]);
              if (AnalogicParam[i]=="Pressure"){
                 AnalogRaw[i]=((AnalogRaw[i]/1024)-0.04)/0.009*1000/100; // in hpa
                 if(AnalogRaw[i]>EmergencySwitch){// emergency activation winting warning
                    State=2;
                    break;
                 }
                 AnalogConverted[i]=AnalogConverted[i]+AnalogRaw[i];
                 if(State==2){//
                    break;
                 } 
              }
              if (AnalogicParam[i]=="Temperature"){
                 AnalogRaw[i] =(1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log((float) (1024.0/AnalogRaw[i]-1)*resRef)+8.758753*pow(10,-8)*pow(log((float) (1024.0/AnalogRaw[i]-1)*resRef),3))-273.15); 
                 AnalogConverted[i]= AnalogConverted[i]+AnalogRaw[i];
              }
            }
 
            if(State==2){//
               break;
            }
             
            delay(AnalogPinFequency);
            
            if(ModeName[Mode-1]=="DP_Control"){
              
              State=5;
              break;
            }
              
          }
          if(State==2){//
              break;
          } 
          if(ModeName[Mode]=="DP_Control"){
              State=5;
              break;
          }
        }
        State=5;
        break;
      
   case 2: // emergency action when warning activated. lead to case 3 but otherwise empty at the moment
          if(PrintCase){
              Serial.println("In case 2");   
          }       
        State=3;        
        break;   
   case 3: // emergency action all vanne switched off
        if(PrintCase){
              Serial.println("In case 3");   
        }
        digitalWrite(AnalysingSecurityDigitalPin, LOW);
        digitalWrite(WashingSecurityDigitalPin, LOW);    

        
        for (int j=0; j < SamplingPointNumber; j++){
          digitalWrite(DigitalPin[j], HIGH);
          delay(50);
        }
        WarningFlag=true;
        State=5;
        break;
   case 4: // Digital write conditions for vanne activation in common
        if(PrintCase){
              Serial.println("In case 4");   
        }
        if(DigitalPinPosition<SamplingPointNumber){
            
            if(DigitalPinPosition<(SamplingPointNumber-1)){
              DigitalPinPosition=DigitalPinPosition+1;
            }else{
              DigitalPinPosition=0;
            }
            if(DigitalPinPosition>=0){ 
              digitalWrite(AnalysingSecurityDigitalPin, HIGH);
              digitalWrite(WashingSecurityDigitalPin, HIGH);
            }  
            DigitalPinToLow=DigitalPinPosition;
            if(DigitalPinPosition>0){           
              DigitalPinToHigh=DigitalPinPosition-1;             
            }
            if(DigitalPinPosition==0){
              DigitalPinToHigh=SamplingPointNumber-1;   
            }
            digitalWrite(DigitalPin[DigitalPinToLow], LOW);
            delay(500);
            digitalWrite(DigitalPin[DigitalPinToHigh], HIGH);
                
            State=1;
            setMillis(new_value);
        }else{
          Serial.println("Error"); 
        }
        break;
   case 5:
        if(PrintCase){
              Serial.println("In case 5");   
        }
        if(DigitalPinPosition>-1){
            Serial.print("<");
            Serial.print (SiteName[Site-1]);
            Serial.print(",");
            if(DigitalPinPosition==(SamplingPointNumber-1)){
                Serial.print(PositionName[0]);
                Serial.print(",");
                Serial.print(PositionName[DigitalPinPosition]);
                Serial.print(",");
                Serial.print(LineName[0]);
                Serial.print(",");
                Serial.print(LineName[DigitalPinPosition]);
            }else{
                Serial.print(PositionName[DigitalPinPosition+1]);
                Serial.print(",");
                Serial.print(PositionName[DigitalPinPosition]);
                Serial.print(",");
                Serial.print(LineName[DigitalPinPosition+1]);                
                Serial.print(",");
                Serial.print(LineName[DigitalPinPosition]);
            }
            Serial.print(",");
            Serial.print(ModeName[Mode-1]);
            Serial.print(",");  
            for (int i=0; i < AnalogPinNumber; i++){
                Serial.print(AnalogConverted[i]/PointCounter);
                Serial.print(",");
            }
             Serial.print(PointCounter);
             Serial.print(",");
             Serial.print(SamplingDuration);
             Serial.print(",");
                
             if(AnalogPinFequency>SamplingDuration){
                Serial.print("Check Sampling duration and analog read duration ratio");
                Serial.print(" ");
             }
             if(WarningFlag){
                Serial.print(Warnings[AnalogPinPosition]);
                Serial.print(" ");
                Serial.print(AnalogRaw[AnalogPinPosition]);
                Serial.print(" hpa of differential pressure");
                Serial.println(">");
                while(WarningFlag){
                  }     
             }
             if((AnalogPinFequency<SamplingDuration)||(WarningFlag==false)){
             Serial.print(" ");
             }
             Serial.println(">");
            if(millis()<SamplingDuration){
              State=1;
             }else{
             State=4;}
         }else{
         State=4;
         }
  }
}



////////////////
///////////////
////////////////
///////////////
////////////////
///////////////
//
//
//
//// parameters
//LiquidCrystal_I2C lcd(0x3F,16,2);  // set the LCD address to 0x27 for a 16 chars and 2 line display
//int balanceID = 91;
//float minWeight = 350;
//
//// Call
//QuickStats stats;
//File monFichier;
//RTC_DS3231 rtc;
//
//// Variable
//int val = 0;
//float valtab[10];
//uint16_t au16data[16];
//uint8_t u8state;
//unsigned long timer;
//int a = 0;
//char filename[12];
//String valmode = "Visite";
//char Heure[16];
//char HeureSvg[16];
//
//
//
//unsigned long u32wait;
//
//
//void setup() {
//  u32wait = millis() + 100;
//  u8state = 0;
//
//  lcd.begin();
//  Serial.begin(9600);
//  Serial.print("Initialisation de la carte SD...");
//  if (!SD.begin(53)) {
//    Serial.println("L'initialisation de la carte SD a echoue !");
//    return;
//  }
//  Serial.println("L'initialisation de la carte SD a reussie !");
//
//  if (! rtc.begin()) {                                                            //Clock opening
//    Serial.println("Couldn't find RTC");
//    while (1);
//  }
//}
//
//
//
//
//void loop() {
//  switch( u8state ) {
//  case 0: {
//    if (millis() > timer){
//      timer = millis() + 3600000;
//      u8state = 2;
//      valmode = "Test";
//      break;
//    }
//    if (millis() > u32wait) u8state++; // wait state
//
//    DateTime now = rtc.now();
//    int heure = now.hour();
//    int minut = now.minute();
//    int seconde = now.second();
//    sprintf(Heure,"Heure: %02u-%02u-%02u",heure,minut,seconde);  //  %d pour un int // Write if median > 35 Kg
//
//
//    lcd.backlight();
//    lcd.setCursor(0,0);
//    lcd.print(Heure);
//    lcd.setCursor(1,1);
//    lcd.print(HeureSvg);
//
//    break;
//  }
//  case 1:
//    for (int i=0; i < 10; i++){
//      val = analogRead(3);
//      Serial.println(val);
//      valtab[i] = val;                                                              // fill in the list with ten values
//      delay(100);
//    }
//    Serial.print("Median: ");
//    Serial.println(stats.median(valtab,10));                                        //print median
//    Serial.print("Standard Error: ");
//    Serial.println(stats.stderror(valtab,10));         //print standard error
//
//    u8state++;
//    for (int i=0; i<10; i++){
//      if(valtab[i]<minWeight){
//        u8state=0;
//      }
//    }
//    break;
//
//  case 2:
//    {
//      DateTime now = rtc.now();
//      int jour = now.day();
//      int mois = now.month();
//      int annee = now.year();
//      sprintf(filename,"%04u%02u%02u.csv",annee,mois,jour);  //  %d pour un int // Write if median > 35 Kg
//
//      bool fileExisting = SD.exists(filename);
//      Serial.print("file is existing? ");
//      Serial.println(fileExisting);
//
//      monFichier = SD.open(filename, FILE_WRITE);                                   // create or open the file
//      if (monFichier) {  // if the file is open :
//
//          if (!fileExisting){
//            monFichier.println("Mode;Hour;Median;Standard_Error;BalanceID");
//            }
//          Serial.println(valmode);
//          Serial.print("Sauvegarde d'une pesée dans le fichier "); Serial.println(filename);
//          monFichier.print(valmode);monFichier.print(';');
//          monFichier.print(now.year(), DEC);monFichier.print('/');monFichier.print(now.month(), DEC);monFichier.print('/');// Date and Hour
//          monFichier.print(now.day(), DEC);monFichier.print(" ");monFichier.print(now.hour(), DEC);monFichier.print(':');
//          monFichier.print(now.minute(), DEC);monFichier.print(':');monFichier.print(now.second(), DEC);monFichier.print(";");
//          monFichier.print(stats.median(valtab,10));monFichier.print(";"); // Median
//          monFichier.print(stats.stderror(valtab,10));monFichier.print(";"); // Standard Error
//          monFichier.println(91); // BalanceID
//          monFichier.close();
//
//          DateTime now = rtc.now();
//          int heure = now.hour();
//          int minut = now.minute();
//          int seconde = now.second();
//          sprintf(HeureSvg,"Sauv: %02u-%02u-%02u",heure,minut,seconde);  //  %d pour un int // Write if median > 35 Kg
//
//        }
//        else{Serial.println("failed to open");}
//    valmode = "visite";
//    u8state = 0;
//    break;
//
//    }
//  }
//}
//
//
//
//#########################
//##########################
//##########################
//###########################
//############################
//#############################
//#include "RunningMedian.h"
//
//float resRef[]= {10000.0,10000.0,10000.0,10000.0,10000.0}; // Valeur de resistance des résitances de référence 10000 ohm 6 valeurs car potentielleme 6 sondes par cellules
//
//int state;
//const byte buffSize = 40;
//char inputBuffer[buffSize];
//const char startMarker = '<';
//const char endMarker = '>';
//byte bytesRecvd = 0;
//boolean readInProgress = false;
//boolean newDataFromPC = false;
//int newValue = 0;
//
//const byte interruptPins[4] = {2, 3, 4, 5};
//
//double waveLength[4] = {0, 0, 0, 0};
//double tStart[4] = {0, 0, 0, 0};
//
//RunningMedian psample[4] = RunningMedian(11);
//
//double t = 0.0;
//double p = 0.0;
//
//int it = 0;
//
//void setup() {
//  // put your setup code here, to run once:
//
//  state = 0;
//  pinMode(LED_BUILTIN, OUTPUT);
//  
//  attachInterrupt(interruptPins[0], freqMeasure0, RISING);
//  attachInterrupt(interruptPins[1], freqMeasure1, RISING);
//  attachInterrupt(interruptPins[2], freqMeasure2, RISING);
//  attachInterrupt(interruptPins[3], freqMeasure3, RISING);
//
//
//  Serial.begin(9600);
//
//  delay(200);
//
//  Serial.println("<Ready>");
//
//}
//
//void loop() {
//  // put your main code here, to run repeatedly:
//
//  getDataFromPC();
//
//  if (not readInProgress and newDataFromPC){
//  
//    state = newValue;
//    newDataFromPC = false;
//
//    if (state == 1) {
//      Serial.println("<Running>");
//    }
//    else if (state == 0) {
//      Serial.println("<Ready>");
//    }
//    else {
//      Serial.println("<Stopped>");
//    }
//  }
//
//
//  if (not readInProgress and state == 1 and it > 10) {
//
//    Serial.print("<state,"); Serial.print(state);
//    for (int i = 0; i < 5; i++){
//      
//      float t, p, resMeas;
//      double sum_t = 0;
//      double sum_p = 0;
//      int n = 10;
//      
//      for (int it = 0; it < n; it++){
//        delay(10);
//        resMeas = (float) (1024.0/ (float) analogRead(i)-1)*resRef[i]; 
//        sum_t += (1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log(resMeas)+8.758753*pow(10,-8)*pow(log(resMeas),3))-273.15);
//        }
//      t = sum_t/n;
//      Serial.print(","); Serial.print(t);  Serial.print(","); Serial.print(waveLength[i]); 
//    }
//    Serial.println(">");   
//    Serial.flush();
//    it = 0;
//    
//  }
//  delay(5000);
//  it++;
//
//}
//
////=============
//
//void getDataFromPC() {
//
//    // receive data from PC and save it into inputBuffer
//    
//  if(Serial.available() > 0) {
//
//    char x = Serial.read();
//
//      // the order of these IF clauses is significant
//      
//    if (x == endMarker) {
//      readInProgress = false;
//      newDataFromPC = true;
//      inputBuffer[bytesRecvd] = 0;
//      parseData();
//    }
//    
//    if(readInProgress) {
//      inputBuffer[bytesRecvd] = x;
//      bytesRecvd ++;
//      if (bytesRecvd == buffSize) {
//        bytesRecvd = buffSize - 1;
//      }
//    }
//
//    if (x == startMarker) { 
//      bytesRecvd = 0; 
//      readInProgress = true;
//    }
//  }
//}
//
////=============
// 
//
//void freqMeasure0() {freqMeasure(0);}
//void freqMeasure1() {freqMeasure(1);}
//void freqMeasure2() {freqMeasure(2);}
//void freqMeasure3() {freqMeasure(3);}
//
//
//void freqMeasure(int i){
//    
//  psample[i].add(micros()-tStart[i]);
//  waveLength[i] = psample[i].getAverage();
//  tStart[i] = micros(); 
//}
// 
//void parseData() {
//
//  newValue = atoi(inputBuffer);     // convert to an integer
//
//}
//
//
///////////////////////
//////////////////////
//////////////////////
//
//
////MesureTemperature Dalle de stockage
//
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
///*-----( Import needed libraries )-----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
////-------- Popur general
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
///*-----( Declare Constants )-----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
///*-----( Declare objects )-----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
///*-----( Declare Variables )-----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//
//
////-------- Lectures analogiques
//int pinsAnalogicRead[5] = {0,1,2,3,4};
//int NAnalogicRead = (int) sizeof(pinsAnalogicRead)/ sizeof(pinsAnalogicRead[0]);
//
////-------- Relais
//const int TableRelaisDispo[5][8]={{2,3,4,5,6,7,8,9},{10,11,12,13,22,23,24,25},{26,27,28,29,30,31,32,33},{34,35,36,37,38,39,40,41},{42,43,44,45,46,47,48,49}};
//const int NRelais = (int) sizeof(TableRelaisDispo)/ sizeof(TableRelaisDispo[0]);
//const int NSondeTot = (int) sizeof(TableRelaisDispo)/sizeof(TableRelaisDispo[0][0]);
//const int NDalle = (int) sizeof(TableRelaisDispo[0])/sizeof(TableRelaisDispo[0][0]);
//
////-------- Temperature
//
//float resRef[]= {10000.0,10000.0,10000.0,10000.0,10000.0,10000.0}; // Valeur de resistance des résitances de référence 10000 ohm 6 valeurs car potentielleme 6 sondes par cellules
//int delai_cycle = 30000;
//
////-------- Communication
//
//int state;
//const byte buffSize = 40;
//char inputBuffer[buffSize];
//const char startMarker = '<';
//const char endMarker = '>';
//byte bytesRecvd = 0;
//boolean readInProgress = false;
//boolean newDataFromPC = false;
//int newValue = 0;
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//void setup()   /*----( SETUP: RUNS ONCE )----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//{
//
//  // Check
//  Serial.print("NRelais");
//  Serial.println(NRelais);
//  Serial.print("NDalle");
//  Serial.println(NDalle);
//  Serial.print("NSondeTot");
//  Serial.println(NSondeTot);
//  Serial.print("NAnalogicRead");
//  Serial.println(NAnalogicRead);
//
//  //-------- Relais 
//  for (int it_relais=0; it_relais < NRelais; it_relais++){
//    for (int it_dalle=0; it_dalle < NDalle; it_dalle++){   
//      pinMode(TableRelaisDispo[it_relais][it_dalle],OUTPUT);
//      digitalWrite(TableRelaisDispo[it_relais][it_dalle],HIGH);
//    }
//  }
//
//  Serial.begin(9600);  // Used to type in characters
//  delay(30000);
//
//}//--(fin setup )---*/
//
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//void loop()   /*----( LOOP: RUNS CONSTANTLY )----*/
//// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//{
//
//  /*  
//  getDataFromPC();
//  if (not readInProgress and newDataFromPC){
//  
//    state = newValue;
//    newDataFromPC = false;
//    if (state == 1) {
//      Serial.println("<Running>");
//    }
//    else if (state == 0) {
//      Serial.println("<Ready>");
//    }
//    else {
//      Serial.println("<Stopped>");
//    }
//  }
//  */
//
//  state = 1;
//  
//  if (not readInProgress and state == 1) {
//    // loop over the NDalles
//    for (int it_dalle = 0; it_dalle < NDalle; it_dalle++){
//      for (int it_relais = 0; it_relais < NRelais; it_relais++){
//        digitalWrite(TableRelaisDispo[it_relais][it_dalle],LOW);
//      }
//      delay(1000);
//  
//      float resMeas;
//      int values [10];
//      
//      for (int it_relais = 0; it_relais < NRelais; it_relais++){
//      
//        for (int it = 0; it < 10; it++){
//          delay(50);
//          resMeas = (float) (1024.0/ (float) analogRead(pinsAnalogicRead[it_relais])-1)*resRef[it_relais]; 
//          values[it] = (1/(1.129336*pow(10,-3)+2.341350*pow(10,-4)*log(resMeas)+8.758753*pow(10,-8)*pow(log(resMeas),3))-273.15);
//        }
//  
//        float sum = 0.0, mean, standardDeviation = 0.0;
//  
//        int i;
//        for(i = 0; i < 10; ++i)
//        {
//          sum += values[i];
//        }
//  
//        mean = sum/10;
//   
//        for(i = 0; i < 10; ++i)
//          standardDeviation += pow(values[i] - mean, 2);
//  
//        standardDeviation = sqrt(standardDeviation / 10);
//        
//        delay(500);
//        Serial.print("<");
//        Serial.print(millis());
//        Serial.print(", ");
//        Serial.print(it_dalle);
//        Serial.print(", ");
//        Serial.print(it_relais);
//        Serial.print(", ");
//        Serial.print(mean);
//        Serial.print(", ");
//        Serial.print(standardDeviation);
//        Serial.println(">");
//      }
//    
//      for (int it_relais = 0; it_relais < NRelais; it_relais++){
//        digitalWrite(TableRelaisDispo[it_relais][it_dalle],HIGH);
//      }
//    }
//    delay(delai_cycle);
//
//  }
//
//}/* --(end main loop )-- */
//
//
//void getDataFromPC() {
//
//    // receive data from PC and save it into inputBuffer
//    
//  if(Serial.available() > 0) {
//
//    char x = Serial.read();
//
//      // the order of these IF clauses is significant
//      
//    if (x == endMarker) {
//      readInProgress = false;
//      newDataFromPC = true;
//      inputBuffer[bytesRecvd] = 0;
//      parseData();
//    }
//    
//    if(readInProgress) {
//      inputBuffer[bytesRecvd] = x;
//      bytesRecvd ++;
//      if (bytesRecvd == buffSize) {
//        bytesRecvd = buffSize - 1;
//      }
//    }
//
//    if (x == startMarker) { 
//      bytesRecvd = 0; 
//      readInProgress = true;
//    }
//  }
//}
//
////=============
// 
//void parseData() {
//
//  newValue = atoi(inputBuffer);     // convert to an integer
//
//}
//
///* ( THE END ) */


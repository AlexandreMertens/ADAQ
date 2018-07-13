cd C:\phpresource
start php -S localhost:80

set /a compteur=24
set /a ouverture=0



Rem Chemin vers python.exe
set cheminpy=C:\Users\l.degreve\AppData\Local\Continuum\anaconda3 

Rem Chemin vers votre navigateur
set cheminnav=C:\Users\l.degreve\AppData\Local\Mozilla Firefox\firefox.exe




:loop
set /a heure = (24-%compteur%)
echo Il reste %heure%h avant la sauvgarde des donnees
@ECHO off 
if %compteur% == 24 (
	cd %cheminpy%
	start python getGFdatabyD.py
	set /a compteur=0
	if %ouverture% == 0 (
		timeout /t 300
		"%cheminnav%" http://localhost/CRA-W/
		)
	set /a ouverture=1
	timeout /t 3600
	goto loop 
) else  (
	cd %cheminpy%
	start python getGFdatabyH.py
	set /a compteur+=1
	timeout /t 3600
	goto loop
)



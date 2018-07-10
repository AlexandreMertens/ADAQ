# This code :
#   -connects to the greenfeed website, download the summary data, the greenfeed's status data
# and the table of the beasts data
#   -save a csv file with the interesting data a other with the beasts's table
# and a last one with the data of the greenfeed .
#   -save in a compress file, the untreated data
# Authors : Luc Degreve, Alexandre Mertens

from robobrowser import RoboBrowser  # website browsing
import xlrd  # Excel file reading
from xlwt import Workbook  # Excel file writing
import pandas as pd  # Csv file writing
from datetime import date  # Date
from datetime import timedelta, datetime
import os  # To delete a file

Greenfeed_90=True
Greenfeed_91=False

GFSummary = False
modifySum = True

colData = False
getRealTData = True
getDateCali = True
getDateRec = True
getStatus = True

tableau =True

convertToCsv = True
deleteXls = True

getUntreatedData = False

def GFLogin(login, password):
    base_url = "https://greenfeed.c-lockinc.com/GreenFeed/home.php"

    # Access the GreenFeed websiteand fill the login form
    browser.open(base_url)
    form = browser.get_form(action="checklogon.php")
    form["username"] = login  # to be completed!
    form["password"] = password  # to be completed!
    browser.session.headers['Referer'] = base_url
    browser.submit_form(form, submit='Login')

    return ()


def getGFSummary(url_sum):
    # create the request to access the xls file
    request = browser.session.get(url_sum, stream=True)

    with open(xls_sum_file, "wb") as test:
        test.write(request.content)

    return ()


def selectData(xls_sum_file):
    # xls file opening and sheet selection
    document = xlrd.open_workbook(xls_sum_file)
    data = document.sheet_by_index(0)

    # Getting the number of lines and columns
    nbrow = data.nrows
    nbcol = data.ncols

    # Creation of the new file
    book = Workbook()
    sheet1 = book.add_sheet('view_data')

    for k in range(0, nbcol):
        for i in range(0, nbrow - 1):
            sheet1.write(i, k, data.cell_value(rowx=(i + 1), colx=k))

    return book


def excelToCsv(xls_sum_file, name_sum_file, withdateYorN='N'):
    # convert the excel file to a CSV file
    today = str(date.today())
    df = pd.read_excel(xls_sum_file)
    if withdateYorN == 'Y':
        df.to_csv(name_sum_file + '_' + today, index=False)
    else :
        df.to_csv(name_sum_file , index=False)


def colTimeData(valeur,GF):
    # get the realtime data of the greenfeed
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getfeederstatistics.php?fid="+GF)

    tests = str(browser.parsed)
    indval = []
    info = []

    for k in valeur:
        ss = (k + '</sensor')
        start = (str.find(tests, ss) - 2)
        if (str.find(tests, ss) - 2) < (str.find(tests, "<sensor10>")):
            indval.append(int(tests[start]))
        else:
            indval.append(int(tests[start - 1:start + 1]))

    for k in indval:
        ss = '<s' + str(k) + '>'
        se = '</s' + str(k) + '>'
        start = (str.find(tests, ss)) + (len(str(k)) + 3)
        end = (str.find(tests, se))
        info.append(tests[start:end])
    return info


def dateCali(GF):
    # get the date of the last calibration
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getstandardcalibrations.php?fid="+GF+"")

    calibration = str(browser.parsed)
    inddatcal = str.find(calibration, "<dt>")
    return calibration[inddatcal + 4:inddatcal + 14]


def dateRec(GF):
    # get the date of the last recovery test
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getco2recoveries.php?fid="+GF+"")

    calibrationco2 = str(browser.parsed)
    inddatco2 = str.find(calibrationco2, "<st>")
    return calibrationco2[inddatco2 + 4:inddatco2 + 14]


def Status(GF):
    # get the status of the greenfeed
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getfeederinfo.php?fid="+GF+"")

    feederinfo = str(browser.parsed)
    indfeedstart = str.find(feederinfo, "<status>")
    indfeedend = str.find(feederinfo, "</status>")
    return feederinfo[indfeedstart + 8:indfeedend]


def Recherche(cont_page, val, lent=None, valend=None):
    # get the characters after a define string or between 2 define string
    indini = 0
    listeind = []
    while indini != (-1):
        indtableau = str.find(cont_page, val, indini + 1)
        listeind.append(indtableau)
        indini = indtableau
    listeind.remove(-1)
    lenlisind = len(listeind)
    listval = []
    listeindend = []
    if lent != None:
        for k in range(0, lenlisind):
            listval.append(cont_page[listeind[k] + len(val):listeind[k] + len(val) + lent])
    else:
        indini = 0
        while indini != (-1):
            indtableauend = str.find(cont_page, valend, indini + 1)
            listeindend.append(indtableauend)
            indini = indtableauend
        listeindend.remove(-1)
        for y in range(0, lenlisind):
            listval.append(cont_page[listeind[y] + len(val):listeindend[y]])
    return listval


def permute(liste, k):
    # permute 2 valeurs in a list
    liste[k], liste[k + 1] = liste[k + 1], liste[k]
    return


def getTableau(GF):
    # get data for the
    #  table
    temps = timedelta(days=7)
    temps2 = timedelta(days=30)
    arrive = datetime.today()
    depart = arrive - temps
    depart2 = arrive - temps2
    departt = str(depart)
    arrivet = str(arrive)
    departt2 = str(depart2)
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/tabledata/cowfeeding.php?fids=0,"+GF+"&from=" + departt[
                                                                                                        0:10] + "&to=" + arrivet[
                                                                                                                         0:10] + "&cons=0&uncons=0&param=1")

    tableaubrute = str(browser.parsed)

    listtag = Recherche(tableaubrute, 'tag="', 24)
    for o in range(len(listtag)):
        listtag[o] = listtag[o].lstrip('0')
    listnom = Recherche(tableaubrute, 'name="', None, '" tag')
    listdate = Recherche(tableaubrute, "<date>", 5)
    listdrop = Recherche(tableaubrute, "<v>", None, '</v>')
    listdrop = list(zip(*[iter(listdrop)] * len(listdate)))

    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/tabledata/cowfeeding.php?fids=0,"+GF+"&from=" + departt2[
                                                                                                        0:10] + "&to=" + arrivet[
                                                                                                                         0:10] + "&cons=0&uncons=0&param=1")

    tableaubrute2 = str(browser.parsed)
    print(tableaubrute)
    listdate2 = Recherche(tableaubrute2, "<date>", 5)
    listdrop2 = Recherche(tableaubrute2, "<v>", None, '</v>')
    listdrop2 = list(zip(*[iter(listdrop2)] * len(listdate2)))
    listnom2 = Recherche(tableaubrute2, 'name="', None, '" tag')
    ind = []
    for a in listnom:
        ind.append(listnom2.index(a))
    somind = []
    for e in range(len(listnom2)):
        som = 0
        for r in range(len(listdate2)):
            som += int(listdrop2[e][r])
        somind.append(som)
    bonval = []
    for y in ind:
        bonval.append(somind[y])
    # ----------------------------------------
    # sort the table
    passage = 0
    while bonval != sorted(bonval):
        passage += 1
        for u in range(len(bonval) - 1):
            if bonval[u] > bonval[u + 1]:
                permute(bonval, u)
                permute(listtag, u)
                permute(listnom, u)
                permute(listdrop, u)
    # ----------------------------------------
    # write in a excel file
    book3 = Workbook()
    sheet1 = book3.add_sheet("Tableau")
    sheet1.write(0, 0, "Animal Name")
    sheet1.write(0, 1, "Animal Tag")
    sheet1.write(0, len(listdate) + 2, "Total 30j")
    for k in range(1, len(listnom) + 1):
        sheet1.write(k, 1, listtag[k - 1])
        sheet1.write(k, 0, listnom[k - 1])
        sheet1.write(k, len(listdate) + 2, bonval[k - 1])
    for i in range(2, len(listdate) + 2):
        sheet1.write(0, i, listdate[i - 2])
    for l in range(len(listdrop)):
        for z in range(len(listdate)):
            sheet1.write(l + 1, z + 2, listdrop[l][z])

    return book3


def exelColData(GF):
    # Creation of the new file
    book2 = Workbook()
    sheet1 = book2.add_sheet('Col_Data')
    valCol = []
    nomCol = []
    couCol = []
    today = str(date.today())


    if getRealTData:
        for k in (colTimeData(valeur,GF)):
            valCol.append(k)
        for l in valeur:
            nomCol.append(l)
    if getDateCali:
        valCol.append(dateCali(GF))
        nomCol.append("Dernière calibration")
    if getDateRec:
        valCol.append(dateRec(GF))
        nomCol.append("Dernière recovery")
    var1 = len(valCol)
    nomCol.append('Date')
    valCol.append(today)
    if getStatus:
        valCol.append(Status(GF))
        nomCol.append("Statut")
    lenVal = len(valCol)

#couleur pour les limites de valeurs


    if float(valCol[0][0:len(valCol[0])-4]) < 26:
        couCol.append("red")
    else:
        couCol.append("black")

    if float(valCol[1][0:len(valCol[0])-3])<-30 or float(valCol[1][0:len(valCol[0])-3])>50:
        couCol.append("red")
    else:
        couCol.append("black")

    if float(valCol[0][0:len(valCol[0])-4])<30:
        couCol.append("red")
    else:
        couCol.append("black")

    aujd = datetime.today()
    dc=datetime.strptime(dateCali(GF), '%Y-%m-%d')
    lastCali=int(str(aujd-dc)[0:2])

    if lastCali>14:
        couCol.append("red")
    elif lastCali>7 and lastCali<15:
        couCol.append("orange")
    else:
        couCol.append("black")

    dr = datetime.strptime(dateRec(GF), '%Y-%m-%d')
    lastReco = int(str(aujd - dr)[0:2])

    if lastReco > 30:
        couCol.append("red")
    elif lastReco > 21 and lastCali < 31:
        couCol.append("orange")
    else:
        couCol.append("black")

    for i in range(0, lenVal):
        sheet1.write(i, 0, nomCol[i])
        sheet1.write(i, 1, valCol[i])
    for k in range(0,var1):
        sheet1.write(k, 2, couCol[k])
    return book2


def untreatedData():
    # get the untreated data
    temps3 = timedelta(days=2)
    arrive = datetime.today()
    depart3 = arrive - temps3
    depart3str = str(depart3)
    request = browser.session.get(
        'https://greenfeed.c-lockinc.com/GreenFeed/downloaddata/downloaddailyfiles.php?dl=11111&fl=,' + depart3str[
                                                                                                        0:4] + depart3str[
                                                                                                               5:7] + depart3str[
                                                                                                                      8:10] + ', stream=True')
    with open('untreated_data ' + depart3str[0:10] + '.zip', "wb") as test:
        test.write(request.content)
    return ()


# ======================================================================================================================#

# get GF summary, transform to csv and save in the csv

url_sum = "https://greenfeed.c-lockinc.com/GreenFeed/downloaddata/downloaddailyfiles.php?file=GreenFeed_Summarized_Data_Belgium_90_91.xlsm"
xls_sum_file = "GF_Summary.xls"
# name_sum_file = "../csv/GreenFeed/GF_Summary"
name_sum_file = "GF_Summary"
valeur = ("Air Flow", "Temperature", "Humidity")

browser = RoboBrowser()
try:
    GFLogin('CRAW', 'greenfeed')
except:
    print("Erreur : La connexion au site du greenfeed est impossible !")
    SystemExit(0)

if GFSummary:
    try:
        getGFSummary(url_sum)
    except:
        print("Erreur : la récupération du résumé est impossible !")

if modifySum and GFSummary:
    try:
        xls_sum_file = selectData(xls_sum_file)
        xls_sum_file.save('GF_Summary.xls')
    except:
        print("Erreur : la modification du résumé est impossible !")

if convertToCsv and GFSummary:
    try:
        excelToCsv("GF_Summary.xls", name_sum_file,'Y')
    except:
        print("Erreur : la convertion du résumé en CSV est impossible !")

if deleteXls and GFSummary:
    try:
        os.remove('GF_Summary.xls')
    except:
        print("Erreur : la supression du document excel est impossible !")

if getUntreatedData:
    try:
        untreatedData()
    except:
        print("Erreur : la sauvegarde des données brutes est impossible !")

# 91
GF=str(91)
if colData and Greenfeed_91:
#    try:
        xls_file2 = exelColData(GF)
        xls_file2.save('Greenfeed_'+GF+'/Col_Data_'+GF+'.xls')
 #   except:
  #      print("Erreur : la récupération des données d'état du greenfeed est impossible !")

if convertToCsv and colData and Greenfeed_91:
    try:
        excelToCsv("Greenfeed_"+GF+"/Col_Data_"+GF+".xls", "Greenfeed_"+GF+"/colData_"+GF,'N')
    except:
        print("Erreur : la convertion des données d'état du greenfeed en CSV est impossible !")

if deleteXls and colData and Greenfeed_91:
    try:
        os.remove('Greenfeed_'+GF+'/Col_Data_'+GF+'.xls')
    except:
        print("Erreur : la supression du document excel est impossible !")

if tableau and Greenfeed_91:
#    try:
        xls_file3 = getTableau(GF)
        xls_file3.save('Greenfeed_'+GF+'/tableau_'+GF+'.xls')
#    except:
 #       print("Erreur : la récupération du tableau des animaux est impossible !")

if convertToCsv and tableau and Greenfeed_91:
    try:
        excelToCsv('Greenfeed_'+GF+'/tableau_'+GF+'.xls', "Greenfeed_"+GF+"/Tableau_"+GF, 'N')
    except:
        print("Erreur : la convertion du tableau des animaux en CSV est impossible !")

if deleteXls and tableau and Greenfeed_91:
    try:
        os.remove('Greenfeed_'+GF+'/tableau_'+GF+'.xls')
    except:
        print("Erreur : la supression du document excel est impossible !")


#90
GF=str(90)
if colData and Greenfeed_90:
    try:
        xls_file2 = exelColData(GF)
        xls_file2.save('Greenfeed_'+GF+'/Col_Data_'+GF+'.xls')
    except:
        print("Erreur : la récupération des données d'état du greenfeed est impossible !")

if convertToCsv and colData and Greenfeed_90:
    try:
        excelToCsv("Greenfeed_"+GF+"/Col_Data_"+GF+".xls", "Greenfeed_"+GF+"/colData_"+GF,'N')
    except:
        print("Erreur : la convertion des données d'état du greenfeed en CSV est impossible !")

if deleteXls and colData and Greenfeed_90:
    try:
        os.remove('Greenfeed_'+GF+'/Col_Data_'+GF+'.xls')
    except:
        print("Erreur : la supression du document excel est impossible !")

if tableau and Greenfeed_90:
    try:
        xls_file3 = getTableau(GF)
        xls_file3.save('Greenfeed_'+GF+'/tableau_'+GF+'.xls')
    except:
        print("Erreur : la récupération du tableau des animaux est impossible !")

if convertToCsv and tableau and Greenfeed_90:
    try:
        excelToCsv('Greenfeed_'+GF+'/tableau_'+GF+'.xls', "Greenfeed_"+GF+"/Tableau_"+GF, 'N')
    except:
        print("Erreur : la convertion du tableau des animaux en CSV est impossible !")

if deleteXls and tableau and Greenfeed_90:
    try:
        os.remove('Greenfeed_'+GF+'/tableau_'+GF+'.xls')
    except:
        print("Erreur : la supression du document excel est impossible !")


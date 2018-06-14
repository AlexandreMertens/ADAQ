# This code :
#   -connects to the greenfeed website, download the summary data
#   -save a csv file with the interesting data.
# Authors : Luc Degreve, Alexandre Mertens

from robobrowser import RoboBrowser     # website browsing
import xlrd                             # Excel file reading
from xlwt import Workbook               # Excel file writing
import pandas as pd                     # Csv file writing
from datetime import date               # Date
import os                               # To delete a file

downloadData = True
modifyData = True
convertToCsv = True
deleteXls = True
getRealTData = True
getDateCali = True
getDateRec = True
getStatus = True
colData = True

def GFLogin(login,password):
    base_url = "https://greenfeed.c-lockinc.com/GreenFeed/home.php"

    # definition of the browser


    # Access the GreenFeed websiteand fill the login form
    browser.open(base_url)
    form = browser.get_form(action="checklogon.php")
    form["username"] = login  # to be completed!
    form["password"] = password  # to be completed!
    browser.session.headers['Referer'] = base_url
    browser.submit_form(form, submit='Login')

    return()

def getGFSummary(base_url_xls):
    # create the request to access the xls file
    request = browser.session.get(base_url_xls, stream=True)

    with open(xls_file, "wb") as test:
        test.write(request.content)

    return()


def selectData(xls_file):

    # xls file opening and sheet selection
    document = xlrd.open_workbook(xls_file)
    data = document.sheet_by_index(0)

    # Getting the number of lines and columns
    nbrow = data.nrows
    nbcol = data.ncols

    # Creation of the new file
    book = Workbook()
    sheet1 = book.add_sheet('view_data')

    for k in range(0, nbcol):
        for i in range(0, nbrow-1):
            sheet1.write(i, k, data.cell_value(rowx=(i+1), colx=k))

    return(book)

def excelToCsv(xls_file,exitFile):
    today = str(date.today())
    df = pd.read_excel(xls_file)
    df.to_csv(exitFile + '_' + today, index=False)

def realTimeData(valeur):
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getfeederstatistics.php?fid=91")

    tests = str((browser.parsed))
    indval = []
    info = []

    for k in valeur:
        ss = (k + '</sensor')
        start = (str.find(tests, ss) - 2)
        if ((str.find(tests, ss) - 2) < (str.find(tests, "<sensor10>"))):
            indval.append(int(tests[start]))
        else:
            indval.append(int(tests[start - 1:start + 1]))

    for k in indval:
        ss = '<s' + str(k) + '>'
        se = '</s' + str(k) + '>'
        start = (str.find(tests, ss)) + (len(str(k)) + 3)
        end = (str.find(tests, se))
        info.append(tests[start:end])
    return(info)

def dateCali():
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getstandardcalibrations.php?fid=91")

    calibration = str(browser.parsed)
    inddatcal = str.find(calibration, "<dt>")
    return(calibration[inddatcal + 4:inddatcal + 14])

def dateRec():
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getco2recoveries.php?fid=91")

    calibrationco2 = str(browser.parsed)
    inddatco2 = str.find(calibrationco2, "<st>")
    return(calibrationco2[inddatco2 + 4:inddatco2 + 14])

def Status():
    browser.open("https://greenfeed.c-lockinc.com/GreenFeed/ajax/getfeederinfo.php?fid=91")

    feederinfo = str(browser.parsed)
    indfeedstart = str.find(feederinfo, "<status>")
    indfeedend = str.find(feederinfo,"</status>")
    return(feederinfo[indfeedstart+8:indfeedend])

def exelColData():
    # Creation of the new file
    book2 = Workbook()
    sheet1 = book2.add_sheet('Col_Data')
    valCol = []
    if getRealTData:
        for k in (realTimeData(valeur)):
            valCol.append(k)
    if getDateCali:
        valCol.append(dateCali())
    if getDateRec:
        valCol.append(dateRec())
    if getStatus:
        valCol.append(Status())
    lenVal=len(valCol)

    for i in range(0, lenVal):
        sheet1.write(i, 0,valCol[i] )

    return(book2)

#======================================================================================================================#

# get GF summary, transform to csv and save in the csv

base_url_xls = "https://greenfeed.c-lockinc.com/GreenFeed/downloaddata/downloaddailyfiles.php?file=GreenFeed_Summarized_Data_Belgium_90_91.xlsm"
xls_file = "GF_Summary.xls"
#exitFile = "../csv/GreenFeed/GF_Summary"
exitFile = "GF_Summary"
valeur = ("Air Flow", "Temperature", "Humidity")

browser = RoboBrowser()
GFLogin('CRAW','greenfeed')

if downloadData:
    getGFSummary(base_url_xls)

if modifyData and downloadData:
    xls_file = selectData(xls_file)
    xls_file.save('GF_Summary.xls')
	
if convertToCsv and downloadData:
	excelToCsv("GF_Summary.xls",exitFile)

if deleteXls and downloadData:
    os.remove('GF_Summary.xls')

if colData:
    xls_file2 = exelColData()
    xls_file2.save('Col_Data.xls')

if convertToCsv and colData:
	excelToCsv("Col_Data.xls","colData")

if deleteXls and colData:
    os.remove('Col_Data.xls')
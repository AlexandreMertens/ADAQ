
from robobrowser import RoboBrowser  # website browsing
from datetime import timedelta, datetime, date

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

def untreatedData(jour, fin):
    # get the untreated data
    temps = timedelta(days=(jour))
    arrive = fin
    depart = arrive - temps
    departstr = str(depart)
    print(departstr[0:10]+" en cours")
    request = browser.session.get(
        'https://greenfeed.c-lockinc.com/GreenFeed/downloaddata/downloaddailyfiles.php?dl=11111&fl=,' + departstr[
                                                                                                        0:4] + departstr[
                                                                                                               5:7] + departstr[
                                                                                                                      8:10] + ', stream=True')
    with open('untreated_data ' + departstr[0:10] + '.zip', "wb") as test:
        test.write(request.content)
    return ()

#======================================================================================================================#

debut = date(2016,8,17)
fin = date(2016,8,18)

browser = RoboBrowser()
GFLogin("craw","greenfeed")
i = str(fin - debut)
j = int(i[0:len(i)-13])
for k in range (0,j+1):
    untreatedData(k,fin)
print ("téléchargement terminé")
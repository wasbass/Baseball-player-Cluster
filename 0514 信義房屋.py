import urllib.request as req
from bs4 import BeautifulSoup
import webbrowser
import urllib
import csv
import numpy as np
import pandas as pd
import os
import time
import csv
import re
os.chdir("C:\Python")
h = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.122 Safari/537.36"

u_home = "https://www.sinyi.com.tw/rent/list/"  #1.html

List_title = list()
List_url = list()

start_page = 1 #開始頁數
page_count = 158   #抓幾頁
page = start_page
house_url_start = "https://www.sinyi.com.tw/rent/"

colnames = ["title","url","rent","deposit","area","pattern","typeof","floor","community",
                          "address","construction","age","neighbor","addroof","direct","fee","guard","parking","cooking","pet","limit",
                          "sofa","TV","laundry","net","bed","refre","hotwater","channel","cabinet","AC","gas",
                          "content","school","market","supermarket","bank","cvs","park","departmentstore","hospital","mrt","bus"]

###抓標題和文章網址###
for i in range(1,page_count+1):
    print("-",end="")
    if i%20 ==0 :
        print(i)
    
    u = u_home + str(page) + ".html"
    #print(u)
    res = req.Request(u, headers = {"User-Agent":h})
    
    with req.urlopen(res) as response:
        data = response.read().decode('utf-8')

    soup = BeautifulSoup(data , "html.parser")
    #print(soup)

    houses = soup.find_all("div",class_ ="ddhouse")

    for house in houses:
        title = house.find("span" ,class_="item_title").get("alt")
        List_title.append(title)
        house_url = house_url_start + house.a.get("href")
        List_url.append(house_url)
    page = page + 1

print("頁數抓完了\n")

'''
dict1 = {"title": List_title, "url" :List_url}
D = pd.DataFrame(dict1,columns=colnames)
print(D)
'''

with open("./Xinyi0515.csv", "w+", newline='', encoding='utf-8-sig') as file:
    writer = csv.writer(file ,delimiter=',')
    writer.writerow(colnames)
    file.close()
    
        
###抓內文###


for n in range(0,len(List_url)):
    try:
        print("*",end = "")
        if (n+1)%20 ==0:
            print(n+1)

        L = [List_title[n],List_url[n]]
        u_a = List_url[n]        
        
        res = req.Request(u_a, headers = {"User-Agent":h})
        with req.urlopen(res) as response:
            data = response.read().decode('utf-8')
        
        soup = BeautifulSoup(data , "html.parser")
        rent = soup.find("h2",class_ = "price-r").string
        L.append(rent)
        

        cutting = soup.find_all("ul",class_ = "cutting-inline detail-list")
        district = soup.find_all("ul" , class_ = "detail-list")[2]
        
        cut1    = cutting[0].find_all("li")
        cut2    = cutting[1].find_all("li")
        cut3    = district.find_all("li")
        
        deposit = cut1[0].string
        area    = cut1[1].string
        pattern = cut1[2].string
        L.extend([deposit,area,pattern])

        typeof  = cut2[0].string
        floor   = cut2[1].string
        L.extend([typeof,floor])

        community = cut3[0].string
        address   = cut3[1].string
        L.extend([community,address])

        inform  = soup.find_all("ul",class_ = "information-content")
        i0 = inform[0].find_all("li")
        i1 = inform[1].find_all("li")

        construction = i0[5].p.string
        age          = i0[6].p.string
        neighbor     = i0[8].p.string

        if str(age).count("--")>0:
            age = "-"
        L.extend([construction , age , neighbor])

        addroof      = i1[2].p.string
        direct       = i1[3].p.string
        fee          = i1[4].p.string
        guard        = i1[5].p.string
        parking      = i1[6].p.string
        cooking      = i1[7].p.string        
        pet          = i1[8].p.string
        limit        = i1[9].p.string

        cooking      = str(cooking).count("不")
        pet          = str(pet).count("不")
        
        L.extend([addroof,direct,fee,guard,parking,cooking,pet,limit])

        ###家具區###
        F = soup.find_all("ul",class_ = "furniture")
        F_list = list()
        for i in range(0,3):
            for j in  range(1,5):
                f = F[i].find_all("input")[j] 
                F_list.append(str(f).count("checked"))
        L.extend(F_list[0:11])


        con = soup.find_all("div",class_ = "width-content")[1]
        content = con.text
        content = content.split("特色說明")
        L.append(content)


        ###交通區###

        remote   = soup.find("div" , class_ = "environment-title text-remote-control").text
        school   = remote.count("學校")
        market   = remote.count("市場")
        sm       = remote.count("超市")
        bank     = remote.count("銀行")
        cvs      = remote.count("便利")
        park     = remote.count("公園")
        ds       = remote.count("百貨")
        hospital = remote.count("醫院")        
        L.extend([school,market,sm,bank,cvs,park,ds,hospital])

        traffic  = soup.find("table",class_="surroundings text-remote-control")
        mrt   = traffic.find_all("td",class_="form-content-White")[0].text
        bus   = traffic.find_all("td",class_="form-content-White")[1].text
        
        if mrt.count("--")==1:
            mrt = 0
        else:
            mrt = 1        
        if bus.count("--") == 0:
            bus = 0
        else:
            bus = 1
        L.extend([mrt,bus])


        with open("./Xinyi0515.csv", "a", newline='', encoding='utf-8-sig') as file:
            writer = csv.writer(file ,delimiter=',')
            writer.writerow(L)
            file.close()
        
    except:
        continue




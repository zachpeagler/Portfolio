import requests
from bs4 import BeautifulSoup
from datetime import datetime
import pandas
import time
import re
import random
import os
from fake_useragent import UserAgent

## URL (Go A-Z)
url= "https://www.localharvest.org/organic-farms/list?l=Z"
base_url = "http://localharvest.org"

## Set starting point along list (useful if script stopped midway)
start_pt = int(1)

## Randomize useragent headers
ua = UserAgent()

headers = {
    "User-Agent": ua.random
}

## The name of the csv file to write data to. This is important to update each time this program is run.
# This includes when the script stops midway and has to be restarted
filename = 'LH_Z_info.csv'

## Send HTTP GET request to the website
response = requests.get(url, headers=headers)

## Extract the list of links from the link page
List_soup = BeautifulSoup(response.content, 'html.parser')
List_soup_skimmed = List_soup.find("div", id = "content").find('ul').find_next('ul')
link_list=list()
for link in List_soup_skimmed('a'):
    link_list.append(link.get('href'))

## Time it
now = datetime.now()
print(now)
counter = int()
counter_max = int(len(link_list))
counter_pct = float((counter/counter_max) * 100)
counter = start_pt - 1

## Matrix for later
farmers = []

## Define link logic to be used with soup.find_all(href=__)
def link_unique(href):
    return href and not re.compile("google").search(href) and not re.compile("localharvest").search(href) and not re.compile("pinterest").search(href) and not re.compile("twitter").search(href) and not re.compile("instagram").search(href) and not re.compile("facebook").search(href) and not re.compile("postemail").search(href)

def twit_link(href):
    return href and re.compile("twitter").search(href)

def insta_link(href):
    return href and re.compile("instagram").search(href)

def fb_link(href):
    return href and re.compile("facebook").search(href)

def pin_link(href):
    return href and re.compile("pinterest").search(href)

def postemail_link(href):
    return href and re.compile("postemail").search(href)

## SETUP TEMP URLS
test_list = list()
placeholder_list = list()

## Set list range (useful if stopped halfway through)
for x in range (int(start_pt), int(counter_max)):
    test_list.append(base_url + link_list[x])    

## Begin looping through assembled URLs
for x in test_list:

    ## Empty the matrix
    farmers=[]

    ## Tick the counter
    counter = counter + 1
    counter_pct = float((counter/counter_max) * 100)

    headers = {
    "User-Agent": ua.random
    }

    ## Send HTTP GET request to website
    response = requests.get(x, headers=headers)

    ## Parse the HTML code with BeautifulSoup
    soup = BeautifulSoup(response.content, 'html.parser')

    ## Skim the soup to only have div with class "panel center" (in which the contact info is nestled)
    soup_content = soup.find("div", id = "content")
    soup_panelcenter = soup.find("div", class_="panel center")
    soup_additionalinfo = soup.find("div", class_ = "right sidebar").find("div", class_ = "panel center").find_next("div", class_ = "panel center").find_next("div", class_ = "panel center")
    soup_fullwidth = soup.find("div", class_ = "fullwidth")
    soup_listingdesc = soup.find("div", class_ = "listingdesc")

    ## Create contact info variables
    Farm_name = soup_fullwidth.find("h1", class_ = "inline").get_text()
    Farm_location = soup_fullwidth.find("a").get_text()
    Listing_description = soup_listingdesc.get_text()
    Farm_type = soup_content.find('i').get_text()
    Last_updated = soup_content.find_next('i').find_next('i').find_next('span').get_text()
    if soup_additionalinfo is None:
        Farming_practices = ""
        Association_memberships = ""
    if soup_additionalinfo is not None:
        if soup_additionalinfo.find("div").find("ul") is not None:
            Farming_practices = soup_additionalinfo.find("div").find("ul").get_text()
        if soup_additionalinfo.find("div").find("ul") is None:
            Farming_practices = ""
        if soup_additionalinfo.find_next("div").find_next("div").find_next("div").find("ul") is not None:
            Association_memberships = soup_additionalinfo.find_next("div").find_next("div").find_next("div").find("ul").get_text()
        if soup_additionalinfo.find_next("div").find_next("div").find_next("div").find("ul") is None:
            Association_memberships = ""
    Name = soup_panelcenter.find("span").get_text()
    Phone_number = soup_panelcenter.find_next("span").find_next("span").get_text()

    ### Produce

    Prod_winter = list()
    Prod_spring = list()
    Prod_summer = list()
    Prod_fall = list()

    if soup_content.find("div", id= "Winter") is not None:
        for x in soup_content.find("div", id = "Winter").find_all('li'):
            if x is not None:
                Prod_winter.append(x.get_text())

    if soup_content.find("div", id= "Spring") is not None:
        for x in soup_content.find("div", id = "Spring").find_all('li'):
            if x is not None:
                Prod_spring.append(x.get_text())

    if soup_content.find("div", id= "Summer") is not None:
        for x in soup_content.find("div", id = "Summer").find_all('li'):
            if x is not None:
                Prod_summer.append(x.get_text())

    if soup_content.find("div", id= "Fall") is not None: 
        for x in soup_content.find("div", id = "Fall").find_all('li'):
            if x is not None:
                Prod_fall.append(x.get_text())

    # Links
    ## clear the links
    Link_unique = ""
    Instagram = ""
    Facebook = ""
    Twitter = ""
    Pinterest = ""
    LH_email = ""

    ##set links to their scraped brethren
    for link in soup_panelcenter.find_all(href=link_unique):
        Link_unique += str(link.get('href'))

    for link in soup_panelcenter.find_all(href=insta_link):
        Instagram += str(link.get('href'))

    for link in soup_panelcenter.find_all(href=pin_link):
        Pinterest += str(link.get('href'))

    for link in soup_panelcenter.find_all(href=fb_link):
        Facebook += str(link.get('href'))

    for link in soup_panelcenter.find_all(href=twit_link):
        Twitter += str(link.get('href'))

    for link in soup_panelcenter.find_all(href=postemail_link):
        LH_email += str(base_url + link.get('href'))

    ## Append variables to matrix
    farmers.append([Farm_name, Farm_location, Farm_type, Last_updated, Farming_practices, Association_memberships, Name, Phone_number, Link_unique, LH_email, Instagram, Pinterest, Twitter, Facebook, Prod_winter, Prod_spring, Prod_summer, Prod_fall])
    
    ## Convert matrix to dataframe
    df = pandas.DataFrame(farmers, columns=['Farm_name','Farm_location', 'Farm_type', 'Last_updated', 'Farming_practices', 'Association_memberships', 'Name', "Phone Number", 'Link_unique', 'LH_email', 'Instagram', 'Pinterest', 'Twitter', 'Facebook', 'Prod_winter', 'Prod_spring', 'Prod_summer', 'Prod_fall'])
    
    ## Append dataframe to CSV
    if os.path.exists(filename) == True:
        df.to_csv(filename, mode='a', index=False, header=False)

    if os.path.exists(filename) == False:
        df.to_csv(filename, index=False)

    ## Sleep for 10 - 80 seconds // Too short
    # Using 20 - 100 now
    time.sleep((random.randint(5,80)))

    now = datetime.now()

    ## Print counter
    print(counter, "out of", counter_max, "-", round(counter_pct, 2), "%", "at", now)


print("Done at", now)

## Test output
#print(test_list)


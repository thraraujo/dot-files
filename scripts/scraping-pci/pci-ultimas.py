import requests
from bs4 import BeautifulSoup


# Ultimos resultados do PCI Concursos
URL = "https://www.pciconcursos.com.br/ultimas/"
page = requests.get(URL)
soup = BeautifulSoup(page.content, "html.parser")

results = soup.find_all("div", class_="ca") # 1. link 2. Institution

entries = ["universidade", "faculdade", "instituto"]
positions = ["fisica", "física", "matemática", "matematica"]

total_positions = 0


def anything_for_me(url) -> bool:
    '''
    Here I define a function that returns a boolean.
    It visits the url and checks if there is a position in
    physics or mathematics. 
    '''
    page_url = requests.get(f"{url}")
    page_soup = BeautifulSoup(page_url.content, "html.parser")

    page_text = page_soup.find_all("div", itemprop="articleBody") # Description of the job offer

    for n in range(len(page_text)):

        page_string = page_text[n].text.lower() # Here I collect all relevant texts of the job offer

        something = any(keyword in page_string for keyword in positions)

    return something
    


for j in range(len(results)):

    string = results[j].text.lower() # Here I collect all relevant texts of the job offer
    link = results[j].find('a').get('href')

    # Here I check if any of the entries are in the strings
    has_position = any(entry in string for entry in entries)
    for_me = anything_for_me(link)

    if has_position and for_me:

        total_positions += 1
        print(70*"#")
        print(f" ")
        print(f"{results[j].text}")
        print(link)
        print(f" ")

print(70*"#")

if total_positions == 0:
    print(f" ")
    print(f"No positions today =(")
    print(f" ")
elif total_positions == 1:
    print(f" ")
    print(f"There is just 1 position today")
    print(f" ")
else:
    print(f" ")
    print(f"There are {total_positions} positions today")
    print(f" ")

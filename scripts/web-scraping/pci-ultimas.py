import requests
from bs4 import BeautifulSoup

URL = "https://www.pciconcursos.com.br/ultimas/"

page = requests.get(URL)

soup = BeautifulSoup(page.content, "html.parser")

# results = soup.find_all("div", class_="cd")
# state = soup.find_all("div", class_="cc")
results = soup.find_all("div", class_="ca")


# entries = ["adjunto", "professor", "docente"]
entries = ["universidade", "faculdade"]

for j in range(len(results)):
    string = results[j].text.lower()

    test = [True for entry in entries if entry in string] 

    if any(test):
        print(30*"#")
        print(f"{results[j].text}")


print("That\'s all folks")
print(30*"#")

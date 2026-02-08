#!/usr/bin/env python3

from sys import argv 
import requests
import feedparser # atom parser

url=f"https://rss.arxiv.org/atom/{argv[1]}"

data = requests.get(url)
file = feedparser.parse(data.content)

for link_file in file.entries:
    print(link_file.link, '|', link_file.title,'|', link_file.author)
       

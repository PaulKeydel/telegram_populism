import requests
from bs4 import BeautifulSoup
import pandas as pd

websites = ["https://identitaerebewegung.wordpress.com/",
            "https://identitaerebewegung.wordpress.com/positionierungen/100-identitat-0-rassismus/"]


def getdata(url):
    # add header to prevent being blocked (403 error) by wordpress websites
    #headers = {
    #    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'}
    r = requests.get(url)#, headers=headers)
    return r.text

def get_text_from_html(website_link) -> str:
    html_data = getdata(website_link)
    soup = BeautifulSoup(html_data, "html.parser")
    list_texts = []
    for txt in soup.find_all('p'):
        list_texts.append(txt.text)
    return "\n".join(list_texts)

def get_all_texts(list_websites: list) -> pd.DataFrame:
    df = pd.DataFrame(columns=["link", "text"])
    for i in range(len(list_websites)):
        str = get_text_from_html(list_websites[i])
        str = str.replace('\t', '')
        df = df.append({'link': list_websites[i], 'text': str}, ignore_index=True)
    return df

get_all_texts(websites).to_csv('website_texts.tsv', sep='\t', index=False)
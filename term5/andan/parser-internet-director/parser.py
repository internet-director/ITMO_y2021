import csv
import time
import datetime

import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.options import Options

DOMAIN = "https://rutracker.org/forum/"
URL = DOMAIN + "viewforum.php?f=934"

def get_all_laptop_urls(count_of_pages_to_parse):
    page_urls = []

    for i in range(11, count_of_pages_to_parse):
        page_urls.append(f"{URL}&start={i * 50}")

    return page_urls


def get_url_movies(url):
    response = requests.get(url, headers=HEADERS)
    soup = BeautifulSoup(response.text, 'lxml')

    movies_list = []
    for tag in soup.findAll('a', class_='torTopic bold tt-text'):
        movies_list.append(DOMAIN + tag['href'])

    return movies_list


def get_field(name):
    answer = soup.find('span', string=name)

    if answer is None or answer.next_sibling is None:
        return ""

    return str(answer.next_sibling)[2:]

def get_table():
    data = []
    table = soup.find('table', attrs={'class': 'attach bordered med'})
    table_body = table.find('tbody')

    rows = table_body.find_all('tr')
    for row in rows:
        cols = row.find_all('td')
        cols = [ele.text.strip() for ele in cols]
        data.append([ele for ele in cols if ele])

    return data

def get_title():
    answer = str()
    for word in soup.find('h1', class_='maintitle').find_all('span', class_='cyrillic-char'):
        answer += word.next_sibling.text

    return answer

def get_weight(weight):
    ans = float(weight.split('\xa0', 1)[0])

    if weight[-2] == 'M':
        ans = ans / 1024
    return ans

def get_duration(duration):
    try:
        d = duration.split(' ')[0]
        d = d.split('.')[0]
        x = time.strptime(d, '%H:%M:%S')
        return datetime.timedelta(hours=x.tm_hour, minutes=x.tm_min, seconds=x.tm_sec).total_seconds() / 60 / 60
    except:
        return 0

if __name__ == '__main__':
    urls = get_all_laptop_urls(20)
    session = requests.Session()

    with open('data.csv', 'w', newline='', encoding='utf-8') as data_csv_:
        writer = csv.writer(data_csv_)
        writer.writerow(
            ['Title', 'Жанры', 'Страна', 'Год выпуска', 'Продолжительность', 'Режиссер', 'Качество видео', 'Формат видео',
             'Субтитры', 'Тип', 'Статус', 'Размер', 'Скачан'])

        ffOptions = Options()

        ffOptions.add_argument("-profile")
        ffOptions.add_argument(r'C:\Users\interceo\AppData\Roaming\Mozilla\Firefox\Profiles\t9gbql78.test')
        driver = webdriver.Firefox(options=ffOptions)

        for url in urls:
            url_movies = get_url_movies(url)

            # Parsing 'HTML' code....
            for url_movie in url_movies:
                print(url_movie)
                driver.get(url_movie)
                # print(driver.page_source)
                soup = BeautifulSoup(driver.page_source, 'lxml')

                if soup is not None:
                    try:
                        title = get_title()

                        genres = get_field('Жанр')
                        country = get_field('Страна')
                        date = get_field('Год выпуска')
                        duration = get_duration(get_field('Продолжительность'))
                        director = get_field('Режиссер')

                        video_quality = get_field('Качество видео')
                        video_format = get_field('Формат видео')
                        subtitles = get_field('Субтитры')

                        metadata = get_table()

                        downloaded = metadata[1][1].split('\n')[1][8:-4].replace(',', '')
                        type = metadata[2][1]
                        status = metadata[3][1][2:]
                        weight = get_weight(metadata[4][1].split('\n', 1)[0])

                        writer.writerow([title, genres, country, date, duration, director,
                                         video_quality, video_format, subtitles, type, status, weight, downloaded])

                        time.sleep(0.5)
                    except Exception as e:
                        print(e)

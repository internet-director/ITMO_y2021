import random
import pandas as pd

from sklearn.feature_extraction.text import TfidfVectorizer
import re
from sklearn.linear_model import LogisticRegression

from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

mapa = {'приключения': 0, 'комедия': 1, 'фантастика': 2, 'романтика': 3, 'драма': 4}


def get_random_word(s):
    if not isinstance(s, str):
        return None

    words = re.findall(r'\b\w+\b', s)

    if words:
        return random.choice(words)
    else:
        return None


data = pd.read_csv('data.csv')

data['Жанры'] = data['Жанры'].str.strip().str.lower().map(lambda x: get_random_word(x))
data = data.drop_duplicates(subset=['Описание'])
data = data[data['Описание'].notnull()]
data['Описание'] = data['Описание'].str.strip().str.lower().map(lambda x: re.sub('[^а-я ]', '', x))

data['Жанры'] = data['Жанры'].map(mapa)

data = data[data['Жанры'].notnull()]
data = data[["Описание", "Жанры"]]

docs = list(data['Описание'])
tfdif = TfidfVectorizer(use_idf=True, max_features=20000)

x = tfdif.fit_transform(docs).toarray()
y = data['Жанры']

train, tst, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=7987979, stratify=y)
logr = LogisticRegression(random_state=888)
logr.fit(train, y_train)

expected = logr.predict(train)
predicted = logr.predict(tst)
print("Training result:", accuracy_score(y_train, expected))
print("Testing  result:", accuracy_score(y_test, predicted))

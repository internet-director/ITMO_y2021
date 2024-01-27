import pathlib
import re
import random
import pandas as pd
import shutil

import matplotlib.pyplot as plt
import numpy as np
import os
import PIL
import tensorflow as tf

import keras
from keras import layers
from keras.models import Sequential

HEIGHT = 140
WIDTH = 140

mapa = {'приключения': 0, 'комедия': 1, 'фантастика': 2, 'романтика': 3, 'драма': 4}


def get_random_word(s):
    if not isinstance(s, str):
        return None

    words = re.findall(r'\b\w+\b', s)

    if words:
        return random.choice(words)
    else:
        return None


def copy_files_to_genre_folders(src_data, src, dst):
    for genre, img_list in src_data.items():
        genre_folder = os.path.join(dst, str(int(genre)))
        os.makedirs(genre_folder, exist_ok=True)

        for img_file in img_list:
            name = str(img_file) + ".jpg"
            try:
                source_path = os.path.join(src, name)
                destination_path = os.path.join(genre_folder, name)
                shutil.copy2(source_path, destination_path)
            except:
                var = ()


def write_preview(_train, _class_names):
    plt.figure(figsize=(10, 10))
    for img, name_type in _train.take(1):
        for i in range(12):
            plt.subplot(3, 4, i + 1)
            plt.imshow(img[i].numpy().astype("uint8"))
            num = int(_class_names[name_type[i]])
            txt = ''
            for key, value in mapa.items():
                if int(value) == num:
                    txt = key
                    break

            plt.title(txt)
            plt.axis("off")

    plt.show()


def write_result(_epochs, _history):
    rngs = range(_epochs)

    plt.figure(figsize=(8, 8))
    plt.subplot(1, 2, 1)
    plt.plot(rngs, _history.history['accuracy'], label='Training Accuracy')
    plt.plot(rngs, _history.history['val_accuracy'], label='Testing Accuracy')
    plt.legend(loc='lower right')
    plt.title('Training and Testing Accuracy')

    plt.subplot(1, 2, 2)
    plt.plot(rngs, _history.history['loss'], label='Training Loss')
    plt.plot(rngs, _history.history['val_loss'], label='Testing Loss')
    plt.legend(loc='lower left')
    plt.title('Training and Testing Loss')
    plt.show()


def create_genre_arrays(dat):
    dat = dat[dat['Жанры'].notnull()]
    return dict(dat.groupby('Жанры')['img'].apply(list))


# data = pd.read_csv('data.csv')

# data['Жанры'] = data['Жанры'].str.strip().str.lower().map(lambda x: get_random_word(x))
# data = data[data['img'].notnull()]
# data['Жанры'] = data['Жанры'].map(mapa)

# data = data[data['Жанры'].notnull()]
# data = data[["img", "Жанры"]]

train = keras.utils.image_dataset_from_directory(
    'anime',
    validation_split=0.2,
    subset="training",
    seed=548,
    image_size=(HEIGHT, WIDTH),
    batch_size=32)

test = keras.utils.image_dataset_from_directory(
    'anime',
    validation_split=0.2,
    subset="validation",
    seed=1147,
    image_size=(HEIGHT, WIDTH),
    batch_size=32)

class_names = train.class_names

# write_preview(train, class_names)

train = train.cache().prefetch(buffer_size=tf.data.AUTOTUNE)
test = test.cache().prefetch(buffer_size=tf.data.AUTOTUNE)

model = Sequential([
    layers.Rescaling(1. / 255, input_shape=(HEIGHT, WIDTH, 3)),
    layers.Conv2D(32, 3, padding='same', activation='relu'),
    layers.MaxPooling2D(),
    layers.Conv2D(32, 3, padding='same', activation='relu'),
    layers.MaxPooling2D(),
    layers.Conv2D(64, 3, padding='same', activation='relu'),
    layers.MaxPooling2D(),
    layers.Flatten(),
    layers.Dense(128, activation='relu'),
    layers.Dense(len(class_names))
])

model.compile(optimizer='adam',
              metrics=['accuracy'],
              loss=keras.losses.SparseCategoricalCrossentropy(from_logits=True))

# model.summary()

epochs = 15
results = model.fit(
    train,
    validation_data=test,
    epochs=epochs
)

write_result(epochs, results)

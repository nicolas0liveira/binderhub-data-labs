#!python
from PIL import Image
from posix import listdir
from os.path import isdir
import numpy as np

#carrega imagem
def select_image(filename):
    # load image from file
    image = Image.open(filename)
    # convert to RGB, if needed
    image = image.convert('RGB')
    image = image.resize((150,150))
    # convert to array
    return np.asarray(image)

# Carrega uma 'classe'
def load_classes(diretorio, classe, imagens, labels):
    # iterando arquivos
    
    if diretorio == ".directory" or classe == ".directory":
        return imagens, labels
    
    for filename in listdir(diretorio):
        if filename == ".directory":
            continue

        path = diretorio + filename

        try:
            imagens.append(select_image(path))
            labels.append(classe)
        except:
            print("Erro ao ler imagem {}".format(path))

    return imagens, labels


# seleciona dataset
def select_data_set(diretorio):

    imagens = list()
    labels = list()

    for subdir in listdir(diretorio):
        # path
        path = diretorio + subdir + '/'

        if not isdir(path) or subdir == ".directory":
            continue
        imagens, labels = load_classes(path, subdir, imagens, labels)

    return imagens, labels


if __name__ == "__main__":
    pass

covid_dataset = "/home/nicolas/dev/github/data-garage/python/covid19/analisando-imagens-covid19/dataset/"  #imagens médicas/dataset/
#imagens, labels  = select_data_set(covid_dataset)
#imagens = np.array(imagens) / 255.0  ## convertendo de lista para array
#labels = np.array(labels)  ## convertendo de lista para array

print(select_data_set(covid_dataset))
#select_data_set(covid_dataset)
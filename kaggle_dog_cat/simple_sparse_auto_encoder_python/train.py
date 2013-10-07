import csv
import numpy as np
import sac
import pickle
from sklearn import svm

def read_data(f, test = False):
    data = []
    labels = []

    csv_reader = csv.reader(open(f, "r"), delimiter = ",")
    for row in csv_reader:
        if not test:
            labels.append(int(row[0]))
            row = row[1 : ]

        data.append(np.array(np.int64(row)))

    return data, labels

def get_feature(data, para1, para2):
    data = data.T
    options = sac.SparseAutoEncoderOptions(para1,
                                       para2,
                                       output_dir="output",
                                       max_iterations = 400)
    network = sac.SparseAutoEncoder(options, data)
    answer = network.learn()
    features, a3 = network.feed_forward(data,
                                  answer.W1, answer.W2,
                                  answer.b1, answer.b2)
    return features

def train_classifier(features, labels):
    model_svm = svm.LinearSVC()
    model_svm.fit(features.T, labels)
    model = pickle.dumps(model_svm)

    return model

def train():
    data, labels = read_data("data/train_data_28.csv")
    data = np.array(data)
    labels = np.array(labels)
    data = data[0 : 10]
    labels = labels[0 : 10]
    
    features = get_feature(data, 784, 196)
    model = train_classifier(features, labels)
    return model

if __name__ == "__main__":
    model = train()

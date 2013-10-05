from train import read_data, get_feature, train
import numpy as np
import pickle

def main():
    model = train()
    model_classifier = pickle.loads(model)

    data, labels = read_data("data/train_data_28.csv")
    data = np.array(data)
    labels = np.array(labels)
    data = data[500 : 510]
    labels = labels[500 : 510]
    
    features = get_feature(data, 784, 196)
    predictions = model_classifier.predict(features.T)

    print predictions
    print labels

if __name__ == "__main__":
    main()

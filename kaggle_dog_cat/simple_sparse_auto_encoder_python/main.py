from train import read_data, get_feature, train
import numpy as np
import pickle

def write_file(file_name, predictions):
    f = open(file_name, "w")
    f.write("id,label\n")
    idx = 1
    line_content = ""
    for p in predictions:
        line_content = str(idx) + "," + str(p) + "\n"
        f.write(line_content)
        idx += 1
    f.close()

def main():
    model = train()
    model_classifier = pickle.loads(model)

    data, tmpl = read_data("data/test_data_28.csv", test = True)
    data = np.array(data)
    data = data[: 10]
    
    features = get_feature(data, 784, 196)
    predictions = model_classifier.predict(features.T)

    write_file("output/predictions_dog_cat.csv", predictions)

if __name__ == "__main__":
    main()

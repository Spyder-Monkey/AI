# Import Statements
from numpy import loadtxt
from keras.models import Sequential
from keras.layers import  Dense
from sklearn.preprocessing import MinMaxScaler

# Load dataset
dataset = loadtxt('breasttissue_train.csv', delimiter=',')
# Split into input (X) and output (Y) variables
X = dataset[:,1:10]
Y = dataset[:,0]

X = MinMaxScaler().fit_transform(X)

# Define Keras Model
def get_model():
    model = Sequential()
    model.add(Dense(12, input_dim=9, activation='relu'))
 #   model.add(Dense(9, activation='relu'))
#    model.add(Dense(6, activation='relu'))
    model.add(Dense(4, activation='softmax'))
    # Compile the Keras model
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    # Fit Keras model on the dataset
    model.fit(X, Y, epochs=500, batch_size=10)
    # Evalute Keras model
    _, accuracy = model.evaluate(X, Y)
    print('Accuracy: %.2f' % (accuracy*100))
    return model

get_model()


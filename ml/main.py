import numpy as np

class MLP:
    def __init__(self, input_size, hidden_size, output_size, learning_rate=0.01):
        self.learning_rate = learning_rate
        
        # Initialize weights and biases
        self.W1 = np.random.randn(input_size, hidden_size) * 0.01
        self.b1 = np.zeros((1, hidden_size))
        self.W2 = np.random.randn(hidden_size, output_size) * 0.01
        self.b2 = np.zeros((1, output_size))

    def sigmoid(self, x):
        return 1 / (1 + np.exp(-x))
    
    def sigmoid_derivative(self, x):
        return x * (1 - x)
    
    def softmax(self, x):
        exp_x = np.exp(x - np.max(x, axis=1, keepdims=True))
        return exp_x / np.sum(exp_x, axis=1, keepdims=True)
    
    def forward(self, X):
        # Forward pass
        self.z1 = np.dot(X, self.W1) + self.b1
        self.a1 = self.sigmoid(self.z1)
        self.z2 = np.dot(self.a1, self.W2) + self.b2
        self.a2 = self.softmax(self.z2)
        return self.a2
    
    def backward(self, X, y):
        m = X.shape[0]
        
        # 1. Calculate output error
        delta2 = self.a2 - y   
        # Example: 
        # a2 = [0.7, 0.3]  # our prediction
        # y =  [1.0, 0.0]  # true label
        # delta2 = [-0.3, 0.3]  # we predicted too low for first class, too high for second
        
        # 2. Calculate how much each weight in W2 contributed to error
        dW2 = np.dot(self.a1.T, delta2) / m
        # If a hidden neuron (a1) was very active and output was wrong (delta2)
        # then its weight contributed a lot to the error
        
        # 3. Calculate how much each bias in b2 contributed
        db2 = np.sum(delta2, axis=0, keepdims=True) / m
        # Bias error is just the output error (because bias is added directly)
        
        # 4. Calculate error at hidden layer
        delta1 = np.dot(delta2, self.W2.T) * self.sigmoid_derivative(self.a1)
        # Propagate error backward through weights
        # and account for sigmoid activation
        
        # 5. Calculate how much each weight in W1 contributed
        dW1 = np.dot(X.T, delta1) / m
        
        # 6. Calculate how much each bias in b1 contributed
        db1 = np.sum(delta1, axis=0, keepdims=True) / m
        
        # 7. Update all weights and biases to reduce error
        self.W2 -= self.learning_rate * dW2
        self.b2 -= self.learning_rate * db2
        self.W1 -= self.learning_rate * dW1
        self.b1 -= self.learning_rate * db1
    
    def train(self, X, y, epochs=1000):
        for epoch in range(epochs):
            self.forward(X)
            self.backward(X, y)
            if epoch % 100 == 0:
                # This is the cross-entropy loss calculation:
                loss = -np.sum(y * np.log(self.a2 + 1e-9)) / X.shape[0]
                print(f"Epoch {epoch}: Loss {loss:.4f}")
    
    def predict(self, X):
        output = self.forward(X)
        return np.argmax(output, axis=1)

# Create a simple dataset (e.g., XOR problem)
X = np.array([
    [0, 0],
    [0, 1],
    [1, 0],
    [1, 1]
])

y_labels = np.array([0, 1, 1, 0])  # Labels for XOR
num_classes = 2  # Number of output classes

# Convert labels to one-hot encoding
y = np.eye(num_classes)[y_labels]  

# Initialize MLP
mlp = MLP(input_size=2, hidden_size=4, output_size=2, learning_rate=0.1)

# Train the model
mlp.train(X, y, epochs=1000)

# Predict on the training data
predictions = mlp.predict(X)
print("Predictions:", predictions)

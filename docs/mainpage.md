# Brezel Tensor Framework {#mainpage}

A modern C++ header-only tensor framework inspired by PyTorch

## Overview

Brezel is a high-performance tensor computation library that provides:

- A PyTorch-like API for tensor operations
- Automatic differentiation for gradient-based optimization
- Hardware acceleration support
- Modern C++ design with robust error handling

## Key Features

### Modern C++ Design

- Header-only implementation for easy integration
- Robust error handling with `Expected<T, E>` pattern inspired by Rust
- Comprehensive logging system
- Memory-efficient data structures

### Tensor Operations

```cpp
// Create a tensor
brezel::Tensor<float> a({2, 3}); // 2x3 tensor
brezel::Tensor<float> b({2, 3}); // another 2x3 tensor

// Fill with values
a.fill_(1.0f);
b.fill_(2.0f);

// Perform operations
auto c = a + b;   // Element-wise addition
auto d = a * b;   // Element-wise multiplication
auto e = a.matmul(b.transpose()); // Matrix multiplication

// Reshaping and view operations
auto reshaped = c.reshape({6});
auto view = d.view({3, 2});
```

### Automatic Differentiation

```cpp
// Create variables for autograd
brezel::Variable x = brezel::tensor({1.0f}).requires_grad(true);
brezel::Variable y = x * x + 2 * x + 1;

// Compute gradients
y.backward();

// Get the gradient
float dx = x.grad().item<float>();
```

### Neural Network Components

```cpp
// Define a simple neural network
brezel::nn::Sequential model({
    brezel::nn::Linear(784, 128),
    brezel::nn::ReLU(),
    brezel::nn::Linear(128, 10)
});

// Forward pass
auto output = model->forward(input);

// Define loss function
auto loss = brezel::nn::CrossEntropyLoss()(output, target);

// Backward pass
loss.backward();

// Optimize
brezel::optim::SGD optimizer(model->parameters(), 0.01);
optimizer.step();
```

## Getting Started

### Installation

```bash
git clone https://github.com/salgue441/brezel.git
cd brezel
mkdir build && cd build
cmake ..
cmake --build .
cmake --install .
```

### Basic Usage

```cpp
#include <brezel/core/tensor.hpp>
#include <iostream>

int main() {
    // Create a 2x3 tensor
    brezel::Tensor<float> tensor({2, 3});
    tensor.fill_(5.0f);

    std::cout << "Tensor shape: ["
              << tensor.shape()[0] << ", "
              << tensor.shape()[1] << "]" << std::endl;

    std::cout << "Tensor data: " << tensor.to_string() << std::endl;

    return 0;
}
```

## License

Brezel is licensed under the MIT License. See the LICENSE file for details.

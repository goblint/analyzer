import os
import yaml
import openai

# [TODO] run "sudo pip install openai"
#
# [TODO] get api key and store it in a yaml file:
# organisation: ...
# api-key: ...
#
api_key_path = os.path.expanduser("~/BA/Goblint-Repo/analyzer/scripts/incremental-test-generation/openai-text-completion/APIKEY.yaml")

# Read the api key and organisation
with open(api_key_path, 'r') as file:
    data = yaml.safe_load(file)
organisation = data.get('organisation')
api_key = data.get('api-key')

# Authenticate
openai.organization = organisation
openai.api_key = api_key

# Select Model
model = "text-davinci-edit-001"

input = r'''
#include <stdio.h>

// Function to calculate the sum of two numbers
int sum(int a, int b) {
    return a + b;
}

// Function to calculate the difference of two numbers
int difference(int a, int b) {
    return a - b;
}

// Function to calculate the product of two numbers
int product(int a, int b) {
    return a * b;
}

// Function to calculate the quotient of two numbers
float divide(int a, int b) {
    if (b != 0) {
        return (float)a / b;
    } else {
        printf("Error: Division by zero is not allowed.\n");
        return 0;
    }
}

int main() {
    int num1, num2;

    printf("Enter two numbers: ");
    scanf("%d %d", &num1, &num2);

    printf("Sum: %d\n", sum(num1, num2));
    printf("Difference: %d\n", difference(num1, num2));
    printf("Product: %d\n", product(num1, num2));
    printf("Quotient: %.2f\n", divide(num1, num2));

    return 0;
}
'''

completion = openai.ChatCompletion.create(
  model="gpt-3.5-turbo",
  n = 3,
  messages=[
    {"role": "user", "content": "Please modify the following C code to a state as it might have been in the earlier implementation. You are allowed to remove code, introduce bugs or do any other modifications that might happen in a development workflow. But the code should still compile. Explain shortly what you have changed. Here the C code: " + input},
  ]
)

print(completion.choices[0].message)
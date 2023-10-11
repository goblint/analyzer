int array[10];

// function returns array of numbers
int* getNumbers(void) {
    for (int i = 0; i < 10; ++i) {
       array[i] = i;//NOWARN
    }

    return array;
}

int* getNumbers2(void) {
    int* numbers = getNumbers();
    // numbers2 is local
    int numbers2[10];

    for (int i = 0; i < 10; ++i) {
        numbers2[i] = numbers[i];//NOWARN
    }

    return numbers2;
}

int main(void) {
   int *numbers = getNumbers2();
   numbers[0] = 100;//WARN

   return 0;
}

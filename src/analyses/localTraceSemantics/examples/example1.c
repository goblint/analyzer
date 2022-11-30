void main() {
  int x = 3;
  while (x < 4) {
    x++;
  }
}

// Beispiel Code für trace removal bei branches, wenn die die condition nicht
// erfüllen

// int unknown;
//   int x = 1;
//   if (unknown) {
//     x = 3;
//   } else {
//     x = 7;
//   }
//   int y = 1;
//   if (x < 4) {
//     y = 42;
//   }

// Code für den Fehler in loops
// int x = 3;
// while (x < 4) {
//   x++;
// }

// Code für Rausschmiss von fehlerhaften traces
// int unknown;
//   int x = 1;
//   if (unknown) {
//     x = 3;
//   } else {
//     x = x + 2147483647;
//   }
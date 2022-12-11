void main() {
  int unknown;
  int x;
  if (1 < unknown) {
    x = 3;
    if (x < unknown) {
      x = unknown;
      if (unknown < x) {
        int y = 1;
      }
    }
  } else {
    x = 7;
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
// while (x < 7) {
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

// Code für Lt, wo der Fixpoint angeblich nicht erreicht wird
// int unknown;
// int x;
// if (2 < unknown) {
// x = 3;
// } else {
//   x = 7;
// }

// Lt-example, wo meine overlap-split strategy gezeigt wird
// int unknown;
// int x;
// if (1 < unknown) {
//   x = 3;
//   if (x < unknown) {
//     x = unknown;
//     if (unknown < x) {
//       int y = 1;
//     }
//   }
// } else {
//   x = 7;
// }
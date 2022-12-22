int y = 1;

void main() {
  int x = 7;
  if (x < 4) {
    y = 3;
  } else {
    x = 5;
  }
  x = y - 8;
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

// Code-Beispiel für enter
//  int f(int x) {
//    x = 10;
//    return x - 4;
//  }

// void main() {
//   int x = 7;
//   int y = f(12);
//   x = 3;
// }

// Code-Beispiel für global-Suche --> der richtige Knoten wird schon mal
// rausgesucht
//  int y = 1;

// int f(int x) {
//   x = y;
//   return x - 4;
// }

// void main() {
//   int x = 7;
//   y = 9;
//   y = f(12);
//   x = 3;
// }
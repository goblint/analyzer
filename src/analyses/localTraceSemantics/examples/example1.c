// PARAM: --set ana.activated[+] "localTraces"

int y = 10;

void main() {
  int x = 3;
  int k = 1;
  while (x < 6) {
    k = y;
    y = x + x;
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

// Code für Lt, wo der Fixpoint angeblich nicht erreicht wird (anscheinend doch)
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

// Code-Beispiel für global-Suche
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

// komplizierteres Beispiel für global Suche
//  int y = 10;

// void main() {
//   int x = 3;
//   int k = 1;
//   while (x < 6) {
//     k = y;
//     y = x + x;
//     x++;
//   }
// }

// obwohl ähnlich, funktioniert das hier nicht:
// int global = 5;

// void main() {
//   int x = 7;
//   int k = 1;
//   while (k < 4) {
//     x = k + global;
//     k++;
//     global -= 1;
//   }
// }

// Code-Beispiel für Probleme bei der global Suche

// int x = 3;
// void main() {
//   int k = 2;
//   while (x < 6) {
//     k = 3;  // wenn ich das entferne, dann terminiert es nicht. das ist, weil
//             // ohne das ist sigma gleich, die loops unterscheiden sich nur,
//             wenn
//             // sigma unterschiedlich sind
//     x = x + 1;
//   }
//   k = x;
// }

// Code, das noch nicht klappt
// void main() {
//   int x = 3;
//   int y = 4;
//   while (y < 7) {
//     x++;
//   }
// }
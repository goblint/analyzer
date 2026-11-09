// This file is part of sv-witnesses repository: https://github.com/sosy-lab/sv-witnesses
//
// SPDX-FileCopyrightText: 2023 Dirk Beyer <https://www.sosy-lab.org>
//
// SPDX-License-Identifier: Apache-2.0
// CRAM
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i, j;
    j = 1;
    i = 5;
    while (i-j >= 1) {
        j = j + 1;
        i = i - 1;
    }
    return 0;
}

//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <stdio.h>

struct auth {
  char name[32];
  int auth;
};

struct auth *auth;
char *service;

int main(int argc, char **argv) {
    char line[128];

    while (1) {
        // printf() is considered an implicit deref => need to warn here
        printf("[ auth = %p, service = %p ]\n", auth, service); //WARN

        if (fgets(line, sizeof(line), stdin) == NULL) break;
        
        if (strncmp(line, "auth ", 5) == 0) {
            // No deref happening in the line below => no need to warn
            auth = malloc(sizeof(auth)); //NOWARN
            // memset() is considered an implicit deref => need to warn
            memset(auth, 0, sizeof(auth)); //WARN
            if (strlen(line + 5) < 31) {
                strcpy(auth->name, line + 5); //WARN
            }
        }
        if (strncmp(line, "reset", 5) == 0) {
            free(auth); //WARN
        }
        if (strncmp(line, "service", 6) == 0) {
            service = strdup(line + 7);
        }
        if (strncmp(line, "login", 5) == 0) {
            if (auth->auth) { //WARN
                printf("you have logged in already!\n");
            } else {
                printf("please enter your password\n");
            }
        }
    }
}
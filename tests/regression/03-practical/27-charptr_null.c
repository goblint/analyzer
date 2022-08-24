#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

struct options
{
    char *ip_range;
};

struct options o;

int get_ip_range(int *iprange)
{
    char *r = iprange;

    while (*r++)
    {
        *r = '\0';
        __goblint_check(1);
    }

    return (0);
}

int main()
{
    char *optarg = "was from unistd.h";
    o.ip_range = malloc((strlen(optarg) + 1) * sizeof(char));
    strncpy(o.ip_range, optarg, strlen(optarg));
    o.ip_range[strlen(optarg)] = '\0';
    get_ip_range(o.ip_range);
}
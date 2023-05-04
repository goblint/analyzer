#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x;
    int y;
} Point;

typedef union {
    int i;
    float f;
} IntOrFloat;

typedef enum {
    RED,
    GREEN,
    BLUE
} Color;

Point* create_point(int x, int y) {
    Point* p = malloc(sizeof(Point));
    p->x = x;
    p->y = y;
    return p;
}

IntOrFloat* create_int_or_float(int i, float f) {
    IntOrFloat* io = malloc(sizeof(IntOrFloat));
    io->i = i;
    io->f = f;
    return io;
}

Color get_color() {
    return BLUE;
}

int main() {
    Point* p = create_point(10, 20);
    printf("Point: (%d, %d)\n", p->x, p->y);

    IntOrFloat* io = create_int_or_float(42, 3.14f);
    printf("Int or float: %d, %f\n", io->i, io->f);

    Color c = get_color();
    printf("Color: %d\n", c);

    free(p);
    free(io);

    return 0;
}


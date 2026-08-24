// CRAM
// Sensor calibration reflects coordinates and clamps bias through looped workers.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t sensor_lock;
struct Sensor { int x; int y; int bias; } sensor;

void *axis_worker(void *arg) {
  int off = 0;
  for (int i = 0; i < 4; i++) {
    if (i == 0) off += 1;
    if (i == 1) off -= 2;
    if (i == 2) off += 3;
    if (i == 3) off -= 1;
  }
  pthread_mutex_lock(&sensor_lock);
  /* GHOST axis_worker 1 */ sensor.x = -sensor.x + off;
  pthread_mutex_unlock(&sensor_lock);
  pthread_mutex_lock(&sensor_lock);
  /* GHOST axis_worker 2 */ sensor.y = -sensor.y - off;
  pthread_mutex_unlock(&sensor_lock);
  return 0;
}

void *bias_worker(void *arg) {
  int steps = 0;
  for (int i = 0; i < 5; i++)
    steps += (i < 3);
  pthread_mutex_lock(&sensor_lock);
  /* GHOST bias_worker 1 */ sensor.bias += steps;
  pthread_mutex_unlock(&sensor_lock);
  return 0;
}

int main(void) {
  pthread_t a, b;
  pthread_mutex_init(&sensor_lock, 0);
  sensor.x = -4;
  sensor.y = 7;
  sensor.bias = 2;
  pthread_create(&a, 0, axis_worker, 0);
  pthread_create(&b, 0, bias_worker, 0);
  pthread_join(a, 0);
  pthread_join(b, 0);
  if (sensor.x != 5 || sensor.y != -8 || !(sensor.bias == 5)) {
    reach_error();
    abort();
  }
  return 0;
}

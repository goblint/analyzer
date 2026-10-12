// CRAM
// Media pipeline negotiates codec masks and frame dimensions.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t media_lock;
struct Media { int video_mask; int audio_mask; int width; } media;

static int supported_video(void) {
  int mask = 0;
  for (int codec = 0; codec < 4; codec++)
    if (codec != 1)
      mask |= 1 << codec;
  return mask;
}

void *decoder(void *arg) {
  int mask = supported_video();
  pthread_mutex_lock(&media_lock);
  /* GHOST decoder 1 */ media.video_mask &= mask;
  pthread_mutex_unlock(&media_lock);
  pthread_mutex_lock(&media_lock);
  /* GHOST decoder 2 */ media.width += 16;
  pthread_mutex_unlock(&media_lock);
  return 0;
}

void *encoder(void *arg) {
  int audio = 0;
  for (int i = 1; i <= 3; i++)
    audio |= 1 << i;
  pthread_mutex_lock(&media_lock);
  /* GHOST encoder 1 */ media.audio_mask |= audio;
  pthread_mutex_unlock(&media_lock);
  pthread_mutex_lock(&media_lock);
  /* GHOST encoder 2 */ media.width -= 4;
  pthread_mutex_unlock(&media_lock);
  return 0;
}

int main(void) {
  pthread_t d, e;
  pthread_mutex_init(&media_lock, 0);
  media.video_mask = 15;
  media.audio_mask = 1;
  media.width = 1280;
  pthread_create(&d, 0, decoder, 0);
  pthread_create(&e, 0, encoder, 0);
  pthread_join(d, 0);
  pthread_join(e, 0);
  if (((media.video_mask & 2) != 0) || media.audio_mask != 15 || media.width != 1292) {
    reach_error();
    abort();
  }
  return 0;
}

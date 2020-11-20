extern void abort(void);
extern void __assert_fail(const char *, const char *, unsigned int, const char *) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
void reach_error() { __assert_fail("0", "minepump_spec2_product33.cil.c", 3, "reach_error"); }

extern int __VERIFIER_nondet_int(void);

int waterLevel  =    1;
int methaneLevelCritical  =    0;
void lowerWaterLevel(void)
{
  if (waterLevel > 0) {
    waterLevel = waterLevel - 1;
  }
}
void waterRise(void)
{
  if (waterLevel < 2) {
    waterLevel = waterLevel + 1;
  }
}
void changeMethaneLevel(void)
{
  if (methaneLevelCritical) {
    methaneLevelCritical = 0;
  } else {
    methaneLevelCritical = 1;
  }
}
int isHighWaterSensorDry(void)
{
  if (waterLevel < 2) {
    return 1;
  } else {
    return 0;
  }
}
int isHighWaterLevel(void)
{
  if (isHighWaterSensorDry()) {
    return 0;
  } else {
    return 1;
  }
}

int pumpRunning  =    0;
int methAndRunningLastTime = 0;

void __utac_acc__Specification2_spec__2(void)
{
  if (methaneLevelCritical) {
    if (pumpRunning) {
      if (methAndRunningLastTime) {
        ERROR: {reach_error();abort();}
      } else {
        methAndRunningLastTime = 1;
      }
    } else {
      methAndRunningLastTime = 0;
    }
  } else {
    methAndRunningLastTime = 0;
  }
}

void timeShift(void)
{
  if (pumpRunning) {
    lowerWaterLevel();
  }
  if (! pumpRunning) {
    if (isHighWaterLevel()) {
      pumpRunning = 1;
    }
  }
  __utac_acc__Specification2_spec__2();
}

int main(void)
{
  while (1) {
    if (__VERIFIER_nondet_int()) {
      waterRise();
    }
    if (__VERIFIER_nondet_int()) {
      changeMethaneLevel();
    }
    timeShift();
  }
  return 0;
}
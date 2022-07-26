#include <assert.h>

int main()
{
  // non-deterministically make all variants live
  int r;
  switch (r)
  {
    case 0:
      single();
      break;
    case 1:
      sequential_last();
      break;
    case 2:
      sequential_both();
      break;
    case 3:
      branch_one();
      break;
    case 4:
      branch_both();
      break;
    case 5:
      nested_outer();
      break;
    case 6:
      nested_inner();
      break;
    case 7:
      nested_both();
      break;
    case 8:
      nested_both_switch();
      break;
  }

  return 0;
}

void single()
{
  while (1)
    assert(1);
}

void sequential_last()
{
  int i = 0;
  while (i < 10)
    i++;

  while (1)
    assert(1);
}

void sequential_both()
{
  while (1)
    assert(1);

  while (1)
    assert(1); // NOWARN (unreachable)
}

void branch_one()
{
  int r;
  if (r)
  {
    int i = 0;
    while (i < 10)
      i++;
  }
  else
  {
    while (1)
      assert(1);
  }
}

void branch_both()
{
  int r;
  if (r)
  {
    while (1)
      assert(1);
  }
  else
  {
    while (1)
      assert(1);
  }
}

void nested_outer()
{
  while (1)
  {
    int i = 0;
    while (i < 10)
      i++;
  }
}

void nested_inner()
{
  int i = 0;
  while (i < 10)
  {
    while (1)
      assert(1);
    i++;
  }
}

void nested_both()
{
  while (1)
  {
    while (1)
      assert(1);
  }
}

void nested_both_switch()
{
  // simplified from sv-benchmarks/c/pthread-ext/41_FreeBSD_abd_kbd_sliced.c thr1
  while (1)
  {
    switch (0)
    {
    case 0:
      while (1)
        assert(1);
    }
  }
}

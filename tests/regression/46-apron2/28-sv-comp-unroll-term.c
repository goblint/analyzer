// SKIP PARAM: --set ana.activated[+] apron --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set sem.int.signed_overflow assume_none

// Minimized from sv-benchmarks/c/ldv-linux-3.4-simple/32_1_cilled_ok_nondet_linux-3.4-32_1-drivers--staging--speakup--speakup_spkout.ko-ldv_main0_sequence_infinite_withcheck_stateful.cil.out.i
// using loop unrolling of 1.
// Used to not terminate.

struct speakup_info_t {
  int port_tts;
};
struct speakup_info_t speakup_info;

unsigned char inb(int port) {
  unsigned char value;
  return value;
}

void synth_flush(void) {
  int timeout = 100000;
  int __cil_tmp4;
  while (1) {
    __cil_tmp4 = speakup_info.port_tts + 5;
    inb(__cil_tmp4);
    timeout --;
    if (!timeout)
      return;
  }
}

int main() {
  synth_flush();
  while (1)
    synth_flush();
  return 0;
}

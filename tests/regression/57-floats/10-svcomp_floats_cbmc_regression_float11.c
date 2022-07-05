// PARAM: --enable ana.float.interval

/* the primary purpose of this regression test is not checking the assertions,
    but showing that the invariant does not <<failwith "meet results in empty interval">> anymore*/
int main()
{
  int success = 1;
  int unknown = 1;

  // relations
  if(!(1.0<2.5)) {success = 0;}
  if(!(1.0<=2.5)) {success = 0;}
  if(!(1.01<=1.01)) {unknown = 0;}
  if(!(2.5>1.0)) {success = 0;}
  if(!(2.5>=1.0)) {success = 0;}
  if(!(1.01>=1.01)) {unknown = 0;}
  if(!(!(1.0>=2.5))) {success = 0;}
  if(!(!(1.0>2.5))) {success = 0;}
  if(!(1.0!=2.5)) {success = 0;}

  // same flipped
  if(!(-1.0>-2.5)) {success = 0;}
  if(!(-1.0>=-2.5)) {success = 0;}
  if(!(-1.01>=-1.01)) {unknown = 0;}
  if(!(-2.5<-1.0)) {success = 0;}
  if(!(-2.5<=-1.0)) {success = 0;}
  if(!(-1.01<=-1.01)) {unknown = 0;}
  if(!(!(-1.0<=-2.5))) {success = 0;}
  if(!(!(-1.0<-2.5))) {success = 0;}
  if(!(-1.0!=-2.5)) {success = 0;}

  // involving zero
  if(!(-1.0<0.0)) {success = 0;}
  if(!(0.0>-1.0)) {success = 0;}
  if(!(0.0==-0.0)) {success = 0;}
  if(!(0.0>=-0.0)) {success = 0;}
  if(!(1>0)) {success = 0;}
  if(!(0<1)) {success = 0;}
  if(!(1>-0)) {success = 0;}
  if(!(-0<1)) {success = 0;}
  
  if(!(!(0.999f<0.0f))) {success = 0;}
  if(!(!(-0.999f>-0.0f))) {success = 0;}
  if(!(!(0.999f<=0.0f))) {success = 0;}
  if(!(!(-0.999f>=-0.0f))) {success = 0;}

  assert(success); // SUCCESS
  assert(unknown); // UNKNOWN
}

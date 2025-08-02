#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <time.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>

#ifdef SINGLETON 
  #define COMPILE_MPI 0
  #define COMPILE_OPENMP 0 
  #define COMPILE_CUDA 0
  #define COMPILE_CUQUANTUM 0
  #include "quest/include/quest.h"
#else 
  #include "quest.h" 
#endif 

int main(void) {
  initQuESTEnv();
  reportQuESTEnv();

  Qureg qureg = createForcedQureg(20);

  initRandomPureState(qureg);
  reportQureg(qureg);

  destroyQureg(qureg);
  finalizeQuESTEnv();
}
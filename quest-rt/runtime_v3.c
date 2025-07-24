# include <stdio.h>
# include <math.h>
# include <complex.h>
# include <time.h>
# include <stdlib.h>

// # include "QuEST.h" 
# include "quest.h"

void applyOracle(Qureg qureg, int numQubits, int secret) {

  int bits = secret;

  for (int q=1; q<numQubits; q++) {

    // extract the (q-1)-th bit of secret
    int bit = bits % 2;
    bits /= 2;
    
    // NOT the ancilla, controlling on the q-th qubit
    if (bit)
      controlledNot(qureg, q, 0);
  }
}



void measureResult(Qureg qureg, int secret) {

  // |ind> = |s>|1>
  int ind = 2*secret + 1;

  qreal prob = getProbAmp(qureg, ind);

  printf("success probability: " REAL_QASM_FORMAT " \n", prob);
}



void applyBernsteinVazirani(Qureg qureg, int numQubits, int secret) {

  // start in |0>
  initZeroState(qureg);

  // NOT the ancilla
  pauliX(qureg, 0);

  // H all qubits, including the ancilla
  for (int q=0; q<numQubits; q++)
    hadamard(qureg, q);

  applyOracle(qureg, numQubits, secret);

  for (int q=0; q<numQubits; q++)
    hadamard(qureg, q);

  // infer the output basis state
  measureResult(qureg, secret);
}

typedef enum gateTy 
  { X   = 0
  , Y   = 1
  , X2P = 2
  , X2M = 3
  , Y2P = 4
  , Y2M = 5
  , Z   = 6
  , S   = 7
  , SD  = 8
  , T   = 9
  , TD  = 10
  , XY  = 11
  , XY2P = 12 
  , XY2M = 13 
  , CZ   = 14
  } GTy;

void xgate(Qureg qureg, int target) {
  // X 
  ComplexMatrix2 ux = {
      .real={{0.0, 1.0},
             {1.0, 0.0}},

      .imag={{0.0, 0.0},
             {0.0, 0.0}}
  };
  auto b = isMatrix2Unitary(ux);
  printf("%d\n", b);
  unitary(qureg, target, ux);
}

void ygate(Qureg qureg, int target) {
  ComplexMatrix2 uy = {
      .real={{0, 0},{0, 0}},
      .imag={{0, -1},{1, 0}}
  };
  unitary(qureg, target, uy);
}

void x2pgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);
  ComplexMatrix2 ux2p = {
      .real={{coeff, 0},{0, coeff}},
      .imag={{0, -coeff},{-coeff, 0}}
  };
  unitary(qureg, target, ux2p);
}

void x2mgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);
  ComplexMatrix2 ux2m = {
      .real={{coeff, 0},{0, coeff}},
      .imag={{0, coeff},{coeff, 0}}
  };
  unitary(qureg, target, ux2m);
}

void y2pgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);
  ComplexMatrix2 uy2p = {
      .real={{coeff, -coeff},{coeff, coeff}},
      .imag={{0, 0},{0, 0}}
  };
  unitary(qureg, target, uy2p);
}

void y2mgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);
  ComplexMatrix2 uy2m = {
      .real={{coeff, coeff},{-coeff, coeff}},
      .imag={{0, 0},{0, 0}}
  };
  unitary(qureg, target, uy2m);
}

void zgate(Qureg qureg, int target) {
  ComplexMatrix2 uz = {
      .real={{1, 0},{0, -1}},
      .imag={{0, 0},{0, 0}}
  };
  unitary(qureg, target, uz);
}

void sgate(Qureg qureg, int target) {
  ComplexMatrix2 us = {
      .real={{1, 0},{0, 0}},
      .imag={{0, 0},{0, 1}}
  };
  unitary(qureg, target, us);
}

void sdgate(Qureg qureg, int target) {
  ComplexMatrix2 usd = {
      .real={{1, 0},{0, 0}},
      .imag={{0, 0},{0, -1}}
  };
  unitary(qureg, target, usd);
}

void tgate(Qureg qureg, int target) {
  double const pi = 3.1415926;
  double complex ct = exp((0 + 1.0 * I) * pi / 4);
  ComplexMatrix2 ut = {
      .real={{1, 0},{0, creal(ct)}},
      .imag={{0, 0},{0, cimag(ct)}}
  };
  unitary(qureg, target, ut);
}

void tdgate(Qureg qureg, int target) {
  double const pi = 3.1415926;
  double complex ctd = exp((0 - 1.0 * I) * pi / 4);
  ComplexMatrix2 utd = {
      .real={{1, 0},{0, creal(ctd)}},
      .imag={{0, 0},{0, cimag(ctd)}}
  };
  unitary(qureg, target, utd);
}

void xygate(Qureg qureg, int target, double theta) {
  double complex cxy1 = exp((0 - 1.0 * I) * theta) * (0 - 1.0 * I);
  double complex cxy2 = exp((0 + 1.0 * I) * theta) * (0 - 1.0 * I);
  ComplexMatrix2 uxy = {
      .real={{0, creal(cxy1)},{creal(cxy2), 0}},
      .imag={{0, cimag(cxy1)},{cimag(cxy2), 0}}
  };
  unitary(qureg, target, uxy);
}

void xy2pgate(Qureg qureg, int target, double theta) {
  qreal coeff = 1/sqrt(2);
  double complex cxy1 = exp((0 - 1.0 * I) * theta) * (0 - 1.0 * I);
  double complex cxy2 = exp((0 + 1.0 * I) * theta) * (0 - 1.0 * I);
  double complex cxy2p1 = cxy1 * coeff;
  double complex cxy2p2 = cxy2 * coeff; 
  ComplexMatrix2 uxy2p = {
      .real={{0, creal(cxy2p1)},{creal(cxy2p2), 0}},
      .imag={{0, cimag(cxy2p1)},{cimag(cxy2p2), 0}}
  };
  unitary(qureg, target, uxy2p);
}

void xy2mgate(Qureg qureg, int target, double theta) {
  qreal coeff = 1/sqrt(2);
  double complex cxy2m1 = exp((0 - 1.0 * I) * theta) * (0 + 1.0 * I) * coeff;
  double complex cxy2m2 = exp((0 + 1.0 * I) * theta) * (0 + 1.0 * I) * coeff; 
  ComplexMatrix2 uxy2m = {
      .real={{0, creal(cxy2m1)},{creal(cxy2m2), 0}},
      .imag={{0, cimag(cxy2m1)},{cimag(cxy2m2), 0}}
  };
  unitary(qureg, target, uxy2m);
}

void czgate(Qureg qureg, int target, int target2) {
  ComplexMatrix4 ucz = {
       .real = {{1,0,0,0},
                {0,1,0,0},
                {0,0,1,0},
                {0,0,0,-1}},
       .imag = {{0},
                {0},
                {0},
                {0}}};
  twoQubitUnitary(qureg, target, target2, ucz);
}

int main() {
  
  // auto ptr = fopen("example.circ", "r");
  // fclose(ptr);

  // prepare the hardware-agnostic QuEST environment
  QuESTEnv env = createQuESTEnv();

  // choose the register size
  int numQubits = 15;


  // prepare our register in the |0> state
  Qureg qureg = createQureg(numQubits, env);

    // randomly choose the secret parameter
    //   srand(time(NULL));
    //   int secret = rand() % (int) pow(2, numQubits - 1);
    // search for s using BV's algorithm
    //   applyBernsteinVazirani(qureg, numQubits, secret);

  int target = 3;
  int target2 = 4;

  qreal coeff = 1/sqrt(2);
  double theta = 2.0;

  GTy gt = X;

  switch (gt) {
    case X:
      // X 
      // printf("X gate\n");
      // xgate(qureg, target);
      {
        // ComplexMatrix2 ux = {
        //     .real={{0.0, 1.0},
        //            {1.0, 0.0}},

        //     .imag={{0.0, 0.0},
        //            {0.0, 0.0}}
        // };
        // unitary(qureg, target, ux);
        ComplexMatrix2 u = {
            .real={{.5,.5},{.5,.5}},
            .imag={{.5,-.5},{-.5,.5}}
        };
        unitary(qureg, 0, u);
      }
    case Y:
      // Y
      ygate(qureg, target);
    case X2P:
      // X2P
      x2pgate(qureg, target);
    case X2M:
      // X2M
      x2mgate(qureg, target);
    case Y2P:
      // Y2P
      y2pgate(qureg, target);
    case Y2M:
      // Y2M
      y2mgate(qureg, target);
    case Z:
      // Z
      zgate(qureg, target);
    case S:
      // S
      sgate(qureg, target);
    case SD:
      // SD
      sdgate(qureg, target);
    case T:
      // T
      tgate(qureg, target);
    case TD:
      // TD
      tdgate(qureg, target);
    case XY:
      // XY theta
      xygate(qureg, target, theta);
    case XY2P:
      // XY2P theta 
      xy2pgate(qureg, target, theta);
    case XY2M:
      // XY2M theta
      xy2mgate(qureg, target, theta);
    case CZ:
      // CZ
      czgate(qureg, target, target2);

    default:
      break;
  };

  // tidy up
  destroyQureg(qureg, env);
  destroyQuESTEnv(env);
  return 0;
}

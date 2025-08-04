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
  , M    = 15
  , Kraus = 16
  } GTy;

void xgate(Qureg qureg, int target) {
  // X 
  CompMatr1 matrix = getInlineCompMatr1(
    {{0.0, 1.0},
     {1.0, 0.0}});
  applyCompMatr1(qureg, target, matrix);
}

void ygate(Qureg qureg, int target) {
  // Y
  CompMatr1 matrix = getInlineCompMatr1(
    {{0.0, -1i},
     {1i, 0.0}});
  applyCompMatr1(qureg, target, matrix);
}

void x2pgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, -coeff * 1i},
     {-coeff * 1i, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void x2mgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, coeff * 1i},
     {coeff * 1i, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void y2pgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, -coeff + 0i},
     {coeff + 0i, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void y2mgate(Qureg qureg, int target) {
  qreal coeff = 1/sqrt(2);

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, coeff + 0i},
     {-coeff + 0i, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void zgate(Qureg qureg, int target) {
  CompMatr1 matrix = getInlineCompMatr1(
    {{1.0, 0.0},
     {0.0, -1.0}});
  applyCompMatr1(qureg, target, matrix);
}

void sgate(Qureg qureg, int target) {
  CompMatr1 matrix = getInlineCompMatr1(
    {{1.0, 0.0},
     {0.0, 1i}});
  applyCompMatr1(qureg, target, matrix);
}

void sdgate(Qureg qureg, int target) {
  CompMatr1 matrix = getInlineCompMatr1(
    {{1.0, 0.0},
     {0.0, -1i}});
  applyCompMatr1(qureg, target, matrix);
}

// e^(ix) = cos(x)+i*sin(x)
qcomp imagexp(qreal x) {
  return std::cos(x) + 1i * std::sin(x);
}

void tgate(Qureg qureg, int target) {
  // qreal const pi = 3.1415926;
  qcomp ct = imagexp( M_PI / 4 );

  CompMatr1 matrix = getInlineCompMatr1(
    {{1.0, 0.0},
     {0.0, ct}});
  applyCompMatr1(qureg, target, matrix);
}

void tdgate(Qureg qureg, int target) {
  // qreal const pi = 3.1415926;
  qcomp ct = imagexp( -M_PI / 4 ); 

  CompMatr1 matrix = getInlineCompMatr1(
    {{1.0, 0.0},
     {0.0, ct}});
  applyCompMatr1(qureg, target, matrix);
}

void xygate(Qureg qureg, int target, qreal theta) {
  qcomp cxy1 = imagexp( -theta ) * -1i;
  qcomp cxy2 = imagexp( theta ) * -1i;

  CompMatr1 matrix = getInlineCompMatr1(
    {{0.0, cxy1},
     {cxy2, 0.0}});
  applyCompMatr1(qureg, target, matrix);
}

void xy2pgate(Qureg qureg, int target, qreal theta) {
  qreal const coeff = 1/sqrt(2);

  qcomp cxy1 = imagexp( -theta ) * -1i; 
  qcomp cxy2 = imagexp( theta ) * -1i;

  qcomp cxy2p1 = cxy1 * coeff;
  qcomp cxy2p2 = cxy2 * coeff; 

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, cxy2p1},
     {cxy2p2, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void xy2mgate(Qureg qureg, int target, qreal theta) {
  qreal const coeff = 1/sqrt(2);

  qcomp cxy2m1 = imagexp( -theta ) * 1i * coeff;
  qcomp cxy2m2 = imagexp( theta ) * 1i * coeff;

  CompMatr1 matrix = getInlineCompMatr1(
    {{coeff + 0i, cxy2m1},
     {cxy2m2, coeff + 0i}});
  applyCompMatr1(qureg, target, matrix);
}

void czgate(Qureg qureg, int target, int target2) {
  CompMatr2 matrix = getInlineCompMatr2(
    {{1, 0, 0, 0},
     {0, 1, 0, 0},
     {0, 0, 1, 0},
     {0, 0, 0, -1}});
  applyCompMatr2(qureg, target, target2, matrix);
}

int findOffset(int* dims, int offset) {
    if (offset == 0) {
      return offset;
    } else {
      int acc = 0;
      for (int i=0; i++; i<offset) {
          int dim = dims[i];
          acc += dim * dim * 2;
      }
      return acc;
    }
}

void kraus(Qureg qureg, int target, int target2, int nQb, int* dims, int *channelIndices, int offsetL, int offsetR, double* krausVec) {
    // currently, we assume nQb should always be 1

    int nOps = offsetR - offsetL;
    if ((nQb != 1) && (nQb != 2))
      throw std::runtime_error("An inner runtime error: currently nQb should be 1\n");
    // 3D nested pointers
    int dim = 1 << nQb;
    int offset;
    qcomp*** ptrs = (qcomp***) malloc(nOps * sizeof *ptrs);
    for (int n=0; n<nOps; n++) {
        // current index machanism require all kraus operator have the same dim
        // TODO: support multiple dimensions
        // offset = channelIndices[offsetL + n] * dim * dim * 2;
        offset = findOffset(dims, channelIndices[offsetL + n]);
        ptrs[n] = (qcomp**) malloc(dim * sizeof **ptrs);
        for (int i=0; i<dim; i++) {
            ptrs[n][i] = (qcomp*) malloc(dim * sizeof ***ptrs);
            for (int j=0; j<dim; j++) {
                qreal rl = krausVec[offset];
                offset += 1;
                qreal ig = krausVec[offset];
                offset += 1;
                ptrs[n][i][j] = rl + ig * 1i;
                // std::cout << "n i j: " << n << " " << i << " " << j << "\n";
                // std::cout << "coeffs: " << ptrs[n][i][j] << "\n";
            }
        }
    }
    KrausMap c = createKrausMap(nQb, nOps);
    setKrausMap(c, ptrs);
    int victims[] = {};
    if (nQb == 1) {
      int victims[] = {target};
    } else if (nQb == 2) {
      int victims[] = {target, target2};
    }
    mixKrausMap(qureg, victims, nQb, c);
}

int measure(Qureg qureg, int target) {
  int ret = applyQubitMeasurement(qureg, target);
  return ret;
}
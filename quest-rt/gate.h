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

void kraus(Qureg qureg, int target) {
    // KrausMap a = createKrausMap(1, 3);
    // setInlineKrausMap(a, 1, 3, {
    //     {{1,2_i},{3,4}},
    //     {{5,5},{6_i,6}},
    //     {{1_i,2_i},{-3_i,-4_i}}
    // });
    int numTargets = 1;
    int numOperators = 4;
    qreal p = 0.1;
    qreal l = 0.3;
    KrausMap map = createInlineKrausMap(numTargets, numOperators, {
        {
            {sqrt(p), 0},
            {0, sqrt(p*(1-l))}
        }, {
            {0, sqrt(p*l)}, 
            {0, 0}
        }, {
            {sqrt((1-p)*(1-l)), 0},
            {0, sqrt(1-p)}
        }, {
            {0, 0},
            {sqrt((1-p)*l), 0}
        }
    });
    int victims[] = {target};
    mixKrausMap(qureg, victims, 1, map);
}

int measure(Qureg qureg, int target) {
  int ret = applyQubitMeasurement(qureg, target);
  return ret;
}
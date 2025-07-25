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

// already defined in /usr/include/c++/9/math.h
// #define M_PI 3.14159265358979323846264338327

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

int measure(Qureg qureg, int target) {
  int ret = applyQubitMeasurement(qureg, target);
  return ret;
}

extern "C" {


int prog(int numQubits, int prog_length, int* ps, double* ts, int* measures) {
  initQuESTEnv();
  // reportQuESTEnv();

  Qureg qureg = createForcedQureg(numQubits);
  int theta_count = 0;
  int measure_count = 0;

  for (int i=0; i<prog_length; i++) {
    int index = i * 3;
    GTy gt = static_cast<GTy>(ps[index]);
    int target = ps[index + 1];
    switch (gt) {
      case X:
        xgate(qureg, target);
      case Y:
        ygate(qureg, target);
      case X2P:
        x2pgate(qureg, target);
      case X2M:
        x2mgate(qureg, target);
      case Y2P:
        y2pgate(qureg, target);
      case Y2M:
        y2mgate(qureg, target);
      case Z:
        zgate(qureg, target);
      case S:
        sgate(qureg, target);
      case SD:
        sdgate(qureg, target);
      case T:
        tgate(qureg, target);
      case TD:
        tdgate(qureg, target);
      case XY:
        {
          double theta = ts[theta_count];
          xygate(qureg, target, theta);
          theta_count += 1;
        }
      case XY2P:
        {
          double theta = ts[theta_count];
          xy2pgate(qureg, target, theta);
          theta_count += 1;
        }
      case XY2M:
        {
          double theta = ts[theta_count];
          xy2mgate(qureg, target, theta);
          theta_count += 1;
        }
      case CZ:
        {
          int target2 = ps[index+2];
          czgate(qureg, target, target2);
        }
      case M:
        {
          int ret = measure(qureg, target);
          measures[measure_count] = ret; 
          measure_count += 1;
        }
      default:
        break;
    }
  }

  destroyQureg(qureg);
  finalizeQuESTEnv();

  return 0;

}

int test() {

  initQuESTEnv();
  reportQuESTEnv();

  Qureg qureg = createForcedQureg(20);

  int target = 1;
  int target2 = 2;
  qreal theta = 2.0;

  xgate(qureg, target);
  ygate(qureg, target);
  x2pgate(qureg, target);
  x2mgate(qureg, target);
  y2pgate(qureg, target);
  y2mgate(qureg, target);
  zgate(qureg, target);
  sgate(qureg, target);
  sdgate(qureg, target);
  tgate(qureg, target);
  tdgate(qureg, target);
  xygate(qureg, target, theta);
  xy2pgate(qureg, target, theta);
  xy2mgate(qureg, target, theta);
  czgate(qureg, target, target2);

  reportQuregParams(qureg);

  initRandomPureState(qureg);
  reportQureg(qureg);

  qreal prob = calcTotalProb(qureg);
  
  // if (getQuESTEnv().rank == 0)
  //     std::cout << "Total probability: " << prob << std::endl;

  destroyQureg(qureg);
  finalizeQuESTEnv();

  return 0;
}

}

int main(void) {

  // int const prog_length = 3;
  // int ps[prog_length * 3] = {0, 1, 0, 0, 2, 0, 15, 1, 0};
  // int numQubits = 3;

  std::ifstream file; 
  std::string content;
  file.open("circuit.byte");
  // std::cout << "open the file" << "\n";
  file >> content; 
  int const prog_length = std::stoi( content );
  file >> content;
  int numQubits = std::stoi( content );
  file >> content;
  int const num_measure = std::stoi( content );
  // gt, target, target2, gt, target ...
  int ps[prog_length * 3];
  int measures[num_measure];
  std::cout << "prog_length: " << prog_length << "\n";
  std::cout << "numQubits: " << numQubits << "\n";
  std::cout << "num_measure: " << num_measure << "\n";

  int count = 0;
  while ( file ) {                // always check whether the file is open
    file >> content;              // pipe file's content into stream
    ps[count] = std::stoi( content );
    // std::cout << ps[count] << "\n"; // pipe stream's content to standard output
    count += 1;
  }
  file.close();

  double ts[1] = {0.0};
  prog(numQubits, prog_length, ps, ts, measures);

  std::ofstream ofile;
  ofile.open("measure.byte");

  for (int i=0; i<num_measure; i++){
    std::cout << "measure: " << measures[i] << "\n";
    content = std::to_string(measures[i]);
    ofile << content;
  }
  ofile.close();

  printf("main prog done");
}


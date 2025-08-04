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

#include "gate.h"
// already defined in /usr/include/c++/9/math.h
// #define M_PI 3.14159265358979323846264338327

extern "C" {


int prog(int numQubits, int prog_length, int* ps, double* ts, int* measures) {
  initQuESTEnv();
  // reportQuESTEnv();

  Qureg qureg = createForcedQureg(numQubits);
  int theta_count = 0;
  int measure_count = 0;

  for (int i=0; i<prog_length; i++) {
    int index = i * 5;
    GTy gt = static_cast<GTy>(ps[index]);
    int target = ps[index + 1];
    if (gt == X) {
      xgate(qureg, target);
    } else if (gt == Y) {
      ygate(qureg, target);
    } else if (gt == X2P) {
      x2pgate(qureg, target);
    } else if (gt == X2M) {
      x2mgate(qureg, target);
    } else if (gt == Y2P) {
      y2pgate(qureg, target);
    } else if (gt == Y2M) {
      y2mgate(qureg, target);
    } else if (gt == Z) {
      zgate(qureg, target);
    } else if (gt == S) {
      sgate(qureg, target);
    } else if (gt == SD) {
      sdgate(qureg, target);
    } else if (gt == T) {
      tgate(qureg, target);
    } else if (gt == TD) {
      tdgate(qureg, target);
    } else if (gt == XY) {
      double theta = ts[theta_count];
      xygate(qureg, target, theta);
      theta_count += 1;
    } else if (gt == XY2P) {
      double theta = ts[theta_count];
      xy2pgate(qureg, target, theta);
      theta_count += 1;
    } else if (gt == XY2M) {
      double theta = ts[theta_count];
      xy2mgate(qureg, target, theta);
      theta_count += 1;
    } else if (gt == CZ) {
      int target2 = ps[index+2];
      czgate(qureg, target, target2);
    } else if (gt == M) {
      int ret = measure(qureg, target);
      measures[measure_count] = ret; 
      measure_count += 1;
    } else {
      std::cout << "error, gate type not supported\n";
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


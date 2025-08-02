#lang racket 
(require racket/match)

(define path "runtime.cc")
(define file "runtime.cc")

(define (cp-to-build path) (string-append "cp " path " ./build"))
(define (build file) (string-append "cd build && cmake ../../QuEST -D USER_SOURCE=" file " -D OUTPUT_EXE=runtime"))

(define (builtin)
  (if (directory-exists? "build")
    (system "rm -r build && mkdir build")
    (system "mkdir build")
  )
  (system (cp-to-build path))
  (system (build file))
  (system "cd build && make")
  (system "./build/runtime")
)

(define (previous-path path)
  ; or using match-let
  (define-values (dbase fname t) (split-path path))
  dbase
)

(define (lib-tag)
  (string-append
    "-L"
    (path->string 
      (build-path 
        (current-directory)
        "build"
      )
    )
  )
)

(define (include-tag)
  (string-append
    "-I"
    (path->string 
      (build-path 
        (previous-path (current-directory))
        "QuEST"
      )
    )
  )
)

(define (singleton)
  (system (cp-to-build path))
  (system 
    (string-append
      ; "cd build && g++ runtime.cc -lQuEST -lm "
      ; "cd build && gcc runtime.c -lQuEST -lm "
      "cd build && g++ runtime.cc -lQuEST -lm -fPIC -shared -DSINGLETON "
      (lib-tag)
      " "
      (include-tag)
      " "
      ; "-o runtime"
      "-o libqrun.so"
    )
  )
  (if (directory-exists? "../hquest/lib")
    '()
    (system "mkdir -p ../hquest/lib")
  )
  (system "cp build/libQuEST.so ../hquest/lib/")
  (system "cp build/libqrun.so ../hquest/lib/")
  ;;; (system "LD_LIBRARY_PATH=\"./build\" && ./build/runtime")
)

(define (excutable)
  (system (cp-to-build path))
  (system 
    (string-append
      ; "cd build && g++ runtime.cc -lQuEST -lm "
      ; "cd build && gcc runtime.c -lQuEST -lm "
      "cd build && g++ runtime.cc -lQuEST -lm -DSINGLETON "
      (lib-tag)
      " "
      (include-tag)
      " "
      "-o quest_run"
    )
  )
)


(builtin)
(singleton)
(excutable)
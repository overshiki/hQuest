#lang racket 
(require racket/match)

(define (previous-path path)
  ; or using match-let
  (define-values (dbase fname t) (split-path path))
  dbase
)

(define (compile path file target is-builtin)
  (define (sys command)
    (display command)
    (display "\n")
    (system command)
  )
  (define (cp-to-build path) (string-append "cp " path " ./build"))
  (define (build file) (string-append "cd build && cmake ../../QuEST -D USER_SOURCE=" file " -D OUTPUT_EXE=runtime"))

  (define (builtin)
    (if (directory-exists? "build")
      (sys "rm -r build && mkdir build")
      (sys "mkdir build")
    )
    (let ((file "builtin.cc"))
      (sys (cp-to-build file))
      (sys (build file))
      (sys "cd build && make")
      ; (sys "./build/runtime")
    )
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
      " "
      "-I"
      (path->string (current-directory))
    )
  )



  (define (singleton)
    (sys (cp-to-build path))
    (sys 
      (string-append
        ; "cd build && g++ runtime.cc -lQuEST -lm "
        "cd build && g++ "
        file
        " -lQuEST -lm -fPIC -shared -DSINGLETON "
        (lib-tag)
        " "
        (include-tag)
        " "
        ; "-o runtime"
        "-o "
        target
      )
    )
    (if (directory-exists? "../hquest/lib")
      '()
      (sys "mkdir -p ../hquest/lib")
    )
    (sys "cp build/libQuEST.so ../hquest/lib/")
    (sys (string-append
      "cp "
      "build/"
      target
      " ../hquest/lib/"))
    ;;; (sys "LD_LIBRARY_PATH=\"./build\" && ./build/runtime")
  )

  (define (excutable)
    (sys (cp-to-build path))
    (sys 
      (string-append
        ; "cd build && g++ runtime.cc -lQuEST -lm "
        ; "cd build && gcc runtime.c -lQuEST -lm "
        "cd build && g++ "
        file
        " -lQuEST -lm -DSINGLETON "
        (lib-tag)
        " "
        (include-tag)
        " "
        "-o quest_run"
      )
    )
  )

  ; (if is-builtin
  ;   (builtin)
  ;   '()
  ; )
  (singleton)
  ; (excutable)
)

(let 
  (
    (path "density_matrix_runtime.cc")
    (file "density_matrix_runtime.cc")
    (target "libqrunDM.so")
  )
  (compile path file target #t)
)

(let 
  (
    (path "runtime.cc")
    (file "runtime.cc")
    (target "libqrun.so")
  )
  (compile path file target #f)
)
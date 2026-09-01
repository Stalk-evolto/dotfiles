;;  machine-learning.scm --- machine-learning packages latest.
;; Copyright (C) 2026  Stalk Evolto <stalk@stalk-laptop>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (config packages machine-learning)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages machine-learning)
  #:use-module (ice-9 match)
  #:export (llama-cpp-latest
            python-gguf-latest))

(define-public llama-cpp-latest
  (let ((tag "7375"))
    (package
     (inherit llama-cpp)
      (name "llama-cpp-latest")
      (version (string-append "0.0.0-" tag))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ggml-org/llama.cpp")
               (commit (string-append "b" tag))))
         (file-name (git-file-name name tag))
         (sha256
          (base32 "1i62w6fbc9id5v925fsayb1n57kz3mi9j8kvms40z8lxdyng261m"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:modules '((ice-9 textual-ports)
                    (guix build utils)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build cmake-build-system))
        #:imported-modules `(,@%cmake-build-system-modules
                             (guix build python-build-system))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (format #f "~a~a"
                                     "ggml/src/ggml-vulkan/vulkan-shaders/"
                                     "vulkan-shaders-gen.cpp")
                  (("\"/bin/sh\"")
                   (string-append "\"" (search-input-file inputs "/bin/sh")
                                  "\"")))))
            (add-after 'unpack 'fix-tests
              (lambda _
                ;; test-thread-safety downloads ML model from network,
                ;; cannot run in Guix build environment
                (substitute* '("tests/CMakeLists.txt")
                  (("llama_build_and_test\\(test-thread-safety.cpp.*")
                   "")
                  ;; error while handling argument "-m": expected value for
                  ;; argument
                  (("llama_build_and_test\\(test-arg-parser.cpp.*")
                   ""))
                ;; test-eval-callback downloads ML model from network, cannot
                ;; run in Guix build environment
                (substitute* '("examples/eval-callback/CMakeLists.txt")
                  (("COMMAND llama-eval-callback")
                   "COMMAND true llama-eval-callback"))
                ;; Help it find the test files it needs
                (substitute* "tests/test-chat.cpp"
                  (("\"\\.\\./\"") "\"../source/\""))))
            (add-after 'install 'wrap-python-scripts
              (assoc-ref python:%standard-phases 'wrap))
            (add-after 'install 'remove-tests
              (lambda* (#:key outputs #:allow-other-keys)
                (for-each delete-file (find-files
                                       (string-append (assoc-ref outputs "out")
                                                      "/bin")
                                       "^test-")))))))
      (inputs
       (list curl glslang python-gguf-latest python-minimal openblas spirv-headers
             spirv-tools vulkan-headers vulkan-loader)))))

(define-public python-gguf-latest
  (package
   (inherit llama-cpp-latest)
   (name "python-gguf-latest")
   (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "gguf-py"))))))
    (inputs '())
    (propagated-inputs (list python-numpy python-pyyaml python-pyside-6
                             python-sentencepiece python-tqdm))
    (native-inputs (list python-poetry-core python-pytest))
    (home-page "https://ggml.ai")
    (synopsis "Read and write ML models in GGUF for GGML")
    (description "A Python library for reading and writing GGUF & GGML format ML models.")))

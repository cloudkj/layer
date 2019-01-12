(require-extension srfi-4 srfi-78)

(include "test/common.scm")
(include "src/functions.scm")

(check (relu 0) => 0)
(check (relu 1) => 1)
(check (relu -1) => 0)

(check (sigmoid 0.3775) (=> approx=?) 0.593269992)
(check (sigmoid 0.3925) (=> approx=?) 0.596884378)

(check (tanh 0.3775) (=> approx=?) 0.360534393)
(check (tanh 0.3925) (=> approx=?) 0.373513453)

(check (softmax! (f64vector 0.42178394 0.92261711 0.48378937 0.44201963 0.07731554 0.71055332) 2)
       (=> f64vector-approx=?)
       (f64vector 0.37734491 0.62265515 0.51044095 0.48955908 0.34677672 0.65322322))

(check (softmax! (f64vector 0.42178394 0.92261711 0.48378937 0.44201963 0.07731554 0.71055332) 3)
       (=> f64vector-approx=?)
       (f64vector 0.26924688 0.44428307 0.28647009 0.33306164 0.23127869 0.43565965))

(check (softmax! (f64vector 0.42178394 0.92261711 0.48378937 0.44201963 0.07731554 0.71055332) 6)
       (=> f64vector-approx=?)
       (f64vector 0.14753862 0.24345282 0.15697639 0.15055457 0.10454541 0.19693218))

(check (softmax! (f64vector 0.42178394 0.92261711 0.48378937 0.44201963 0.07731554 0.71055332))
       (=> f64vector-approx=?)
       (f64vector 0.14753862 0.24345282 0.15697639 0.15055457 0.10454541 0.19693218))

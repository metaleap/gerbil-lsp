(def (sq x)
  (* x x))

(def (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(def (improve guess x)
  (average guess (/ x guess)))

(def (average x y)
  (* (+ x y) 0.5))

(def (good-enough? guess x)
  (< (abs (- (sq guess) x)) 0.001))

(def (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9.0)

(def (sq x) (* x x))

(def (proc n1 n2 n3)
  (def l1 (if (> n1 n2) n1 n2))
  (def s1 (if (> n1 n2) n2 n1))
  (def l2 (if (> n3 l1) n3 (if (> n3 s1) n3 s1)))
  (+ (sq l1) (sq l2))
  )

(proc 22 33 11 )

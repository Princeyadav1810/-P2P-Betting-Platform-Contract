;; A simple contract to log daily water intake per user

(define-map water-log principal uint)
(define-data-var total-water-consumed uint u0)

(define-constant err-invalid-amount (err u100))

;; Log water intake in milliliters (mL)
(define-public (log-water (ml uint))
  (begin
    (asserts! (> ml u0) err-invalid-amount)
    (map-set water-log tx-sender
             (+ (default-to u0 (map-get? water-log tx-sender)) ml))
    (var-set total-water-consumed (+ (var-get total-water-consumed) ml))
    (ok true)))

;; Get your total logged water intake
(define-read-only (get-my-water-intake)
  (ok (default-to u0 (map-get? water-log tx-sender))))

;; Decentralized Credit Scoring Smart Contract
;; This contract manages credit scores in a decentralized manner
;; allowing lenders to assess borrower creditworthiness

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SCORE (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-LOAN-NOT-FOUND (err u106))
(define-constant ERR-LOAN-ALREADY-REPAID (err u107))
(define-constant ERR-PAYMENT-OVERDUE (err u108))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Credit score bounds
(define-constant MIN-CREDIT-SCORE u300)
(define-constant MAX-CREDIT-SCORE u850)

;; Data structures
(define-map credit-scores
  { user: principal }
  {
    score: uint,
    last-updated: uint,
    total-loans: uint,
    repaid-loans: uint,
    defaulted-loans: uint,
    total-borrowed: uint,
    total-repaid: uint
  }
)

(define-map authorized-reporters
  { reporter: principal }
  { authorized: bool }
)

(define-map loan-records
  { loan-id: uint }
  {
    borrower: principal,
    lender: principal,
    amount: uint,
    interest-rate: uint,
    due-date: uint,
    repaid: bool,
    repaid-amount: uint,
    created-at: uint
  }
)

;; Counters
(define-data-var loan-counter uint u0)
(define-data-var total-users uint u0)

;; Initialize contract owner as authorized reporter
(map-set authorized-reporters { reporter: CONTRACT-OWNER } { authorized: true })

;; Read-only functions

(define-read-only (get-credit-score (user principal))
  (match (map-get? credit-scores { user: user })
    score-data (ok score-data)
    ERR-USER-NOT-FOUND
  )
)

(define-read-only (get-loan-record (loan-id uint))
  (match (map-get? loan-records { loan-id: loan-id })
    loan-data (ok loan-data)
    ERR-LOAN-NOT-FOUND
  )
)

(define-read-only (is-authorized-reporter (reporter principal))
  (default-to false (get authorized (map-get? authorized-reporters { reporter: reporter })))
)

(define-read-only (calculate-credit-score (user principal))
  (match (map-get? credit-scores { user: user })
    user-data 
    (let (
      (total-loans (get total-loans user-data))
      (repaid-loans (get repaid-loans user-data))
      (defaulted-loans (get defaulted-loans user-data))
      (total-borrowed (get total-borrowed user-data))
      (total-repaid (get total-repaid user-data))
    )
    (if (is-eq total-loans u0)
      u650 ;; Default score for new users
      (let (
        (repayment-rate (/ (* repaid-loans u100) total-loans))
        (default-rate (/ (* defaulted-loans u100) total-loans))
        (repayment-amount-rate (if (> total-borrowed u0) 
                                 (/ (* total-repaid u100) total-borrowed) 
                                 u100))
        (base-score u650)
        (repayment-bonus (/ (* repayment-rate u150) u100))
        (default-penalty (/ (* default-rate u200) u100))
        (amount-bonus (/ (* repayment-amount-rate u50) u100))
      )
      (let ((calculated-score (+ (- (+ base-score repayment-bonus amount-bonus) default-penalty) u0)))
        (if (< calculated-score MIN-CREDIT-SCORE)
          MIN-CREDIT-SCORE
          (if (> calculated-score MAX-CREDIT-SCORE)
            MAX-CREDIT-SCORE
            calculated-score
          )
        )
      )
      )
    ))
    u650 ;; Default for users not found
  )
)

(define-read-only (get-total-users)
  (var-get total-users)
)

(define-read-only (get-loan-counter)
  (var-get loan-counter)
)

;; Public functions

(define-public (initialize-user-score (user principal))
  (begin
    (asserts! (is-none (map-get? credit-scores { user: user })) ERR-ALREADY-EXISTS)
    (map-set credit-scores 
      { user: user }
      {
        score: u650,
        last-updated: block-height,
        total-loans: u0,
        repaid-loans: u0,
        defaulted-loans: u0,
        total-borrowed: u0,
        total-repaid: u0
      }
    )
    (var-set total-users (+ (var-get total-users) u1))
    (ok true)
  )
)

(define-public (add-authorized-reporter (reporter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-reporters { reporter: reporter } { authorized: true })
    (ok true)
  )
)

(define-public (remove-authorized-reporter (reporter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-reporters { reporter: reporter } { authorized: false })
    (ok true)
  )
)

(define-public (record-loan (borrower principal) (lender principal) (amount uint) (interest-rate uint) (duration-blocks uint))
  (let (
    (loan-id (+ (var-get loan-counter) u1))
    (due-date (+ block-height duration-blocks))
  )
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (or (is-eq tx-sender borrower) (is-eq tx-sender lender)) ERR-NOT-AUTHORIZED)
    
    ;; Initialize user if doesn't exist
    (if (is-none (map-get? credit-scores { user: borrower }))
      (unwrap! (initialize-user-score borrower) ERR-USER-NOT-FOUND)
      true
    )
    
    ;; Record the loan
    (map-set loan-records
      { loan-id: loan-id }
      {
        borrower: borrower,
        lender: lender,
        amount: amount,
        interest-rate: interest-rate,
        due-date: due-date,
        repaid: false,
        repaid-amount: u0,
        created-at: block-height
      }
    )
    
    ;; Update borrower's loan count and total borrowed
    (match (map-get? credit-scores { user: borrower })
      user-data
      (let (
        (new-total-loans (+ (get total-loans user-data) u1))
        (new-total-borrowed (+ (get total-borrowed user-data) amount))
        (new-score (calculate-credit-score borrower))
      )
      (map-set credit-scores
        { user: borrower }
        (merge user-data {
          total-loans: new-total-loans,
          total-borrowed: new-total-borrowed,
          score: new-score,
          last-updated: block-height
        })
      ))
      false
    )
    
    (var-set loan-counter loan-id)
    (ok loan-id)
  ))
)

(define-public (record-loan-repayment (loan-id uint) (repaid-amount uint))
  (match (map-get? loan-records { loan-id: loan-id })
    loan-data
    (let (
      (borrower (get borrower loan-data))
      (is-overdue (> block-height (get due-date loan-data)))
    )
    (begin
      (asserts! (not (get repaid loan-data)) ERR-LOAN-ALREADY-REPAID)
      (asserts! (> repaid-amount u0) ERR-INVALID-AMOUNT)
      (asserts! (or (is-eq tx-sender borrower) (is-authorized-reporter tx-sender)) ERR-NOT-AUTHORIZED)
      
      ;; Update loan record
      (map-set loan-records
        { loan-id: loan-id }
        (merge loan-data {
          repaid: true,
          repaid-amount: repaid-amount
        })
      )
      
      ;; Update borrower's credit data
      (match (map-get? credit-scores { user: borrower })
        user-data
        (let (
          (new-repaid-loans (+ (get repaid-loans user-data) u1))
          (new-total-repaid (+ (get total-repaid user-data) repaid-amount))
          (new-defaulted-loans (if is-overdue 
                                 (+ (get defaulted-loans user-data) u1)
                                 (get defaulted-loans user-data)))
        )
        (let ((new-score (calculate-credit-score borrower)))
          (map-set credit-scores
            { user: borrower }
            (merge user-data {
              repaid-loans: new-repaid-loans,
              total-repaid: new-total-repaid,
              defaulted-loans: new-defaulted-loans,
              score: new-score,
              last-updated: block-height
            })
          )
        ))
        false
      )
      (ok true)
    ))
    ERR-LOAN-NOT-FOUND
  )
)

(define-public (record-loan-default (loan-id uint))
  (match (map-get? loan-records { loan-id: loan-id })
    loan-data
    (let ((borrower (get borrower loan-data)))
    (begin
      (asserts! (is-authorized-reporter tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (not (get repaid loan-data)) ERR-LOAN-ALREADY-REPAID)
      (asserts! (> block-height (get due-date loan-data)) ERR-PAYMENT-OVERDUE)
      
      ;; Update borrower's default count
      (match (map-get? credit-scores { user: borrower })
        user-data
        (let (
          (new-defaulted-loans (+ (get defaulted-loans user-data) u1))
          (new-score (calculate-credit-score borrower))
        )
        (map-set credit-scores
          { user: borrower }
          (merge user-data {
            defaulted-loans: new-defaulted-loans,
            score: new-score,
            last-updated: block-height
          })
        ))
        false
      )
      (ok true)
    ))
    ERR-LOAN-NOT-FOUND
  )
)

(define-public (update-credit-score (user principal))
  (match (map-get? credit-scores { user: user })
    user-data
    (let ((new-score (calculate-credit-score user)))
    (begin
      (map-set credit-scores
        { user: user }
        (merge user-data {
          score: new-score,
          last-updated: block-height
        })
      )
      (ok new-score)
    ))
    ERR-USER-NOT-FOUND
  )
)
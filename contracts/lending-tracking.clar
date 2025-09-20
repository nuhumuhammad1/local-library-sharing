;; lending-tracking
;; Track borrowed items and return dates across the network

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u600))
(define-constant ERR_LOAN_NOT_FOUND (err u601))
(define-constant ERR_RESOURCE_NOT_AVAILABLE (err u602))
(define-constant ERR_ALREADY_BORROWED (err u603))
(define-constant ERR_INVALID_LOAN (err u604))
(define-constant ERR_LOAN_OVERDUE (err u605))
(define-constant ERR_ALREADY_RETURNED (err u606))

;; data vars
(define-data-var loan-counter uint u0)
(define-data-var default-loan-duration uint u1008) ;; ~1 week in blocks
(define-data-var late-fee-rate uint u5) ;; 5 tokens per day late
(define-data-var max-concurrent-loans uint u5)

;; data maps
(define-map loans
  uint
  {
    resource-id: uint,
    borrower: principal,
    lender: principal,
    start-date: uint,
    due-date: uint,
    return-date: (optional uint),
    status: (string-ascii 20), ;; active, returned, overdue, disputed
    deposit-amount: uint,
    late-fees: uint,
    condition-on-borrow: uint,
    condition-on-return: (optional uint),
    notes: (string-utf8 300)
  }
)

(define-map borrower-history
  principal
  {
    total-loans: uint,
    active-loans: uint,
    completed-loans: uint,
    overdue-loans: uint,
    total-late-fees: uint,
    reputation-score: uint,
    last-activity: uint
  }
)

(define-map lender-history
  principal
  {
    total-lends: uint,
    active-lends: uint,
    completed-lends: uint,
    total-earnings: uint,
    average-rating: uint,
    resources-available: uint
  }
)

(define-map loan-extensions
  uint ;; loan-id
  {
    original-due-date: uint,
    extended-due-date: uint,
    extension-fee: uint,
    approved-by-lender: bool,
    extension-reason: (string-utf8 200)
  }
)

(define-map dispute-cases
  uint ;; loan-id
  {
    dispute-type: (string-ascii 30), ;; damage, late, missing, other
    filed-by: principal,
    description: (string-utf8 500),
    evidence-hash: (optional (buff 32)),
    filed-date: uint,
    resolution-date: (optional uint),
    status: (string-ascii 20), ;; pending, resolved, escalated
    resolution: (optional (string-utf8 300))
  }
)

;; public functions

;; Request to borrow a resource
(define-public (request-loan
  (resource-id uint)
  (duration-blocks uint)
  (deposit-amount uint)
  (notes (string-utf8 300)))
  (let (
    (loan-id (+ (var-get loan-counter) u1))
    (borrower-stats (default-to 
      { total-loans: u0, active-loans: u0, completed-loans: u0, overdue-loans: u0, total-late-fees: u0, reputation-score: u100, last-activity: u0 }
      (map-get? borrower-history tx-sender)
    ))
  )
    ;; Check borrower eligibility
    (asserts! (< (get active-loans borrower-stats) (var-get max-concurrent-loans)) ERR_INVALID_LOAN)
    (asserts! (>= (get reputation-score borrower-stats) u60) ERR_NOT_AUTHORIZED)
    (asserts! (> duration-blocks u0) ERR_INVALID_LOAN)
    (asserts! (<= duration-blocks u4032) ERR_INVALID_LOAN) ;; Max 4 weeks
    
    ;; This would normally check resource availability from resource-catalog
    ;; For now, we'll create a placeholder loan
    (map-set loans loan-id {
      resource-id: resource-id,
      borrower: tx-sender,
      lender: tx-sender, ;; Would be fetched from resource catalog
      start-date: stacks-block-height,
      due-date: (+ stacks-block-height duration-blocks),
      return-date: none,
      status: "active",
      deposit-amount: deposit-amount,
      late-fees: u0,
      condition-on-borrow: u5, ;; Would be fetched from resource
      condition-on-return: none,
      notes: notes
    })
    
    ;; Update borrower statistics
    (map-set borrower-history tx-sender
      (merge borrower-stats {
        total-loans: (+ (get total-loans borrower-stats) u1),
        active-loans: (+ (get active-loans borrower-stats) u1),
        last-activity: stacks-block-height
      })
    )
    
    (var-set loan-counter loan-id)
    (ok loan-id)
  )
)

;; Return a borrowed resource
(define-public (return-resource
  (loan-id uint)
  (condition-on-return uint)
  (return-notes (string-utf8 300)))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (borrower-stats (unwrap-panic (map-get? borrower-history tx-sender)))
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan) "active") ERR_ALREADY_RETURNED)
    (asserts! (and (> condition-on-return u0) (<= condition-on-return u5)) ERR_INVALID_LOAN)
    
    ;; Calculate late fees if overdue
    (let (
      (current-block stacks-block-height)
      (due-date (get due-date loan))
      (late-days (if (> current-block due-date) (/ (- current-block due-date) u144) u0))
      (late-fees (* late-days (var-get late-fee-rate)))
    )
      ;; Update loan record
      (map-set loans loan-id
        (merge loan {
          return-date: (some current-block),
          status: (if (> late-days u0) "returned-late" "returned"),
          condition-on-return: (some condition-on-return),
          late-fees: late-fees,
          notes: return-notes
        })
      )
      
      ;; Update borrower statistics
      (map-set borrower-history tx-sender
        (merge borrower-stats {
          active-loans: (- (get active-loans borrower-stats) u1),
          completed-loans: (+ (get completed-loans borrower-stats) u1),
          overdue-loans: (if (> late-days u0) (+ (get overdue-loans borrower-stats) u1) (get overdue-loans borrower-stats)),
          total-late-fees: (+ (get total-late-fees borrower-stats) late-fees),
          last-activity: current-block
        })
      )
      
      (ok { returned: true, late-fees: late-fees, condition-difference: (- (get condition-on-borrow loan) condition-on-return) })
    )
  )
)

;; Extend loan duration
(define-public (request-extension
  (loan-id uint)
  (additional-days uint)
  (extension-reason (string-utf8 200)))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (extension-fee (* additional-days u2)) ;; 2 tokens per day
  )
    (asserts! (is-eq tx-sender (get borrower loan)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan) "active") ERR_INVALID_LOAN)
    (asserts! (and (> additional-days u0) (<= additional-days u14)) ERR_INVALID_LOAN)
    
    (map-set loan-extensions loan-id {
      original-due-date: (get due-date loan),
      extended-due-date: (+ (get due-date loan) (* additional-days u144)),
      extension-fee: extension-fee,
      approved-by-lender: false,
      extension-reason: extension-reason
    })
    
    (ok extension-fee)
  )
)

;; Approve loan extension (lender)
(define-public (approve-extension (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (extension (unwrap! (map-get? loan-extensions loan-id) ERR_LOAN_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get lender loan)) ERR_NOT_AUTHORIZED)
    
    ;; Update loan due date
    (map-set loans loan-id
      (merge loan { due-date: (get extended-due-date extension) })
    )
    
    ;; Mark extension as approved
    (map-set loan-extensions loan-id
      (merge extension { approved-by-lender: true })
    )
    
    (ok true)
  )
)

;; File a dispute
(define-public (file-dispute
  (loan-id uint)
  (dispute-type (string-ascii 30))
  (description (string-utf8 500))
  (evidence-hash (optional (buff 32))))
  (let (
    (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
  )
    (asserts! 
      (or 
        (is-eq tx-sender (get borrower loan))
        (is-eq tx-sender (get lender loan))
      )
      ERR_NOT_AUTHORIZED
    )
    
    (map-set dispute-cases loan-id {
      dispute-type: dispute-type,
      filed-by: tx-sender,
      description: description,
      evidence-hash: evidence-hash,
      filed-date: stacks-block-height,
      resolution-date: none,
      status: "pending",
      resolution: none
    })
    
    ;; Update loan status
    (map-set loans loan-id
      (merge loan { status: "disputed" })
    )
    
    (ok true)
  )
)

;; Calculate reputation score
(define-public (update-reputation (user principal))
  (let (
    (borrower-stats (unwrap! (map-get? borrower-history user) ERR_NOT_AUTHORIZED))
    (total-loans (get total-loans borrower-stats))
    (overdue-loans (get overdue-loans borrower-stats))
    (completed-loans (get completed-loans borrower-stats))
  )
    (if (> total-loans u0)
      (let (
        (completion-rate (/ (* completed-loans u100) total-loans))
        (overdue-rate (/ (* overdue-loans u100) total-loans))
        (base-score u100)
        (penalty (/ (* overdue-rate u30) u100)) ;; Max 30 point penalty
        (new-score (if (> base-score penalty) (- base-score penalty) u10))
      )
        (map-set borrower-history user
          (merge borrower-stats { reputation-score: new-score })
        )
        (ok new-score)
      )
      (ok u100)
    )
  )
)

;; read only functions

;; Get loan details
(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id)
)

;; Get borrower history
(define-read-only (get-borrower-history (borrower principal))
  (map-get? borrower-history borrower)
)

;; Get lender history
(define-read-only (get-lender-history (lender principal))
  (map-get? lender-history lender)
)

;; Get loan extension details
(define-read-only (get-extension (loan-id uint))
  (map-get? loan-extensions loan-id)
)

;; Get dispute details
(define-read-only (get-dispute (loan-id uint))
  (map-get? dispute-cases loan-id)
)

;; Get total loans count
(define-read-only (get-loan-count)
  (var-get loan-counter)
)

;; Check if loan is overdue
(define-read-only (is-loan-overdue (loan-id uint))
  (match (map-get? loans loan-id)
    loan
      (and
        (is-eq (get status loan) "active")
        (> stacks-block-height (get due-date loan))
      )
    false
  )
)

;; Get user's active loans
(define-read-only (get-active-loan-count (user principal))
  (match (map-get? borrower-history user)
    stats (get active-loans stats)
    u0
  )
)

;; Check borrowing eligibility
(define-read-only (is-eligible-to-borrow (user principal))
  (match (map-get? borrower-history user)
    stats
      (and
        (< (get active-loans stats) (var-get max-concurrent-loans))
        (>= (get reputation-score stats) u60)
      )
    true ;; New users are eligible
  )
)

;; private functions

;; Calculate late fee
(define-private (calculate-late-fee (due-date uint) (return-date uint))
  (if (> return-date due-date)
    (let (
      (days-late (/ (- return-date due-date) u144))
    )
      (* days-late (var-get late-fee-rate))
    )
    u0
  )
)

;; Validate dispute type
(define-private (is-valid-dispute-type (dispute-type (string-ascii 30)))
  (or
    (is-eq dispute-type "damage")
    (is-eq dispute-type "late")
    (is-eq dispute-type "missing")
    (is-eq dispute-type "condition")
    (is-eq dispute-type "other")
  )
)

;; Validate loan status
(define-private (is-valid-loan-status (status (string-ascii 20)))
  (or
    (is-eq status "active")
    (is-eq status "returned")
    (is-eq status "returned-late")
    (is-eq status "overdue")
    (is-eq status "disputed")
  )
)


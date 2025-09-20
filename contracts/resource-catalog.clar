;; resource-catalog
;; Catalog of books, tools, and resources available for sharing

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u500))
(define-constant ERR_RESOURCE_NOT_FOUND (err u501))
(define-constant ERR_INVALID_RESOURCE (err u502))
(define-constant ERR_ALREADY_EXISTS (err u503))
(define-constant ERR_NOT_AVAILABLE (err u504))
(define-constant ERR_INVALID_CONDITION (err u505))

;; data vars
(define-data-var resource-counter uint u0)
(define-data-var category-counter uint u0)
(define-data-var min-condition-rating uint u3)

;; data maps
(define-map resources
  uint
  {
    owner: principal,
    title: (string-utf8 200),
    description: (string-utf8 500),
    category: (string-ascii 50),
    resource-type: (string-ascii 30), ;; book, tool, equipment, digital
    condition: uint, ;; 1-5 scale
    estimated-value: uint,
    location-hash: (buff 32),
    availability-status: (string-ascii 20), ;; available, borrowed, maintenance
    tags: (list 10 (string-ascii 30)),
    created-at: uint,
    last-updated: uint,
    total-borrows: uint,
    average-rating: uint,
    is-active: bool
  }
)

(define-map resource-categories
  (string-ascii 50)
  {
    category-id: uint,
    name: (string-ascii 50),
    description: (string-utf8 300),
    resource-count: uint,
    is-active: bool
  }
)

(define-map resource-ratings
  { resource-id: uint, rater: principal }
  {
    rating: uint,
    review: (string-utf8 300),
    timestamp: uint
  }
)

(define-map owner-resources
  principal
  {
    total-resources: uint,
    active-resources: uint,
    total-borrows: uint,
    average-rating: uint,
    contribution-score: uint
  }
)

(define-map resource-requests
  uint
  {
    requester: principal,
    resource-title: (string-utf8 200),
    description: (string-utf8 500),
    category: (string-ascii 50),
    max-value: uint,
    request-date: uint,
    fulfilled: bool
  }
)

;; public functions

;; Add a new resource to the catalog
(define-public (add-resource
  (title (string-utf8 200))
  (description (string-utf8 500))
  (category (string-ascii 50))
  (resource-type (string-ascii 30))
  (condition uint)
  (estimated-value uint)
  (location-hash (buff 32))
  (tags (list 10 (string-ascii 30))))
  (let (
    (resource-id (+ (var-get resource-counter) u1))
  )
    (asserts! (> (len title) u0) ERR_INVALID_RESOURCE)
    (asserts! (and (> condition u0) (<= condition u5)) ERR_INVALID_CONDITION)
    (asserts! (>= condition (var-get min-condition-rating)) ERR_INVALID_CONDITION)
    (asserts! (> estimated-value u0) ERR_INVALID_RESOURCE)
    
    (map-set resources resource-id {
      owner: tx-sender,
      title: title,
      description: description,
      category: category,
      resource-type: resource-type,
      condition: condition,
      estimated-value: estimated-value,
      location-hash: location-hash,
      availability-status: "available",
      tags: tags,
      created-at: stacks-block-height,
      last-updated: stacks-block-height,
      total-borrows: u0,
      average-rating: u0,
      is-active: true
    })
    
    ;; Update owner statistics
    (let (
      (current-stats (default-to 
        { total-resources: u0, active-resources: u0, total-borrows: u0, average-rating: u0, contribution-score: u0 }
        (map-get? owner-resources tx-sender)
      ))
    )
      (map-set owner-resources tx-sender
        (merge current-stats {
          total-resources: (+ (get total-resources current-stats) u1),
          active-resources: (+ (get active-resources current-stats) u1),
          contribution-score: (+ (get contribution-score current-stats) u10)
        })
      )
    )
    
    ;; Update category count
    (update-category-count category u1)
    
    (var-set resource-counter resource-id)
    (ok resource-id)
  )
)

;; Rate a resource after borrowing
(define-public (rate-resource
  (resource-id uint)
  (rating uint)
  (review (string-utf8 300)))
  (let (
    (resource (unwrap! (map-get? resources resource-id) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (and (> rating u0) (<= rating u5)) ERR_INVALID_RESOURCE)
    (asserts! (not (is-eq tx-sender (get owner resource))) ERR_NOT_AUTHORIZED)
    
    ;; Record the rating
    (map-set resource-ratings
      { resource-id: resource-id, rater: tx-sender }
      {
        rating: rating,
        review: review,
        timestamp: stacks-block-height
      }
    )
    
    ;; Update resource average rating (simplified)
    (map-set resources resource-id
      (merge resource {
        average-rating: rating, ;; Simplified - would calculate average in real implementation
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Update resource availability status
(define-public (update-availability
  (resource-id uint)
  (new-status (string-ascii 20)))
  (let (
    (resource (unwrap! (map-get? resources resource-id) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner resource)) ERR_NOT_AUTHORIZED)
    (asserts! (is-valid-status new-status) ERR_INVALID_RESOURCE)
    
    (map-set resources resource-id
      (merge resource {
        availability-status: new-status,
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Create a new resource category
(define-public (create-category
  (name (string-ascii 50))
  (description (string-utf8 300)))
  (let (
    (category-id (+ (var-get category-counter) u1))
  )
    (asserts! (> (len name) u0) ERR_INVALID_RESOURCE)
    (asserts! (is-none (map-get? resource-categories name)) ERR_ALREADY_EXISTS)
    
    (map-set resource-categories name {
      category-id: category-id,
      name: name,
      description: description,
      resource-count: u0,
      is-active: true
    })
    
    (var-set category-counter category-id)
    (ok category-id)
  )
)

;; Submit a resource request
(define-public (request-resource
  (resource-title (string-utf8 200))
  (description (string-utf8 500))
  (category (string-ascii 50))
  (max-value uint))
  (let (
    (request-id (+ (var-get resource-counter) u1000)) ;; Use different counter range
  )
    (asserts! (> (len resource-title) u0) ERR_INVALID_RESOURCE)
    
    (map-set resource-requests request-id {
      requester: tx-sender,
      resource-title: resource-title,
      description: description,
      category: category,
      max-value: max-value,
      request-date: stacks-block-height,
      fulfilled: false
    })
    
    (ok request-id)
  )
)

;; Deactivate a resource
(define-public (deactivate-resource (resource-id uint))
  (let (
    (resource (unwrap! (map-get? resources resource-id) ERR_RESOURCE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner resource)) ERR_NOT_AUTHORIZED)
    
    (map-set resources resource-id
      (merge resource { is-active: false })
    )
    
    ;; Update owner statistics
    (let (
      (current-stats (unwrap-panic (map-get? owner-resources tx-sender)))
    )
      (map-set owner-resources tx-sender
        (merge current-stats {
          active-resources: (- (get active-resources current-stats) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; read only functions

;; Get resource details
(define-read-only (get-resource (resource-id uint))
  (map-get? resources resource-id)
)

;; Get resource rating
(define-read-only (get-resource-rating (resource-id uint) (rater principal))
  (map-get? resource-ratings { resource-id: resource-id, rater: rater })
)

;; Get category information
(define-read-only (get-category (category-name (string-ascii 50)))
  (map-get? resource-categories category-name)
)

;; Get owner statistics
(define-read-only (get-owner-stats (owner principal))
  (map-get? owner-resources owner)
)

;; Get resource request
(define-read-only (get-resource-request (request-id uint))
  (map-get? resource-requests request-id)
)

;; Get total resource count
(define-read-only (get-resource-count)
  (var-get resource-counter)
)

;; Check if resource is available
(define-read-only (is-resource-available (resource-id uint))
  (match (map-get? resources resource-id)
    resource
      (and
        (get is-active resource)
        (is-eq (get availability-status resource) "available")
      )
    false
  )
)

;; Get resources by category (simplified)
(define-read-only (get-category-resource-count (category (string-ascii 50)))
  (match (map-get? resource-categories category)
    cat-info (get resource-count cat-info)
    u0
  )
)

;; private functions

;; Update category resource count
(define-private (update-category-count (category (string-ascii 50)) (increment uint))
  (match (map-get? resource-categories category)
    category-info
      (map-set resource-categories category
        (merge category-info {
          resource-count: (+ (get resource-count category-info) increment)
        })
      )
    ;; Create category if it doesn't exist
    (map-set resource-categories category {
      category-id: (+ (var-get category-counter) u1),
      name: category,
      description: u"Auto-created category",
      resource-count: increment,
      is-active: true
    })
  )
)

;; Validate availability status
(define-private (is-valid-status (status (string-ascii 20)))
  (or
    (is-eq status "available")
    (is-eq status "borrowed")
    (is-eq status "maintenance")
    (is-eq status "reserved")
  )
)

;; Validate resource type
(define-private (is-valid-resource-type (resource-type (string-ascii 30)))
  (or
    (is-eq resource-type "book")
    (is-eq resource-type "tool")
    (is-eq resource-type "equipment")
    (is-eq resource-type "digital")
    (is-eq resource-type "media")
    (is-eq resource-type "other")
  )
)


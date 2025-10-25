;; possession-caretaker

;; ========================================
;; SYSTEM ADMINISTRATION
;; ========================================
(define-constant CONTRACT_ADMINISTRATOR tx-sender)

;; ========================================
;; COUNTER STORAGE
;; ========================================
(define-data-var item-count uint u0)

;; ========================================
;; CORE DATA STRUCTURES
;; ========================================
(define-map chronicle-storage
  { item-id: uint }
  {
    item-name: (string-ascii 64),
    owner-address: principal,
    mass-value: uint,
    registration-height: uint,
    source-location: (string-ascii 32),
    description-text: (string-ascii 128),
    category-list: (list 10 (string-ascii 32))
  }
)

(define-map access-control-list
  { item-id: uint, viewer-address: principal }
  {
    can-view: bool,
    granted-by: principal,
    granted-at-height: uint
  }
)

(define-map user-wishlist
  { viewer-address: principal, item-id: uint }
  {
    added-at-height: uint,
    updated-at-height: uint
  }
)


;; ========================================
;; ERROR CODE DEFINITIONS
;; ========================================
(define-constant ERR_ITEM_NOT_FOUND (err u301))
(define-constant ERR_ITEM_ALREADY_EXISTS (err u302))
(define-constant ERR_INVALID_NAME (err u303))
(define-constant ERR_INVALID_MASS (err u304))
(define-constant ERR_UNAUTHORIZED (err u305))
(define-constant ERR_INVALID_SOURCE (err u306))
(define-constant ERR_FORBIDDEN_ACTION (err u307))
(define-constant ERR_VIEW_DENIED (err u308))
(define-constant ERR_SECURITY_VIOLATION (err u309))
(define-constant ERR_INVALID_OWNER (err u310))
(define-constant ERR_WISHLIST_DUPLICATE (err u311))
(define-constant ERR_WISHLIST_NOT_FOUND (err u312))

;; ========================================
;; PUBLIC FUNCTIONS - ITEM MANAGEMENT
;; ========================================
(define-public (create-new-item (name (string-ascii 64)) (mass uint) (source (string-ascii 32)) 
                                (description (string-ascii 128)) (categories (list 10 (string-ascii 32))))
  (let
    (
      (new-id (+ (var-get item-count) u1))
    )
    (asserts! (> (len name) u0) ERR_INVALID_NAME)
    (asserts! (< (len name) u65) ERR_INVALID_NAME)
    (asserts! (> mass u0) ERR_INVALID_MASS)
    (asserts! (< mass u1000000000) ERR_INVALID_MASS)
    (asserts! (> (len source) u0) ERR_INVALID_SOURCE)
    (asserts! (< (len source) u33) ERR_INVALID_SOURCE)
    (asserts! (> (len description) u0) ERR_INVALID_NAME)
    (asserts! (< (len description) u129) ERR_INVALID_NAME)
    (asserts! (validate-category-list categories) ERR_INVALID_NAME)

    (map-insert chronicle-storage
      { item-id: new-id }
      {
        item-name: name,
        owner-address: tx-sender,
        mass-value: mass,
        registration-height: block-height,
        source-location: source,
        description-text: description,
        category-list: categories
      }
    )

    (map-insert access-control-list
      { item-id: new-id, viewer-address: tx-sender }
      { 
        can-view: true,
        granted-by: tx-sender,
        granted-at-height: block-height
      }
    )
    (var-set item-count new-id)
    (ok new-id)
  )
)

(define-public (update-item-details (item-id uint) (name (string-ascii 64)) (mass uint) 
                                    (source (string-ascii 32)) (description (string-ascii 128)) 
                                    (categories (list 10 (string-ascii 32))))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (is-eq (get owner-address record) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_NAME)
    (asserts! (< (len name) u65) ERR_INVALID_NAME)
    (asserts! (> mass u0) ERR_INVALID_MASS)
    (asserts! (< mass u1000000000) ERR_INVALID_MASS)
    (asserts! (> (len source) u0) ERR_INVALID_SOURCE)
    (asserts! (< (len source) u33) ERR_INVALID_SOURCE)
    (asserts! (> (len description) u0) ERR_INVALID_NAME)
    (asserts! (< (len description) u129) ERR_INVALID_NAME)
    (asserts! (validate-category-list categories) ERR_INVALID_NAME)

    (map-set chronicle-storage
      { item-id: item-id }
      (merge record { 
        item-name: name, 
        mass-value: mass, 
        source-location: source, 
        description-text: description, 
        category-list: categories 
      })
    )
    (ok true)
  )
)

(define-public (transfer-item-ownership (item-id uint) (new-owner principal))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-is-owner item-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq new-owner tx-sender)) ERR_SECURITY_VIOLATION)
    (asserts! (check-valid-principal new-owner) ERR_INVALID_OWNER)

    (map-set chronicle-storage
      { item-id: item-id }
      (merge record { owner-address: new-owner })
    )

    (map-set access-control-list
      { item-id: item-id, viewer-address: new-owner }
      {
        can-view: true,
        granted-by: tx-sender,
        granted-at-height: block-height
      }
    )
    (ok true)
  )
)

(define-public (delete-item (item-id uint))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (is-eq (get owner-address record) tx-sender) ERR_UNAUTHORIZED)

    (map-delete chronicle-storage { item-id: item-id })
    (ok true)
  )
)

;; ========================================
;; PUBLIC FUNCTIONS - ACCESS CONTROL
;; ========================================
(define-public (grant-view-permission (item-id uint) (viewer principal))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-is-owner item-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq viewer tx-sender)) ERR_SECURITY_VIOLATION)

    (map-set access-control-list
      { item-id: item-id, viewer-address: viewer }
      { 
        can-view: true,
        granted-by: tx-sender,
        granted-at-height: block-height
      }
    )
    (ok true)
  )
)

(define-public (revoke-view-permission (item-id uint) (viewer principal))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (access-record (unwrap! (map-get? access-control-list { item-id: item-id, viewer-address: viewer }) ERR_UNAUTHORIZED))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-is-owner item-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq viewer tx-sender)) ERR_SECURITY_VIOLATION)

    (map-delete access-control-list { item-id: item-id, viewer-address: viewer })
    (if (check-in-wishlist item-id viewer)
      (map-delete user-wishlist { viewer-address: viewer, item-id: item-id })
      true
    )
    (ok true)
  )
)

(define-public (remove-all-view-permissions (item-id uint))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-is-owner item-id tx-sender) ERR_UNAUTHORIZED)

    (map-set access-control-list
      { item-id: item-id, viewer-address: tx-sender }
      { 
        can-view: true,
        granted-by: tx-sender,
        granted-at-height: block-height
      }
    )
    (ok true)
  )
)

;; ========================================
;; PUBLIC FUNCTIONS - WISHLIST
;; ========================================
(define-public (add-to-wishlist (item-id uint))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-has-view-access item-id tx-sender) ERR_VIEW_DENIED)
    (asserts! (not (check-in-wishlist item-id tx-sender)) ERR_WISHLIST_DUPLICATE)

    (map-insert user-wishlist
      { viewer-address: tx-sender, item-id: item-id }
      {
        added-at-height: block-height,
        updated-at-height: block-height
      }
    )
    (ok true)
  )
)

(define-public (remove-from-wishlist (item-id uint))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-in-wishlist item-id tx-sender) ERR_WISHLIST_NOT_FOUND)

    (map-delete user-wishlist { viewer-address: tx-sender, item-id: item-id })
    (ok true)
  )
)

;; ========================================
;; PUBLIC FUNCTIONS - ADVANCED SECURITY
;; ========================================
(define-public (perform-multilayer-check (item-id uint) (code uint) (op-type (string-ascii 32)))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (access-record (unwrap! (map-get? access-control-list { item-id: item-id, viewer-address: tx-sender }) ERR_VIEW_DENIED))
      (hash-value (+ item-id block-height (get mass-value record)))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-has-view-access item-id tx-sender) ERR_VIEW_DENIED)
    (asserts! (> (len op-type) u0) ERR_INVALID_NAME)
    (asserts! (< (len op-type) u33) ERR_INVALID_NAME)
    (asserts! (> code u0) ERR_SECURITY_VIOLATION)

    (asserts! (is-eq (mod hash-value u1000) (mod code u1000)) ERR_SECURITY_VIOLATION)

    (map-set access-control-list
      { item-id: item-id, viewer-address: tx-sender }
      (merge access-record { granted-at-height: block-height })
    )

    (ok hash-value)
  )
)

(define-public (create-audit-log (item-id uint) (category (string-ascii 32)) (details (string-ascii 96)))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-has-view-access item-id tx-sender) ERR_VIEW_DENIED)
    (asserts! (> (len category) u0) ERR_INVALID_NAME)
    (asserts! (< (len category) u33) ERR_INVALID_NAME)
    (asserts! (> (len details) u0) ERR_INVALID_NAME)
    (asserts! (< (len details) u97) ERR_INVALID_NAME)

    (map-set access-control-list
      { item-id: item-id, viewer-address: tx-sender }
      { 
        can-view: true,
        granted-by: tx-sender,
        granted-at-height: block-height
      }
    )

    (ok block-height)
  )
)

(define-public (verify-item-integrity (item-id uint))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
      (owner-access (map-get? access-control-list { item-id: item-id, viewer-address: (get owner-address record) }))
    )
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-is-owner item-id tx-sender) ERR_UNAUTHORIZED)

    (asserts! (is-some owner-access) ERR_VIEW_DENIED)
    (asserts! (and (> (len (get item-name record)) u0) (< (len (get item-name record)) u65)) ERR_INVALID_NAME)
    (asserts! (and (> (get mass-value record) u0) (< (get mass-value record) u1000000000)) ERR_INVALID_MASS)
    (asserts! (and (> (len (get source-location record)) u0) (< (len (get source-location record)) u33)) ERR_INVALID_SOURCE)
    (asserts! (and (> (len (get description-text record)) u0) (< (len (get description-text record)) u129)) ERR_INVALID_NAME)
    (asserts! (validate-category-list (get category-list record)) ERR_INVALID_NAME)

    (ok true)
  )
)

(define-public (emergency-ownership-transfer (item-id uint) (new-owner principal) (justification (string-ascii 64)))
  (let
    (
      (record (unwrap! (map-get? chronicle-storage { item-id: item-id }) ERR_ITEM_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_ADMINISTRATOR) ERR_UNAUTHORIZED)
    (asserts! (check-item-exists item-id) ERR_ITEM_NOT_FOUND)
    (asserts! (check-valid-principal new-owner) ERR_INVALID_OWNER)
    (asserts! (> (len justification) u0) ERR_INVALID_NAME)
    (asserts! (< (len justification) u65) ERR_INVALID_NAME)
    (asserts! (not (is-eq new-owner (get owner-address record))) ERR_SECURITY_VIOLATION)

    (map-set chronicle-storage
      { item-id: item-id }
      (merge record { owner-address: new-owner })
    )

    (map-set access-control-list
      { item-id: item-id, viewer-address: new-owner }
      {
        can-view: true,
        granted-by: CONTRACT_ADMINISTRATOR,
        granted-at-height: block-height
      }
    )
    (ok true)
  )
)


;; ========================================
;; VALIDATION HELPERS
;; ========================================
(define-private (validate-single-category (cat (string-ascii 32)))
  (and 
    (> (len cat) u0)
    (< (len cat) u33)
  )
)

(define-private (validate-category-list (cats (list 10 (string-ascii 32))))
  (and
    (> (len cats) u0)
    (<= (len cats) u10)
    (is-eq (len (filter validate-single-category cats)) (len cats))
  )
)

(define-private (check-item-exists (item-id uint))
  (is-some (map-get? chronicle-storage { item-id: item-id }))
)

(define-private (check-is-owner (item-id uint) (addr principal))
  (match (map-get? chronicle-storage { item-id: item-id })
    record (is-eq (get owner-address record) addr)
    false
  )
)

(define-private (check-valid-principal (addr principal))
  (not (is-eq addr 'ST000000000000000000002AMW42H))
)

(define-private (check-has-view-access (item-id uint) (addr principal))
  (match (map-get? access-control-list { item-id: item-id, viewer-address: addr })
    access-record (get can-view access-record)
    false
  )
)

(define-private (check-in-wishlist (item-id uint) (addr principal))
  (is-some (map-get? user-wishlist { viewer-address: addr, item-id: item-id }))
)

(define-private (get-item-mass (item-id uint))
  (default-to u0 
    (get mass-value 
      (map-get? chronicle-storage { item-id: item-id })
    )
  )
)



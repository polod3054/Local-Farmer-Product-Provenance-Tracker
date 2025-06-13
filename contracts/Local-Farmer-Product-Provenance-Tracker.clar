(define-non-fungible-token produce-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-batch (err u103))

(define-data-var next-batch-id uint u1)

(define-map batches uint {
    farmer: principal,
    product-name: (string-ascii 64),
    harvest-date: uint,
    latitude: int,
    longitude: int,
    farming-method: (string-ascii 64),
    quantity: uint,
    status: (string-ascii 20)
})

(define-map transport-events uint 
    {
        timestamp: uint,
        location: (string-ascii 64),
        handler: principal,
        temperature: int
    }
)

(define-public (register-batch (product-name (string-ascii 64)) (harvest-date uint) (latitude int) (longitude int) (farming-method (string-ascii 64)) (quantity uint))
    (let ((batch-id (var-get next-batch-id)))
        (try! (nft-mint? produce-batch batch-id tx-sender))
        (map-set batches batch-id {
            farmer: tx-sender,
            product-name: product-name,
            harvest-date: harvest-date,
            latitude: latitude,
            longitude: longitude,
            farming-method: farming-method,
            quantity: quantity,
            status: "harvested"
        })
        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)))

(define-public (add-transport-event (batch-id uint) (location (string-ascii 64)) (temperature int))
    (begin
        (map-set transport-events batch-id {
            timestamp: stacks-block-height,
            location: location,
            handler: tx-sender,
            temperature: temperature
        })
        (ok true)))

(define-public (update-batch-status (batch-id uint) (new-status (string-ascii 20)))
    (let ((batch (map-get? batches batch-id)))
        (match batch
            batch-data (begin
                (asserts! (is-eq (get farmer batch-data) tx-sender) err-owner-only)
                (map-set batches batch-id (merge batch-data {status: new-status}))
                (ok true))
            err-not-found)))

(define-read-only (get-batch-details (batch-id uint))
    (ok (map-get? batches batch-id)))

(define-read-only (get-transport-history (batch-id uint))
    (ok (map-get? transport-events batch-id)))

(define-read-only (verify-batch-ownership (batch-id uint) (owner principal))
    (ok (is-eq (some owner) (nft-get-owner? produce-batch batch-id))))

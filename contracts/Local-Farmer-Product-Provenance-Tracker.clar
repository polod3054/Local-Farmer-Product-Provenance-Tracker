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

(define-constant err-not-inspector (err u104))
(define-constant err-already-certified (err u105))

(define-data-var inspector-count uint u0)

(define-map inspectors principal bool)

(define-map certifications uint {
    inspector: principal,
    certification-type: (string-ascii 32),
    grade: (string-ascii 16),
    certification-date: uint,
    expiry-date: uint,
    notes: (string-ascii 128)
})

(define-public (add-inspector (inspector principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set inspectors inspector true)
        (var-set inspector-count (+ (var-get inspector-count) u1))
        (ok true)))

(define-public (remove-inspector (inspector principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete inspectors inspector)
        (ok true)))

(define-public (certify-batch (batch-id uint) (certification-type (string-ascii 32)) (grade (string-ascii 16)) (expiry-date uint) (notes (string-ascii 128)))
    (let ((batch (map-get? batches batch-id)))
        (begin
            (asserts! (default-to false (map-get? inspectors tx-sender)) err-not-inspector)
            (asserts! (is-some batch) err-not-found)
            (asserts! (is-none (map-get? certifications batch-id)) err-already-certified)
            (map-set certifications batch-id {
                inspector: tx-sender,
                certification-type: certification-type,
                grade: grade,
                certification-date: stacks-block-height,
                expiry-date: expiry-date,
                notes: notes
            })
            (ok true))))

(define-read-only (get-certification (batch-id uint))
    (ok (map-get? certifications batch-id)))

(define-read-only (is-inspector (inspector principal))
    (ok (default-to false (map-get? inspectors inspector))))

(define-read-only (is-certification-valid (batch-id uint))
    (match (map-get? certifications batch-id)
        cert (ok (> (get expiry-date cert) stacks-block-height))
        (ok false)))

        (define-constant err-not-for-sale (err u106))
(define-constant err-insufficient-payment (err u107))
(define-constant err-already-listed (err u108))

(define-map marketplace-listings uint {
    seller: principal,
    price-per-unit: uint,
    available-quantity: uint,
    listing-date: uint,
    is-active: bool
})

(define-map sales-history uint {
    buyer: principal,
    seller: principal,
    quantity-sold: uint,
    total-price: uint,
    sale-date: uint
})

(define-data-var total-sales uint u0)

(define-public (list-batch-for-sale (batch-id uint) (price-per-unit uint))
    (let ((batch (map-get? batches batch-id)))
        (match batch
            batch-data (begin
                (asserts! (is-eq (get farmer batch-data) tx-sender) err-owner-only)
                (asserts! (is-none (map-get? marketplace-listings batch-id)) err-already-listed)
                (map-set marketplace-listings batch-id {
                    seller: tx-sender,
                    price-per-unit: price-per-unit,
                    available-quantity: (get quantity batch-data),
                    listing-date: stacks-block-height,
                    is-active: true
                })
                (ok true))
            err-not-found)))

(define-public (purchase-batch (batch-id uint) (quantity uint))
    (let (
        (listing (map-get? marketplace-listings batch-id))
        (batch (map-get? batches batch-id))
    )
        (match listing
            listing-data (match batch
                batch-data (let (
                    (total-cost (* (get price-per-unit listing-data) quantity))
                    (remaining-quantity (- (get available-quantity listing-data) quantity))
                )
                    (begin
                        (asserts! (get is-active listing-data) err-not-for-sale)
                        (asserts! (>= (get available-quantity listing-data) quantity) err-invalid-batch)
                        (try! (stx-transfer? total-cost tx-sender (get seller listing-data)))
                        (try! (nft-transfer? produce-batch batch-id (get seller listing-data) tx-sender))
                        (map-set sales-history batch-id {
                            buyer: tx-sender,
                            seller: (get seller listing-data),
                            quantity-sold: quantity,
                            total-price: total-cost,
                            sale-date: stacks-block-height
                        })
                        (if (is-eq remaining-quantity u0)
                            (map-set marketplace-listings batch-id (merge listing-data {is-active: false, available-quantity: u0}))
                            (map-set marketplace-listings batch-id (merge listing-data {available-quantity: remaining-quantity})))
                        (var-set total-sales (+ (var-get total-sales) u1))
                        (ok true)))
                err-not-found)
            err-not-for-sale)))

(define-public (remove-listing (batch-id uint))
    (let ((listing (map-get? marketplace-listings batch-id)))
        (match listing
            listing-data (begin
                (asserts! (is-eq (get seller listing-data) tx-sender) err-owner-only)
                (map-set marketplace-listings batch-id (merge listing-data {is-active: false}))
                (ok true))
            err-not-found)))

(define-read-only (get-listing (batch-id uint))
    (ok (map-get? marketplace-listings batch-id)))

(define-read-only (get-sale-history (batch-id uint))
    (ok (map-get? sales-history batch-id)))

(define-read-only (get-total-sales)
    (ok (var-get total-sales)))

(define-constant err-verification-exists (err u109))
(define-constant err-verification-not-found (err u110))



(define-map verification-codes (string-ascii 16) uint)
(define-map batch-verification-codes uint (string-ascii 16))

(define-map verification-attempts uint {
    verifier: principal,
    verification-date: uint,
    success: bool
})

(define-data-var total-verifications uint u0)

(define-private (generate-verification-code (batch-id uint))
    "VRF001BATCH001XY")

(define-public (generate-batch-verification (batch-id uint))
    (let (
        (batch (map-get? batches batch-id))
        (existing-code (map-get? batch-verification-codes batch-id))
    )
        (match batch
            batch-data (begin
                (asserts! (is-eq (get farmer batch-data) tx-sender) err-owner-only)
                (asserts! (is-none existing-code) err-verification-exists)
                (let ((verification-code (generate-verification-code batch-id)))
                    (map-set verification-codes verification-code batch-id)
                    (map-set batch-verification-codes batch-id verification-code)
                    (ok verification-code)))
            err-not-found)))

(define-public (verify-product (verification-code (string-ascii 16)))
    (let ((batch-id (map-get? verification-codes verification-code)))
        (match batch-id
            found-batch-id (let (
                (verification-id (var-get total-verifications))
                (batch-data (map-get? batches found-batch-id))
            )
                (map-set verification-attempts verification-id {
                    verifier: tx-sender,
                    verification-date: stacks-block-height,
                    success: true
                })
                (var-set total-verifications (+ (var-get total-verifications) u1))
                (ok {
                    batch-id: found-batch-id,
                    batch-data: batch-data,
                    verification-successful: true
                }))
            (begin
                (let ((verification-id (var-get total-verifications)))
                    (map-set verification-attempts verification-id {
                        verifier: tx-sender,
                        verification-date: stacks-block-height,
                        success: false
                    })
                    (var-set total-verifications (+ (var-get total-verifications) u1))
                    err-verification-not-found)))))

(define-read-only (get-batch-verification-code (batch-id uint))
    (ok (map-get? batch-verification-codes batch-id)))

(define-read-only (get-verification-stats)
    (ok (var-get total-verifications)))

(define-read-only (get-verification-attempt (verification-id uint))
    (ok (map-get? verification-attempts verification-id)))
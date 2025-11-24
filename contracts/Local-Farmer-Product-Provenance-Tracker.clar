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

(define-constant err-score-exists (err u111))
(define-constant err-invalid-score (err u112))

(define-map quality-scores uint {
    base-score: uint,
    farming-bonus: uint,
    transport-bonus: uint,
    certification-bonus: uint,
    total-score: uint,
    calculated-date: uint
})

(define-map farming-method-scores (string-ascii 64) uint)
(define-data-var quality-calculations uint u0)

(define-private (init-farming-scores)
    (begin
        (map-set farming-method-scores "organic" u30)
        (map-set farming-method-scores "conventional" u15)
        (map-set farming-method-scores "hydroponic" u25)
        (map-set farming-method-scores "permaculture" u35)
        (map-set farming-method-scores "biodynamic" u40)
        true))

(define-private (calculate-farming-score (farming-method (string-ascii 64)))
    (default-to u10 (map-get? farming-method-scores farming-method)))

(define-private (calculate-transport-score (batch-id uint))
    (match (map-get? transport-events batch-id)
        transport-data (let ((temp (get temperature transport-data)))
            (if (and (>= temp 32) (<= temp 40))
                u25
                (if (and (>= temp 25) (<= temp 45))
                    u15
                    u5)))
        u0))

(define-private (calculate-certification-score (batch-id uint))
    (match (map-get? certifications batch-id)
        cert-data (let ((grade (get grade cert-data)))
            (if (is-eq grade "A+")
                u30
                (if (is-eq grade "A")
                    u25
                    (if (is-eq grade "B+")
                        u20
                        (if (is-eq grade "B")
                            u15
                            u10)))))
        u0))

(define-public (calculate-quality-score (batch-id uint))
    (let (
        (batch (map-get? batches batch-id))
        (existing-score (map-get? quality-scores batch-id))
    )
        (match batch
            batch-data (begin
                (asserts! (is-none existing-score) err-score-exists)
                (let (
                    (base-score u40)
                    (farming-bonus (calculate-farming-score (get farming-method batch-data)))
                    (transport-bonus (calculate-transport-score batch-id))
                    (certification-bonus (calculate-certification-score batch-id))
                )
                    (let ((total-score (+ (+ (+ base-score farming-bonus) transport-bonus) certification-bonus)))
                        (map-set quality-scores batch-id {
                            base-score: base-score,
                            farming-bonus: farming-bonus,
                            transport-bonus: transport-bonus,
                            certification-bonus: certification-bonus,
                            total-score: (if (> total-score u100) u100 total-score),
                            calculated-date: stacks-block-height
                        })
                        (var-set quality-calculations (+ (var-get quality-calculations) u1))
                        (ok (if (> total-score u100) u100 total-score)))))
            err-not-found)))

(define-public (recalculate-quality-score (batch-id uint))
    (let (
        (batch (map-get? batches batch-id))
        (existing-score (map-get? quality-scores batch-id))
    )
        (match batch
            batch-data (begin
                (asserts! (is-eq (get farmer batch-data) tx-sender) err-owner-only)
                (let (
                    (base-score u40)
                    (farming-bonus (calculate-farming-score (get farming-method batch-data)))
                    (transport-bonus (calculate-transport-score batch-id))
                    (certification-bonus (calculate-certification-score batch-id))
                )
                    (let ((total-score (+ (+ (+ base-score farming-bonus) transport-bonus) certification-bonus)))
                        (map-set quality-scores batch-id {
                            base-score: base-score,
                            farming-bonus: farming-bonus,
                            transport-bonus: transport-bonus,
                            certification-bonus: certification-bonus,
                            total-score: (if (> total-score u100) u100 total-score),
                            calculated-date: stacks-block-height
                        })
                        (var-set quality-calculations (+ (var-get quality-calculations) u1))
                        (ok (if (> total-score u100) u100 total-score)))))
            err-not-found)))

(define-read-only (get-quality-score (batch-id uint))
    (ok (map-get? quality-scores batch-id)))

(define-read-only (get-quality-breakdown (batch-id uint))
    (ok (map-get? quality-scores batch-id)))

(define-read-only (get-farming-method-score (farming-method (string-ascii 64)))
    (ok (default-to u10 (map-get? farming-method-scores farming-method))))

(define-read-only (get-quality-calculation-stats)
    (ok (var-get quality-calculations)))

(init-farming-scores)

(define-constant err-already-reviewed (err u113))
(define-constant err-cannot-review-own-batch (err u114))
(define-constant err-invalid-rating (err u115))
(define-constant err-not-buyer (err u116))

(define-map product-reviews {batch-id: uint, reviewer: principal} {
    rating: uint,
    comment: (string-ascii 256),
    review-date: uint,
    verified-purchase: bool
})

(define-map farmer-reputation principal {
    total-reviews: uint,
    total-rating-sum: uint,
    average-rating: uint,
    five-star-count: uint,
    four-star-count: uint,
    three-star-count: uint,
    two-star-count: uint,
    one-star-count: uint
})

(define-map batch-review-count uint uint)
(define-data-var total-reviews-submitted uint u0)

(define-private (is-verified-buyer (batch-id uint) (buyer principal))
    (match (map-get? sales-history batch-id)
        sale-data (is-eq (get buyer sale-data) buyer)
        false))

(define-private (update-farmer-reputation (farmer principal) (rating uint))
    (let (
        (current-rep (default-to 
            {total-reviews: u0, total-rating-sum: u0, average-rating: u0, five-star-count: u0, four-star-count: u0, three-star-count: u0, two-star-count: u0, one-star-count: u0}
            (map-get? farmer-reputation farmer)))
        (new-total-reviews (+ (get total-reviews current-rep) u1))
        (new-rating-sum (+ (get total-rating-sum current-rep) rating))
        (new-average (/ new-rating-sum new-total-reviews))
    )
        (map-set farmer-reputation farmer {
            total-reviews: new-total-reviews,
            total-rating-sum: new-rating-sum,
            average-rating: new-average,
            five-star-count: (if (is-eq rating u5) (+ (get five-star-count current-rep) u1) (get five-star-count current-rep)),
            four-star-count: (if (is-eq rating u4) (+ (get four-star-count current-rep) u1) (get four-star-count current-rep)),
            three-star-count: (if (is-eq rating u3) (+ (get three-star-count current-rep) u1) (get three-star-count current-rep)),
            two-star-count: (if (is-eq rating u2) (+ (get two-star-count current-rep) u1) (get two-star-count current-rep)),
            one-star-count: (if (is-eq rating u1) (+ (get one-star-count current-rep) u1) (get one-star-count current-rep))
        })
        true))

(define-public (submit-review (batch-id uint) (rating uint) (comment (string-ascii 256)))
    (let (
        (batch (map-get? batches batch-id))
        (existing-review (map-get? product-reviews {batch-id: batch-id, reviewer: tx-sender}))
    )
        (match batch
            batch-data (begin
                (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
                (asserts! (not (is-eq (get farmer batch-data) tx-sender)) err-cannot-review-own-batch)
                (asserts! (is-none existing-review) err-already-reviewed)
                (let ((verified-purchase (is-verified-buyer batch-id tx-sender)))
                    (map-set product-reviews {batch-id: batch-id, reviewer: tx-sender} {
                        rating: rating,
                        comment: comment,
                        review-date: stacks-block-height,
                        verified-purchase: verified-purchase
                    })
                    (update-farmer-reputation (get farmer batch-data) rating)
                    (map-set batch-review-count batch-id (+ (default-to u0 (map-get? batch-review-count batch-id)) u1))
                    (var-set total-reviews-submitted (+ (var-get total-reviews-submitted) u1))
                    (ok true)))
            err-not-found)))

(define-read-only (get-review (batch-id uint) (reviewer principal))
    (ok (map-get? product-reviews {batch-id: batch-id, reviewer: reviewer})))

(define-read-only (get-farmer-reputation (farmer principal))
    (ok (map-get? farmer-reputation farmer)))

(define-read-only (get-batch-review-count (batch-id uint))
    (ok (default-to u0 (map-get? batch-review-count batch-id))))

(define-read-only (get-total-reviews)
    (ok (var-get total-reviews-submitted)))

(define-constant err-already-recalled (err u117))
(define-constant err-invalid-severity (err u118))
(define-constant err-unauthorized-recall (err u119))
(define-constant err-recall-not-found (err u120))

(define-map batch-recalls uint {
    issuer: principal,
    issuer-type: (string-ascii 16),
    severity: (string-ascii 16),
    reason: (string-ascii 256),
    affected-quantity: uint,
    recall-date: uint,
    status: (string-ascii 16),
    resolution-date: (optional uint),
    resolution-notes: (optional (string-ascii 256))
})

(define-map recall-notifications {batch-id: uint, notified-party: principal} {
    notification-date: uint,
    acknowledged: bool,
    acknowledgment-date: (optional uint)
})

(define-data-var total-recalls uint u0)
(define-data-var active-recalls uint u0)

(define-private (is-authorized-for-recall (batch-id uint) (issuer principal))
    (let ((batch (map-get? batches batch-id)))
        (match batch
            batch-data (or 
                (is-eq (get farmer batch-data) issuer)
                (default-to false (map-get? inspectors issuer)))
            false)))

(define-private (is-valid-severity (severity (string-ascii 16)))
    (or (or (is-eq severity "critical") (is-eq severity "high"))
        (or (is-eq severity "medium") (is-eq severity "low"))))

(define-public (issue-recall (batch-id uint) (severity (string-ascii 16)) (reason (string-ascii 256)) (affected-quantity uint))
    (let (
        (batch (map-get? batches batch-id))
        (existing-recall (map-get? batch-recalls batch-id))
    )
        (match batch
            batch-data (begin
                (asserts! (is-authorized-for-recall batch-id tx-sender) err-unauthorized-recall)
                (asserts! (is-none existing-recall) err-already-recalled)
                (asserts! (is-valid-severity severity) err-invalid-severity)
                (let (
                    (issuer-type (if (default-to false (map-get? inspectors tx-sender)) "inspector" "farmer"))
                )
                    (map-set batch-recalls batch-id {
                        issuer: tx-sender,
                        issuer-type: issuer-type,
                        severity: severity,
                        reason: reason,
                        affected-quantity: affected-quantity,
                        recall-date: stacks-block-height,
                        status: "active",
                        resolution-date: none,
                        resolution-notes: none
                    })
                    (var-set total-recalls (+ (var-get total-recalls) u1))
                    (var-set active-recalls (+ (var-get active-recalls) u1))
                    (ok true)))
            err-not-found)))

(define-public (resolve-recall (batch-id uint) (resolution-notes (string-ascii 256)))
    (let ((recall (map-get? batch-recalls batch-id)))
        (match recall
            recall-data (begin
                (asserts! (is-eq (get issuer recall-data) tx-sender) err-owner-only)
                (asserts! (is-eq (get status recall-data) "active") err-invalid-batch)
                (map-set batch-recalls batch-id (merge recall-data {
                    status: "resolved",
                    resolution-date: (some stacks-block-height),
                    resolution-notes: (some resolution-notes)
                }))
                (var-set active-recalls (- (var-get active-recalls) u1))
                (ok true))
            err-recall-not-found)))

(define-public (acknowledge-recall (batch-id uint))
    (let (
        (recall (map-get? batch-recalls batch-id))
        (notification-key {batch-id: batch-id, notified-party: tx-sender})
        (existing-notification (map-get? recall-notifications notification-key))
    )
        (match recall
            recall-data (begin
                (asserts! (is-some recall) err-recall-not-found)
                (match existing-notification
                    notif-data (begin
                        (map-set recall-notifications notification-key (merge notif-data {
                            acknowledged: true,
                            acknowledgment-date: (some stacks-block-height)
                        }))
                        (ok true))
                    (begin
                        (map-set recall-notifications notification-key {
                            notification-date: stacks-block-height,
                            acknowledged: true,
                            acknowledgment-date: (some stacks-block-height)
                        })
                        (ok true))))
            err-recall-not-found)))

(define-read-only (get-recall-info (batch-id uint))
    (ok (map-get? batch-recalls batch-id)))

(define-read-only (get-recall-acknowledgment (batch-id uint) (party principal))
    (ok (map-get? recall-notifications {batch-id: batch-id, notified-party: party})))

(define-read-only (get-recall-stats)
    (ok {
        total-recalls: (var-get total-recalls),
        active-recalls: (var-get active-recalls),
        resolved-recalls: (- (var-get total-recalls) (var-get active-recalls))
    }))

(define-read-only (is-batch-recalled (batch-id uint))
    (match (map-get? batch-recalls batch-id)
        recall-data (ok (is-eq (get status recall-data) "active"))
        (ok false)))

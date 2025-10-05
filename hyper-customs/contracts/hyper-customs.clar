;; HyperCustoms - Blockchain Customs Orchestration Platform
;; A comprehensive smart contract for international trade compliance

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-risk-score (err u106))

;; Shipment status constants
(define-constant status-pending u0)
(define-constant status-in-transit u1)
(define-constant status-customs-review u2)
(define-constant status-cleared u3)
(define-constant status-held u4)
(define-constant status-disputed u5)

;; Data Variables
(define-data-var next-shipment-id uint u1)
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Data Maps

;; Compliance DNA - Digital passport for each shipment
(define-map compliance-dna
    { shipment-id: uint }
    {
        product-classification: (string-ascii 50),
        origin-country: (string-ascii 3),
        destination-country: (string-ascii 3),
        risk-score: uint, ;; 0-100
        certificates: (list 10 (string-ascii 100)),
        harmonized-code: (string-ascii 20),
        created-at: uint,
        updated-at: uint
    }
)

;; Shipment tracking
(define-map shipments
    { shipment-id: uint }
    {
        shipper: principal,
        consignee: principal,
        customs-authority: (optional principal),
        status: uint,
        declared-value: uint,
        calculated-duty: uint,
        duty-paid: bool,
        escrow-amount: uint,
        clearance-date: (optional uint),
        tamper-verified: bool
    }
)

;; IoT sensor data for cargo monitoring
(define-map cargo-monitoring
    { shipment-id: uint, checkpoint-id: uint }
    {
        location: (string-ascii 100),
        temperature: int,
        humidity: uint,
        timestamp: uint,
        tamper-detected: bool,
        validator: principal
    }
)

;; Customs authorities registry
(define-map customs-authorities
    { authority: principal }
    {
        country-code: (string-ascii 3),
        active: bool,
        registered-at: uint
    }
)

;; Dispute records
(define-map disputes
    { shipment-id: uint }
    {
        initiated-by: principal,
        reason: (string-ascii 500),
        status: (string-ascii 20),
        votes-approve: uint,
        votes-reject: uint,
        resolved: bool,
        resolution-date: (optional uint)
    }
)

;; Validator registry for dispute resolution
(define-map validators
    { validator: principal }
    {
        reputation-score: uint,
        total-validations: uint,
        active: bool
    }
)

;; Read-only functions

(define-read-only (get-shipment (shipment-id uint))
    (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-compliance-dna (shipment-id uint))
    (map-get? compliance-dna { shipment-id: shipment-id })
)

(define-read-only (get-cargo-checkpoint (shipment-id uint) (checkpoint-id uint))
    (map-get? cargo-monitoring { shipment-id: shipment-id, checkpoint-id: checkpoint-id })
)

(define-read-only (get-dispute (shipment-id uint))
    (map-get? disputes { shipment-id: shipment-id })
)

(define-read-only (is-customs-authority (authority principal))
    (match (map-get? customs-authorities { authority: authority })
        auth-data (get active auth-data)
        false
    )
)

(define-read-only (calculate-duty (declared-value uint) (risk-score uint))
    (let (
        (base-rate u10) ;; 10% base duty rate
        (risk-multiplier (if (> risk-score u50) u15 u10))
    )
        (/ (* declared-value risk-multiplier) u100)
    )
)

;; Public functions

;; Register a customs authority
(define-public (register-customs-authority (authority principal) (country-code (string-ascii 3)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set customs-authorities
            { authority: authority }
            {
                country-code: country-code,
                active: true,
                registered-at: block-height
            }
        ))
    )
)

;; Register a validator for dispute resolution
(define-public (register-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set validators
            { validator: validator }
            {
                reputation-score: u100,
                total-validations: u0,
                active: true
            }
        ))
    )
)

;; Create a new shipment with Compliance DNA
(define-public (create-shipment
    (consignee principal)
    (product-classification (string-ascii 50))
    (origin-country (string-ascii 3))
    (destination-country (string-ascii 3))
    (harmonized-code (string-ascii 20))
    (certificates (list 10 (string-ascii 100)))
    (declared-value uint)
    (risk-score uint)
)
    (let (
        (shipment-id (var-get next-shipment-id))
        (calculated-duty (calculate-duty declared-value risk-score))
        (escrow-required (+ calculated-duty (/ (* declared-value (var-get platform-fee-percentage)) u100)))
    )
        (asserts! (<= risk-score u100) err-invalid-risk-score)
        (try! (stx-transfer? escrow-required tx-sender (as-contract tx-sender)))
        
        ;; Create Compliance DNA
        (map-set compliance-dna
            { shipment-id: shipment-id }
            {
                product-classification: product-classification,
                origin-country: origin-country,
                destination-country: destination-country,
                risk-score: risk-score,
                certificates: certificates,
                harmonized-code: harmonized-code,
                created-at: block-height,
                updated-at: block-height
            }
        )
        
        ;; Create shipment record
        (map-set shipments
            { shipment-id: shipment-id }
            {
                shipper: tx-sender,
                consignee: consignee,
                customs-authority: none,
                status: status-pending,
                declared-value: declared-value,
                calculated-duty: calculated-duty,
                duty-paid: false,
                escrow-amount: escrow-required,
                clearance-date: none,
                tamper-verified: true
            }
        )
        
        (var-set next-shipment-id (+ shipment-id u1))
        (ok shipment-id)
    )
)

;; Update shipment status
(define-public (update-shipment-status (shipment-id uint) (new-status uint))
    (let (
        (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
    )
        (asserts! 
            (or 
                (is-eq tx-sender (get shipper shipment))
                (is-customs-authority tx-sender)
            )
            err-unauthorized
        )
        (ok (map-set shipments
            { shipment-id: shipment-id }
            (merge shipment { status: new-status })
        ))
    )
)

;; Record IoT sensor checkpoint
(define-public (record-checkpoint
    (shipment-id uint)
    (checkpoint-id uint)
    (location (string-ascii 100))
    (temperature int)
    (humidity uint)
    (tamper-detected bool)
)
    (let (
        (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
    )
        (map-set cargo-monitoring
            { shipment-id: shipment-id, checkpoint-id: checkpoint-id }
            {
                location: location,
                temperature: temperature,
                humidity: humidity,
                timestamp: block-height,
                tamper-detected: tamper-detected,
                validator: tx-sender
            }
        )
        
        ;; Update tamper status if detected
        (if tamper-detected
            (map-set shipments
                { shipment-id: shipment-id }
                (merge shipment { tamper-verified: false })
            )
            true
        )
        (ok true)
    )
)

;; Clear shipment through customs
(define-public (clear-shipment (shipment-id uint))
    (let (
        (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
    )
        (asserts! (is-customs-authority tx-sender) err-unauthorized)
        (asserts! (get tamper-verified shipment) err-invalid-status)
        
        ;; Release escrow minus duty to shipper
        (let (
            (duty-amount (get calculated-duty shipment))
            (refund-amount (- (get escrow-amount shipment) duty-amount))
        )
            (try! (as-contract (stx-transfer? refund-amount tx-sender (get shipper shipment))))
            
            (ok (map-set shipments
                { shipment-id: shipment-id }
                (merge shipment {
                    status: status-cleared,
                    duty-paid: true,
                    clearance-date: (some block-height),
                    customs-authority: (some tx-sender)
                })
            ))
        )
    )
)

;; Initiate a dispute
(define-public (initiate-dispute (shipment-id uint) (reason (string-ascii 500)))
    (let (
        (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) err-not-found))
    )
        (asserts! 
            (or 
                (is-eq tx-sender (get shipper shipment))
                (is-eq tx-sender (get consignee shipment))
            )
            err-unauthorized
        )
        (asserts! (is-none (map-get? disputes { shipment-id: shipment-id })) err-already-exists)
        
        (map-set disputes
            { shipment-id: shipment-id }
            {
                initiated-by: tx-sender,
                reason: reason,
                status: "open",
                votes-approve: u0,
                votes-reject: u0,
                resolved: false,
                resolution-date: none
            }
        )
        
        (update-shipment-status shipment-id status-disputed)
    )
)

;; Vote on dispute (validator consensus)
(define-public (vote-on-dispute (shipment-id uint) (approve bool))
    (let (
        (dispute (unwrap! (map-get? disputes { shipment-id: shipment-id }) err-not-found))
        (validator-data (unwrap! (map-get? validators { validator: tx-sender }) err-unauthorized))
    )
        (asserts! (get active validator-data) err-unauthorized)
        (asserts! (not (get resolved dispute)) err-invalid-status)
        
        (ok (map-set disputes
            { shipment-id: shipment-id }
            (merge dispute {
                votes-approve: (if approve (+ (get votes-approve dispute) u1) (get votes-approve dispute)),
                votes-reject: (if approve (get votes-reject dispute) (+ (get votes-reject dispute) u1))
            })
        ))
    )
)

;; Resolve dispute (requires minimum 3 validator votes)
(define-public (resolve-dispute (shipment-id uint))
    (let (
        (dispute (unwrap! (map-get? disputes { shipment-id: shipment-id }) err-not-found))
        (total-votes (+ (get votes-approve dispute) (get votes-reject dispute)))
    )
        (asserts! (>= total-votes u3) err-invalid-status)
        
        (ok (map-set disputes
            { shipment-id: shipment-id }
            (merge dispute {
                resolved: true,
                status: (if (> (get votes-approve dispute) (get votes-reject dispute)) "approved" "rejected"),
                resolution-date: (some block-height)
            })
        ))
    )
)

;; Update compliance DNA (for regulatory changes)
(define-public (update-compliance-dna 
    (shipment-id uint)
    (risk-score uint)
)
    (let (
        (dna (unwrap! (map-get? compliance-dna { shipment-id: shipment-id }) err-not-found))
    )
        (asserts! (is-customs-authority tx-sender) err-unauthorized)
        (asserts! (<= risk-score u100) err-invalid-risk-score)
        
        (ok (map-set compliance-dna
            { shipment-id: shipment-id }
            (merge dna {
                risk-score: risk-score,
                updated-at: block-height
            })
        ))
    )
)

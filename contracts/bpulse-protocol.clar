;; BPulse Network - Rhythmic Information Exchange Ecosystem

;; Core data storage matrices
(define-map genomic-sequence-holdings principal uint)
(define-map token-holdings principal uint)
(define-map sequence-listings {contributor: principal} {quantity: uint, token-rate: uint})

;; System counters and monitoring
(define-data-var sequence-acquisition-cost uint u200) 
(define-data-var contributor-capacity-ceiling uint u5000) 
(define-data-var protocol-tribute-rate uint u5) 
(define-data-var contribution-reclaim-ratio uint u80) 
(define-data-var ecosystem-capacity-threshold uint u100000) 
(define-data-var active-sequences-counter uint u0) 

;; Protocol-wide constants
(define-constant protocol-steward tx-sender)
(define-constant err-unauthorized-access (err u100))
(define-constant err-insufficient-sequences (err u101))
(define-constant err-invalid-token-rate (err u102))
(define-constant err-invalid-sequence-quantity (err u103))
(define-constant err-invalid-tribute-setting (err u104))
(define-constant err-sequence-transfer-failure (err u105))
(define-constant err-identical-entity (err u106))
(define-constant err-threshold-breach (err u107))
(define-constant err-invalid-threshold (err u108))

;; Internal computational functions

;; Calculate ecosystem tribute amount for governance fund
(define-private (derive-tribute-amount (base-amount uint))
  (/ (* base-amount (var-get protocol-tribute-rate)) u100))

;; Calculate contribution reclaim compensation
(define-private (derive-reclaim-compensation (sequence-count uint))
  (/ (* sequence-count (var-get sequence-acquisition-cost) (var-get contribution-reclaim-ratio)) u100))

;; Adjust ecosystem sequence tracker
(define-private (adjust-sequence-inventory (quantity-change int))
  (let (
    (current-inventory (var-get active-sequences-counter))
    (adjusted-inventory (if (< quantity-change 0)
                         (if (>= current-inventory (to-uint (- 0 quantity-change)))
                             (- current-inventory (to-uint (- 0 quantity-change)))
                             u0)
                         (+ current-inventory (to-uint quantity-change))))
  )
    (asserts! (<= adjusted-inventory (var-get ecosystem-capacity-threshold)) err-threshold-breach)
    (var-set active-sequences-counter adjusted-inventory)
    (ok true)))

;; Public interface functions

;; Contribute sequences for exchange
(define-public (contribute-sequence-listing (quantity uint) (token-rate uint))
  (let (
    (available-sequences (default-to u0 (map-get? genomic-sequence-holdings tx-sender)))
    (current-listing-amount (get quantity (default-to {quantity: u0, token-rate: u0} (map-get? sequence-listings {contributor: tx-sender}))))
    (total-listed-sequences (+ quantity current-listing-amount))
  )
    (asserts! (> quantity u0) err-invalid-sequence-quantity)
    (asserts! (> token-rate u0) err-invalid-token-rate)
    (asserts! (>= available-sequences total-listed-sequences) err-insufficient-sequences)
    (try! (adjust-sequence-inventory (to-int quantity)))
    (map-set sequence-listings {contributor: tx-sender} {quantity: total-listed-sequences, token-rate: token-rate})
    (ok true)))

;; Withdraw sequences from marketplace
(define-public (withdraw-sequence-listing (quantity uint))
  (let (
    (current-listing (get quantity (default-to {quantity: u0, token-rate: u0} (map-get? sequence-listings {contributor: tx-sender}))))
  )
    (asserts! (>= current-listing quantity) err-insufficient-sequences)
    (try! (adjust-sequence-inventory (to-int (- quantity))))
    (map-set sequence-listings {contributor: tx-sender} 
             {quantity: (- current-listing quantity), token-rate: (get token-rate (default-to {quantity: u0, token-rate: u0} (map-get? sequence-listings {contributor: tx-sender})))})
    (ok true)))

;; Acquire sequences from contributor
(define-public (acquire-sequences-from-contributor (contributor principal) (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, token-rate: u0} (map-get? sequence-listings {contributor: contributor})))
    (sequence-cost (* quantity (get token-rate listing-data)))
    (tribute (derive-tribute-amount sequence-cost))
    (total-cost (+ sequence-cost tribute))
    (contributor-sequences (default-to u0 (map-get? genomic-sequence-holdings contributor)))
    (acquirer-balance (default-to u0 (map-get? token-holdings tx-sender)))
    (contributor-balance (default-to u0 (map-get? token-holdings contributor)))
    (steward-balance (default-to u0 (map-get? token-holdings protocol-steward)))
  )
    (asserts! (not (is-eq tx-sender contributor)) err-identical-entity)
    (asserts! (> quantity u0) err-invalid-sequence-quantity)
    (asserts! (>= (get quantity listing-data) quantity) err-insufficient-sequences)
    (asserts! (>= contributor-sequences quantity) err-insufficient-sequences)
    (asserts! (>= acquirer-balance total-cost) err-insufficient-sequences)

    ;; Update contributor's sequence balance and listing quantity
    (map-set genomic-sequence-holdings contributor (- contributor-sequences quantity))
    (map-set sequence-listings {contributor: contributor} 
             {quantity: (- (get quantity listing-data) quantity), token-rate: (get token-rate listing-data)})

    ;; Update acquirer's token and sequence balances
    (map-set token-holdings tx-sender (- acquirer-balance total-cost))
    (map-set genomic-sequence-holdings tx-sender (+ (default-to u0 (map-get? genomic-sequence-holdings tx-sender)) quantity))

    ;; Update contributor's and steward's token balances
    (map-set token-holdings contributor (+ contributor-balance sequence-cost))
    (map-set token-holdings protocol-steward (+ steward-balance tribute))

    (ok true)))

;; Return sequences for token compensation
(define-public (return-sequences (quantity uint))
  (let (
    (contributor-sequences (default-to u0 (map-get? genomic-sequence-holdings tx-sender)))
    (compensation-amount (derive-reclaim-compensation quantity))
    (steward-token-balance (default-to u0 (map-get? token-holdings protocol-steward)))
  )
    (asserts! (> quantity u0) err-invalid-sequence-quantity)
    (asserts! (>= contributor-sequences quantity) err-insufficient-sequences)
    (asserts! (>= steward-token-balance compensation-amount) err-sequence-transfer-failure)

    ;; Update contributor's sequence holdings
    (map-set genomic-sequence-holdings tx-sender (- contributor-sequences quantity))

    ;; Update token balances for contributor and protocol steward
    (map-set token-holdings tx-sender (+ (default-to u0 (map-get? token-holdings tx-sender)) compensation-amount))
    (map-set token-holdings protocol-steward (- steward-token-balance compensation-amount))

    ;; Transfer returned sequences to protocol steward
    (map-set genomic-sequence-holdings protocol-steward (+ (default-to u0 (map-get? genomic-sequence-holdings protocol-steward)) quantity))

    ;; Update global sequence counter
    (try! (adjust-sequence-inventory (to-int (- quantity))))

    (ok true)))

;; Update ecosystem parameters
;; Only protocol steward can adjust these global parameters
(define-public (reconfigure-ecosystem-parameters (new-tribute-rate uint) (new-reclaim-ratio uint) (new-contributor-ceiling uint) (new-ecosystem-threshold uint))
  (begin
    ;; Only protocol steward can update parameters
    (asserts! (is-eq tx-sender protocol-steward) err-unauthorized-access)
    ;; Validate parameter constraints
    (asserts! (<= new-tribute-rate u30) err-invalid-tribute-setting) ;; Tribute can't exceed 30%
    (asserts! (<= new-reclaim-ratio u100) err-invalid-tribute-setting) ;; Reclaim ratio can't exceed 100%
    (asserts! (>= new-contributor-ceiling u1000) err-invalid-threshold) ;; Min contributor capacity is 1000 units
    (asserts! (>= new-ecosystem-threshold (var-get active-sequences-counter)) err-invalid-threshold) ;; New threshold can't be less than current inventory

    ;; Apply parameter updates
    (var-set protocol-tribute-rate new-tribute-rate)
    (var-set contribution-reclaim-ratio new-reclaim-ratio)
    (var-set contributor-capacity-ceiling new-contributor-ceiling)
    (var-set ecosystem-capacity-threshold new-ecosystem-threshold)

    (ok true)))

;; Direct sequence transfer between entities
;; Facilitates peer-to-peer sequence ownership exchange
(define-public (direct-sequence-transfer (recipient principal) (quantity uint) (transfer-compensation uint))
  (let (
    (initiator tx-sender)
    (initiator-sequences (default-to u0 (map-get? genomic-sequence-holdings initiator)))
    (recipient-sequences (default-to u0 (map-get? genomic-sequence-holdings recipient)))
    (recipient-new-total (+ recipient-sequences quantity))
    (tribute (derive-tribute-amount transfer-compensation))
    (initiator-payment (- transfer-compensation tribute))
    (initiator-token-balance (default-to u0 (map-get? token-holdings initiator)))
    (recipient-token-balance (default-to u0 (map-get? token-holdings recipient)))
    (steward-token-balance (default-to u0 (map-get? token-holdings protocol-steward)))
  )
    ;; Validate transfer parameters
    (asserts! (not (is-eq initiator recipient)) err-identical-entity)
    (asserts! (> quantity u0) err-invalid-sequence-quantity)
    (asserts! (>= initiator-sequences quantity) err-insufficient-sequences)
    (asserts! (<= recipient-new-total (var-get contributor-capacity-ceiling)) err-threshold-breach)
    (asserts! (>= recipient-token-balance transfer-compensation) err-insufficient-sequences)

    ;; Update sequence balances
    (map-set genomic-sequence-holdings initiator (- initiator-sequences quantity))
    (map-set genomic-sequence-holdings recipient recipient-new-total)

    ;; Process token compensation
    (map-set token-holdings recipient (- recipient-token-balance transfer-compensation))
    (map-set token-holdings initiator (+ initiator-token-balance initiator-payment))
    (map-set token-holdings protocol-steward (+ steward-token-balance tribute))

    (ok true)))

;; Contribute sequences to ecosystem
;; Increases contributor's sequence holdings
(define-public (contribute-sequences (quantity uint))
  (let (
    (contributor tx-sender)
    (current-sequences (default-to u0 (map-get? genomic-sequence-holdings contributor)))
    (new-total (+ current-sequences quantity))
  )
    (asserts! (> quantity u0) err-invalid-sequence-quantity)
    (asserts! (<= new-total (var-get contributor-capacity-ceiling)) err-threshold-breach)
    ;; Update contributor's sequence holdings
    (map-set genomic-sequence-holdings contributor new-total)
    ;; Update global sequence tracking
    (try! (adjust-sequence-inventory (to-int quantity)))
    (ok true)))

;; Extended ecosystem functionality

;; Additional data structures for advanced features
(define-map subscription-arrangements {provider: principal} {cost-per-cycle: uint, sequences-per-cycle: uint, max-cycles: uint, enabled: bool})
(define-map active-subscriptions {subscriber: principal, provider: principal} {cycles-purchased: uint, cycles-remaining: uint, sequences-per-cycle: uint})
(define-map allocated-subscription-sequences principal uint)
(define-map exchange-records {acquirer: principal, provider: principal} {quantity: uint, timestamp: uint, cost: uint})
(define-map sequence-quality-reports {provider: principal} {issues: uint})
(define-map daily-exchange-tracker {day: uint} uint)
(define-map daily-value-exchange {day: uint} uint)
(define-map participant-exchange-count principal uint)
(define-map ecosystem-metrics {id: uint} {total-exchanges: uint, total-value: uint, active-contributors: uint})
(define-map authorized-moderators principal bool)
(define-map sequence-access-grants {owner: principal, accessor: principal} {quantity: uint, expiration: uint, canceled: bool})
(define-map reserved-access-sequences {owner: principal, accessor: principal} uint)
(define-map access-grant-history {owner: principal, accessor: principal, timestamp: uint} {quantity: uint, duration: uint, granted-at: uint})
(define-map provider-reputation principal {total-evaluations: uint, evaluation-sum: uint, average: uint})
(define-map exchange-evaluations {acquirer: principal, provider: principal, exchange-id: uint} {score: uint, timestamp: uint})
(define-map provider-classification principal uint)

;; Establish sequence subscription arrangement
;; Creates recurring access plan for sequence data
(define-public (establish-subscription-arrangement (cost-per-cycle uint) (sequences-per-cycle uint) (max-cycles uint))
  (let (
    (provider tx-sender)
    (provider-sequence-balance (default-to u0 (map-get? genomic-sequence-holdings provider)))
    (max-sequences-needed (* sequences-per-cycle max-cycles))
  )
    ;; Verify provider capacity and valid parameters
    (asserts! (> cost-per-cycle u0) err-invalid-token-rate)
    (asserts! (> sequences-per-cycle u0) err-invalid-sequence-quantity)
    (asserts! (> max-cycles u0) err-invalid-threshold)
    (asserts! (>= provider-sequence-balance sequences-per-cycle) err-insufficient-sequences)

    ;; Register the subscription arrangement
    (map-set subscription-arrangements 
             {provider: provider} 
             {cost-per-cycle: cost-per-cycle, 
              sequences-per-cycle: sequences-per-cycle, 
              max-cycles: max-cycles,
              enabled: true})

    ;; Reserve initial sequences for the arrangement
    (try! (adjust-sequence-inventory (to-int sequences-per-cycle)))
    (map-set allocated-subscription-sequences provider sequences-per-cycle)

    (ok true)))

;; Subscribe to provider's sequence data
;; Establishes recurring access to sequence data
(define-public (register-subscription (provider principal) (cycles uint))
  (let (
    (subscriber tx-sender)
    (arrangement (default-to {cost-per-cycle: u0, sequences-per-cycle: u0, max-cycles: u0, enabled: false} 
                  (map-get? subscription-arrangements {provider: provider})))
    (cost-per-cycle (get cost-per-cycle arrangement))
    (sequences-per-cycle (get sequences-per-cycle arrangement))
    (max-cycles (get max-cycles arrangement))
    (enabled (get enabled arrangement))
    (total-subscription-cost (* cost-per-cycle cycles))
    (total-sequences (* sequences-per-cycle cycles))
    (subscriber-balance (default-to u0 (map-get? token-holdings subscriber)))
    (tribute (derive-tribute-amount total-subscription-cost))
    (provider-payment (- total-subscription-cost tribute))
    (provider-balance (default-to u0 (map-get? token-holdings provider)))
    (steward-balance (default-to u0 (map-get? token-holdings protocol-steward)))
  )
    ;; Validate subscription registration
    (asserts! (not (is-eq subscriber provider)) err-identical-entity)
    (asserts! enabled err-sequence-transfer-failure)
    (asserts! (> cycles u0) err-invalid-sequence-quantity) 
    (asserts! (<= cycles max-cycles) err-threshold-breach)
    (asserts! (>= subscriber-balance total-subscription-cost) err-insufficient-sequences)

    ;; Transfer sequences and handle payment
    (map-set genomic-sequence-holdings subscriber (+ (default-to u0 (map-get? genomic-sequence-holdings subscriber)) total-sequences))
    (map-set genomic-sequence-holdings provider (- (default-to u0 (map-get? genomic-sequence-holdings provider)) total-sequences))

    ;; Update token balances
    (map-set token-holdings subscriber (- subscriber-balance total-subscription-cost))
    (map-set token-holdings provider (+ provider-balance provider-payment))
    (map-set token-holdings protocol-steward (+ steward-balance tribute))

    ;; Record subscription details
    (map-insert active-subscriptions 
                {subscriber: subscriber, provider: provider} 
                {cycles-purchased: cycles, 
                 cycles-remaining: cycles, 
                 sequences-per-cycle: sequences-per-cycle})

    (ok true)))

;; Quality control assessment for sequence data
;; Protocol steward can verify sequence quality and process remediation
(define-public (assess-sequence-quality (sequence-provider principal) (sequence-acquirer principal) (compensation-amount uint))
  (let (
    (assessment-initiator tx-sender)
    (provider-balance (default-to u0 (map-get? token-holdings sequence-provider)))
    (acquirer-balance (default-to u0 (map-get? token-holdings sequence-acquirer)))
    (acquirer-sequences (default-to u0 (map-get? genomic-sequence-holdings sequence-acquirer)))
    (exchange (default-to {quantity: u0, timestamp: u0, cost: u0} 
                 (map-get? exchange-records {acquirer: sequence-acquirer, provider: sequence-provider})))
    (exchange-quantity (get quantity exchange))
  )
    ;; Only protocol steward can perform quality assessments
    (asserts! (is-eq assessment-initiator protocol-steward) err-unauthorized-access)
    (asserts! (> compensation-amount u0) err-invalid-sequence-quantity)
    (asserts! (<= compensation-amount exchange-quantity) err-threshold-breach)
    (asserts! (>= provider-balance compensation-amount) err-insufficient-sequences)

    ;; Process quality assessment compensation
    (map-set token-holdings sequence-provider (- provider-balance compensation-amount))

    (ok true)))

;; Ecosystem analytics collection
;; Track protocol-wide metrics for governance insights
(define-public (record-ecosystem-metrics (provider principal) (acquirer principal) (sequence-quantity uint) (cost uint))
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (daily-exchanges (default-to u0 (map-get? daily-exchange-tracker {day: (/ current-time u86400)})))
    (total-value (default-to u0 (map-get? daily-value-exchange {day: (/ current-time u86400)})))
    (provider-exchange-count (default-to u0 (map-get? participant-exchange-count provider)))
    (acquirer-exchange-count (default-to u0 (map-get? participant-exchange-count acquirer)))
    (ecosystem-tracking (default-to {total-exchanges: u0, total-value: u0, active-contributors: u0}
                        (map-get? ecosystem-metrics {id: u1})))
  )
    ;; Only protocol steward or authorized moderators can record ecosystem metrics
    (asserts! (or (is-eq tx-sender protocol-steward) 
                 (is-some (map-get? authorized-moderators tx-sender))) err-unauthorized-access)
    (asserts! (> sequence-quantity u0) err-invalid-sequence-quantity)
    (asserts! (> cost u0) err-invalid-token-rate)

    ;; Update daily exchange metrics
    (map-set daily-exchange-tracker {day: (/ current-time u86400)} (+ daily-exchanges u1))
    (map-set daily-value-exchange {day: (/ current-time u86400)} (+ total-value cost))

    ;; Update global ecosystem metrics
    (map-set ecosystem-metrics {id: u1}
             {total-exchanges: (+ (get total-exchanges ecosystem-tracking) u1),
              total-value: (+ (get total-value ecosystem-tracking) cost),
              active-contributors: (get active-contributors ecosystem-tracking)})

    (ok true)))

;; Grant sequence access to external services
;; Allows sequence owners to authorize specific access permissions
(define-public (grant-sequence-access (service-entity principal) (sequence-quantity uint) (access-period uint))
  (let (
    (sequence-owner tx-sender)
    (owner-sequence-balance (default-to u0 (map-get? genomic-sequence-holdings sequence-owner)))
    (current-time (unwrap-panic (get-block-info? time u0)))
    (expiration-timestamp (+ current-time (* access-period u86400)))
    (existing-grant (default-to {quantity: u0, expiration: u0, canceled: false}
                    (map-get? sequence-access-grants {owner: sequence-owner, accessor: service-entity})))
    (is-canceled (get canceled existing-grant))
  )
    ;; Validate access grant parameters
    (asserts! (> sequence-quantity u0) err-invalid-sequence-quantity)
    (asserts! (> access-period u0) err-invalid-threshold)
    (asserts! (>= owner-sequence-balance sequence-quantity) err-insufficient-sequences)
    (asserts! (not is-canceled) err-sequence-transfer-failure)

    (ok true)))

;; Establish provider reputation system
;; Enables quality assessment for sequence providers
(define-public (evaluate-sequence-provider (provider principal) (evaluation-score uint) (exchange-id uint))
  (let (
    (evaluator tx-sender)
    (exchange (default-to {quantity: u0, timestamp: u0, cost: u0} 
                 (map-get? exchange-records {acquirer: evaluator, provider: provider})))
    (current-reputation (default-to {total-evaluations: u0, evaluation-sum: u0, average: u0} 
                         (map-get? provider-reputation provider)))
    (total-evaluations (get total-evaluations current-reputation))
    (evaluation-sum (get evaluation-sum current-reputation))
    (new-total-evaluations (+ total-evaluations u1))
    (new-evaluation-sum (+ evaluation-sum evaluation-score))
    (new-average (/ new-evaluation-sum new-total-evaluations))
  )
    ;; Validate evaluation parameters
    (asserts! (and (>= evaluation-score u1) (<= evaluation-score u5)) err-invalid-sequence-quantity) ;; Score must be between 1-5
    (asserts! (> (get quantity exchange) u0) err-sequence-transfer-failure) ;; Must have a valid exchange
    (asserts! (not (is-eq evaluator provider)) err-identical-entity) ;; Can't evaluate yourself

    ;; Verify this exchange exists and hasn't been evaluated
    (asserts! (is-none (map-get? exchange-evaluations {acquirer: evaluator, provider: provider, exchange-id: exchange-id}))
              err-sequence-transfer-failure)

    ;; Update provider's reputation
    (map-set provider-reputation 
             provider
             {total-evaluations: new-total-evaluations,
              evaluation-sum: new-evaluation-sum,
              average: new-average})

    ;; Update provider classification tier based on reputation
    (if (>= new-average u4)
        (map-set provider-classification provider u3) ;; Platinum contributor
        (if (>= new-average u3)
            (map-set provider-classification provider u2) ;; Gold contributor
            (map-set provider-classification provider u1))) ;; Standard contributor

    (ok true)))


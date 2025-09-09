;; ReefMine DAO - Liquid Reputation Governance Platform

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PROPOSAL (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-MILESTONE-NOT-READY (err u107))
(define-constant ERR-REPUTATION-LOCKED (err u108))
(define-constant ERR-INVALID-PHASE (err u109))
(define-constant ERR-ORACLE-ERROR (err u110))
(define-constant ERR-INSUFFICIENT-FUNDS (err u111))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u112))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant REPUTATION-DECAY-RATE u5) ;; 5% per cycle
(define-constant MIN-PROPOSAL-REPUTATION u100)
(define-constant VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant INCUBATION-PERIOD u144) ;; ~1 day in blocks
(define-constant QUADRATIC-SCALING u10000)

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var reputation-decay-cycle uint u0)
(define-data-var oracle-address (optional principal) none)
(define-data-var governance-paused bool false)
(define-data-var min-quorum uint u1000)

;; Proposal Structure
(define-map proposals uint {
    id: uint,
    proposer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    requested-amount: uint,
    phase: (string-ascii 20), ;; "incubation", "voting", "execution", "completed", "rejected"
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    reputation-weighted-yes: uint,
    reputation-weighted-no: uint,
    milestones-completed: uint,
    total-milestones: uint,
    impact-score: uint,
    dna-tags: (list 5 (string-ascii 20)),
    executed: bool,
    funds-released: uint
})

;; User Reputation System
(define-map user-reputation principal {
    base-reputation: uint,
    decay-adjusted: uint,
    last-activity: uint,
    successful-proposals: uint,
    failed-proposals: uint,
    voting-accuracy: uint,
    locked-reputation: uint,
    reputation-source: (string-ascii 50)
})

;; Voting Records
(define-map votes {proposal-id: uint, voter: principal} {
    vote-weight: uint,
    reputation-at-vote: uint,
    vote-direction: bool, ;; true for yes, false for no
    timestamp: uint,
    quadratic-weight: uint
})

;; Proposal DNA System
(define-map proposal-dna {proposal-id: uint, tag: (string-ascii 20)} {
    confidence-score: uint,
    historical-success-rate: uint,
    similar-proposals: (list 10 uint),
    risk-assessment: uint
})

;; Milestone Tracking
(define-map proposal-milestones {proposal-id: uint, milestone-id: uint} {
    description: (string-utf8 200),
    target-date: uint,
    completion-date: (optional uint),
    required-amount: uint,
    verification-method: (string-ascii 30),
    completed: bool,
    oracle-verified: bool
})

;; Treasury Management
(define-map fund-allocations uint {
    proposal-id: uint,
    allocated-amount: uint,
    released-amount: uint,
    locked-until: uint,
    reallocation-target: (optional uint)
})

;; Reputation Appeals
(define-map reputation-appeals principal {
    appeal-reason: (string-utf8 300),
    requested-adjustment: int,
    submitted-at: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected"
    reviewed-by: (optional principal)
})

;; Oracle Data Integration
(define-map oracle-requests uint {
    request-type: (string-ascii 30),
    proposal-id: uint,
    data-hash: (buff 32),
    timestamp: uint,
    verified: bool,
    result: (optional uint)
})

;; Administrative Functions
(define-public (initialize-dao (initial-treasury uint) (oracle-addr principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set treasury-balance initial-treasury)
        (var-set oracle-address (some oracle-addr))
        (ok true)
    )
)

(define-public (update-governance-parameters (new-min-quorum uint) (new-min-reputation uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> new-min-quorum u0) ERR-INVALID-AMOUNT)
        (asserts! (> new-min-reputation u0) ERR-INVALID-AMOUNT)
        (var-set min-quorum new-min-quorum)
        (ok true)
    )
)

(define-public (pause-governance)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set governance-paused true)
        (ok true)
    )
)

;; Proposal Lifecycle Management
(define-public (create-proposal 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (requested-amount uint)
    (milestones uint)
    (dna-tags (list 5 (string-ascii 20))))
    (let (
        (user-rep (get-user-reputation tx-sender))
        (new-proposal-id (+ (var-get proposal-counter) u1))
        (current-block block-height)
    )
        (asserts! (not (var-get governance-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get decay-adjusted user-rep) MIN-PROPOSAL-REPUTATION) ERR-INSUFFICIENT-REPUTATION)
        (asserts! (> requested-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> milestones u0) ERR-INVALID-AMOUNT)
        (asserts! (<= requested-amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set proposals new-proposal-id {
            id: new-proposal-id,
            proposer: tx-sender,
            title: title,
            description: description,
            requested-amount: requested-amount,
            phase: "incubation",
            created-at: current-block,
            voting-ends-at: (+ current-block INCUBATION-PERIOD VOTING-PERIOD),
            yes-votes: u0,
            no-votes: u0,
            reputation-weighted-yes: u0,
            reputation-weighted-no: u0,
            milestones-completed: u0,
            total-milestones: milestones,
            impact-score: u0,
            dna-tags: dna-tags,
            executed: false,
            funds-released: u0
        })
        
        (var-set proposal-counter new-proposal-id)
        (update-user-activity tx-sender)
        (ok new-proposal-id)
    )
)

(define-public (advance-proposal-phase (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (current-phase (get phase proposal))
        (current-block block-height)
    )
        (asserts! (not (var-get governance-paused)) ERR-NOT-AUTHORIZED)
        
        (if (is-eq current-phase "incubation")
            (begin
                (asserts! (> current-block (+ (get created-at proposal) INCUBATION-PERIOD)) ERR-INVALID-PHASE)
                (map-set proposals proposal-id (merge proposal {phase: "voting"}))
                (ok "moved-to-voting")
            )
            (if (is-eq current-phase "voting")
                (begin
                    (asserts! (> current-block (get voting-ends-at proposal)) ERR-INVALID-PHASE)
                    (let ((proposal-passed (evaluate-proposal-outcome proposal-id)))
                        (if proposal-passed
                            (begin
                                (map-set proposals proposal-id (merge proposal {phase: "execution"}))
                                (allocate-funds proposal-id (get requested-amount proposal))
                                (ok "moved-to-execution")
                            )
                            (begin
                                (map-set proposals proposal-id (merge proposal {phase: "rejected"}))
                                (ok "proposal-rejected")
                            )
                        )
                    )
                )
                ERR-INVALID-PHASE
            )
        )
    )
)

;; Reputation-Weighted Voting System
(define-public (cast-vote (proposal-id uint) (vote-direction bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (user-rep (get-user-reputation tx-sender))
        (current-block block-height)
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
        (base-weight (get decay-adjusted user-rep))
        (quadratic-weight (calculate-quadratic-weight base-weight))
    )
        (asserts! (not (var-get governance-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get phase proposal) "voting") ERR-INVALID-PHASE)
        (asserts! (< current-block (get voting-ends-at proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        (asserts! (> base-weight u0) ERR-INSUFFICIENT-REPUTATION)
        
        (map-set votes vote-key {
            vote-weight: base-weight,
            reputation-at-vote: base-weight,
            vote-direction: vote-direction,
            timestamp: current-block,
            quadratic-weight: quadratic-weight
        })
        
        (if vote-direction
            (map-set proposals proposal-id (merge proposal {
                yes-votes: (+ (get yes-votes proposal) u1),
                reputation-weighted-yes: (+ (get reputation-weighted-yes proposal) quadratic-weight)
            }))
            (map-set proposals proposal-id (merge proposal {
                no-votes: (+ (get no-votes proposal) u1),
                reputation-weighted-no: (+ (get reputation-weighted-no proposal) quadratic-weight)
            }))
        )
        
        (update-user-activity tx-sender)
        (ok true)
    )
)

;; Milestone and Treasury Management
(define-public (complete-milestone (proposal-id uint) (milestone-id uint) (verification-data (buff 32)))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR
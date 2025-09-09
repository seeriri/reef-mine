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

;; Helper function to calculate reputation with decay
(define-private (calculate-current-reputation (user principal))
    (let (
        (stored-rep (default-to {
            base-reputation: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-proposals: u0,
            failed-proposals: u0,
            voting-accuracy: u100,
            locked-reputation: u0,
            reputation-source: "none"
        } (map-get? user-reputation user)))
        (blocks-since-activity (- block-height (get last-activity stored-rep)))
        (decay-cycles (/ blocks-since-activity u144))
        (total-decay-rate (* decay-cycles REPUTATION-DECAY-RATE))
        (decay-multiplier (if (>= total-decay-rate u100) u0 (- u100 total-decay-rate)))
        (current-reputation (/ (* (get base-reputation stored-rep) decay-multiplier) u100))
    )
        current-reputation
    )
)

;; Utility Functions - Fixed to return proper response type
(define-private (update-user-activity (user principal))
    (let (
        (current-rep (default-to {
            base-reputation: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-proposals: u0,
            failed-proposals: u0,
            voting-accuracy: u100,
            locked-reputation: u0,
            reputation-source: "activity"
        } (map-get? user-reputation user)))
    )
        (map-set user-reputation user (merge current-rep {
            last-activity: block-height
        }))
        (ok true)
    )
)

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
        (current-reputation (calculate-current-reputation tx-sender))
        (new-proposal-id (+ (var-get proposal-counter) u1))
        (current-block block-height)
    )
        (asserts! (not (var-get governance-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-reputation MIN-PROPOSAL-REPUTATION) ERR-INSUFFICIENT-REPUTATION)
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
        (unwrap! (update-user-activity tx-sender) ERR-NOT-AUTHORIZED)
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
                                (unwrap! (allocate-funds proposal-id (get requested-amount proposal)) ERR-INSUFFICIENT-FUNDS)
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
        (base-weight (calculate-current-reputation tx-sender))
        (current-block block-height)
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
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
        
        (unwrap! (update-user-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

;; Milestone and Treasury Management
(define-public (complete-milestone (proposal-id uint) (milestone-id uint) (verification-data (buff 32)))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (milestone-key {proposal-id: proposal-id, milestone-id: milestone-id})
        (milestone (unwrap! (map-get? proposal-milestones milestone-key) ERR-MILESTONE-NOT-READY))
        (current-block block-height)
    )
        (asserts! (is-eq (get phase proposal) "execution") ERR-INVALID-PHASE)
        (asserts! (is-eq tx-sender (get proposer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-NOT-READY)
        
        ;; Submit oracle verification request
        (unwrap! (request-oracle-verification proposal-id milestone-id verification-data) ERR-ORACLE-ERROR)
        
        (map-set proposal-milestones milestone-key (merge milestone {
            completion-date: (some current-block),
            completed: true
        }))
        
        ;; Update proposal milestone completion count
        (map-set proposals proposal-id (merge proposal {
            milestones-completed: (+ (get milestones-completed proposal) u1)
        }))
        
        ;; Release funds if milestone verified
        (unwrap! (release-milestone-funds proposal-id milestone-id) ERR-INSUFFICIENT-FUNDS)
        
        (ok true)
    )
)

(define-public (create-milestone 
    (proposal-id uint) 
    (milestone-id uint)
    (description (string-utf8 200))
    (target-date uint)
    (required-amount uint)
    (verification-method (string-ascii 30)))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (milestone-key {proposal-id: proposal-id, milestone-id: milestone-id})
    )
        (asserts! (is-eq tx-sender (get proposer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get phase proposal) "execution") ERR-INVALID-PHASE)
        (asserts! (is-none (map-get? proposal-milestones milestone-key)) ERR-INVALID-PROPOSAL)
        
        (map-set proposal-milestones milestone-key {
            description: description,
            target-date: target-date,
            completion-date: none,
            required-amount: required-amount,
            verification-method: verification-method,
            completed: false,
            oracle-verified: false
        })
        
        (ok true)
    )
)

;; Reputation Management Functions
(define-public (initialize-reputation (user principal) (initial-rep uint) (source (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> initial-rep u0) ERR-INVALID-AMOUNT)
        
        (map-set user-reputation user {
            base-reputation: initial-rep,
            decay-adjusted: initial-rep,
            last-activity: block-height,
            successful-proposals: u0,
            failed-proposals: u0,
            voting-accuracy: u100,
            locked-reputation: u0,
            reputation-source: source
        })
        
        (ok true)
    )
)

(define-public (submit-reputation-appeal (reason (string-utf8 300)) (adjustment int))
    (begin
        (asserts! (not (is-eq adjustment 0)) ERR-INVALID-AMOUNT)
        
        (map-set reputation-appeals tx-sender {
            appeal-reason: reason,
            requested-adjustment: adjustment,
            submitted-at: block-height,
            status: "pending",
            reviewed-by: none
        })
        
        (ok true)
    )
)

(define-public (review-reputation-appeal (user principal) (approved bool))
    (let (
        (appeal (unwrap! (map-get? reputation-appeals user) ERR-NOT-AUTHORIZED))
        (stored-rep (default-to {
            base-reputation: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-proposals: u0,
            failed-proposals: u0,
            voting-accuracy: u100,
            locked-reputation: u0,
            reputation-source: "none"
        } (map-get? user-reputation user)))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status appeal) "pending") ERR-INVALID-PHASE)
        
        (if approved
            (begin
                (let ((new-base-rep (if (< (get requested-adjustment appeal) 0)
                                       (if (>= (get base-reputation stored-rep) 
                                              (to-uint (- 0 (get requested-adjustment appeal))))
                                          (- (get base-reputation stored-rep) 
                                             (to-uint (- 0 (get requested-adjustment appeal))))
                                          u0)
                                       (+ (get base-reputation stored-rep) 
                                          (to-uint (get requested-adjustment appeal))))))
                    
                    (map-set user-reputation user (merge stored-rep {
                        base-reputation: new-base-rep,
                        decay-adjusted: new-base-rep
                    }))
                )
                
                (map-set reputation-appeals user (merge appeal {
                    status: "approved",
                    reviewed-by: (some tx-sender)
                }))
            )
            (map-set reputation-appeals user (merge appeal {
                status: "rejected",
                reviewed-by: (some tx-sender)
            }))
        )
        
        (ok approved)
    )
)

;; Oracle Integration Functions
(define-public (request-oracle-verification (proposal-id uint) (milestone-id uint) (data-hash (buff 32)))
    (let (
        (request-id (+ (var-get proposal-counter) block-height))
    )
        (asserts! (is-some (var-get oracle-address)) ERR-ORACLE-ERROR)
        
        (map-set oracle-requests request-id {
            request-type: "milestone-verification",
            proposal-id: proposal-id,
            data-hash: data-hash,
            timestamp: block-height,
            verified: false,
            result: none
        })
        
        (ok request-id)
    )
)

(define-public (oracle-callback (request-id uint) (verified bool) (result uint))
    (let (
        (request (unwrap! (map-get? oracle-requests request-id) ERR-ORACLE-ERROR))
        (oracle-addr (unwrap! (var-get oracle-address) ERR-ORACLE-ERROR))
    )
        (asserts! (is-eq tx-sender oracle-addr) ERR-NOT-AUTHORIZED)
        
        (map-set oracle-requests request-id (merge request {
            verified: verified,
            result: (some result)
        }))
        
        ;; Update milestone verification status
        (let ((milestone-key {proposal-id: (get proposal-id request), milestone-id: u1}))
            (match (map-get? proposal-milestones milestone-key)
                milestone (map-set proposal-milestones milestone-key (merge milestone {oracle-verified: verified}))
                true
            )
        )
        
        (ok verified)
    )
)

;; Treasury and Fund Management - Fixed to return proper response type
(define-private (allocate-funds (proposal-id uint) (amount uint))
    (let (
        (current-treasury (var-get treasury-balance))
    )
        (asserts! (>= current-treasury amount) ERR-INSUFFICIENT-FUNDS)
        
        (map-set fund-allocations proposal-id {
            proposal-id: proposal-id,
            allocated-amount: amount,
            released-amount: u0,
            locked-until: (+ block-height u144),
            reallocation-target: none
        })
        
        (var-set treasury-balance (- current-treasury amount))
        (ok true)
    )
)

(define-private (release-milestone-funds (proposal-id uint) (milestone-id uint))
    (let (
        (allocation (unwrap! (map-get? fund-allocations proposal-id) ERR-INSUFFICIENT-FUNDS))
        (milestone-key {proposal-id: proposal-id, milestone-id: milestone-id})
        (milestone (unwrap! (map-get? proposal-milestones milestone-key) ERR-MILESTONE-NOT-READY))
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
        (asserts! (get oracle-verified milestone) ERR-ORACLE-ERROR)
        (asserts! (>= (get allocated-amount allocation) (get required-amount milestone)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set fund-allocations proposal-id (merge allocation {
            released-amount: (+ (get released-amount allocation) (get required-amount milestone))
        }))
        
        (map-set proposals proposal-id (merge proposal {
            funds-released: (+ (get funds-released proposal) (get required-amount milestone))
        }))
        
        (ok true)
    )
)

;; Utility Functions
(define-private (calculate-quadratic-weight (base-weight uint))
    (let ((scaled-weight (/ (* base-weight QUADRATIC-SCALING) u100)))
        (sqrti scaled-weight)
    )
)

(define-private (evaluate-proposal-outcome (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) false))
        (total-rep-votes (+ (get reputation-weighted-yes proposal) (get reputation-weighted-no proposal)))
        (quorum-met (>= total-rep-votes (var-get min-quorum)))
        (majority-yes (> (get reputation-weighted-yes proposal) (get reputation-weighted-no proposal)))
    )
        (and quorum-met majority-yes)
    )
)

;; Read-Only Functions
(define-read-only (get-user-reputation (user principal))
    (let (
        (stored-rep (default-to {
            base-reputation: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-proposals: u0,
            failed-proposals: u0,
            voting-accuracy: u100,
            locked-reputation: u0,
            reputation-source: "none"
        } (map-get? user-reputation user)))
        (blocks-since-activity (- block-height (get last-activity stored-rep)))
        (decay-cycles (/ blocks-since-activity u144))
        (total-decay-rate (* decay-cycles REPUTATION-DECAY-RATE))
        (decay-multiplier (if (>= total-decay-rate u100) u0 (- u100 total-decay-rate)))
        (new-adjusted-rep (/ (* (get base-reputation stored-rep) decay-multiplier) u100))
    )
        (merge stored-rep {decay-adjusted: new-adjusted-rep})
    )
)

(define-read-only (get-proposal-details (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote-record (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-milestone-info (proposal-id uint) (milestone-id uint))
    (map-get? proposal-milestones {proposal-id: proposal-id, milestone-id: milestone-id})
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (get-governance-status)
    {
        paused: (var-get governance-paused),
        min-quorum: (var-get min-quorum),
        proposal-count: (var-get proposal-counter),
        treasury-balance: (var-get treasury-balance)
    }
)

(define-read-only (get-fund-allocation (proposal-id uint))
    (map-get? fund-allocations proposal-id)
)

(define-read-only (get-reputation-appeal (user principal))
    (map-get? reputation-appeals user)
)

(define-read-only (get-oracle-request (request-id uint))
    (map-get? oracle-requests request-id)
)

(define-read-only (get-proposal-dna (proposal-id uint) (tag (string-ascii 20)))
    (map-get? proposal-dna {proposal-id: proposal-id, tag: tag})
)
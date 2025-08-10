;; student-organization-governance.clar

;; --- Constants ---
;; Errors
(define-constant ERR-NOT-AUTHORIZED u100)
(define-constant ERR-PROPOSAL-NOT-FOUND u101)
(define-constant ERR-ALREADY-MEMBER u102)
(define-constant ERR-ALREADY-VOTED u103)
(define-constant ERR-INVALID-PROPOSAL-TYPE u104)
(define-constant ERR-ALREADY-EXECUTED u105)
(define-constant ERR-VOTING-FAILED u106)

;; Roles
(define-constant ROLE-KETUA-ORMAWA u5)
(define-constant ROLE-KETUA-BIDANG u3)
(define-constant ROLE-ANGGOTA-BIASA u1)

;; Proposal Types
(define-constant PROPOSAL-TYPE-BUDGET-ALLOCATION u1)
(define-constant PROPOSAL-TYPE-PROGRAM-APPROVAL u2)

;; --- Data Maps and Variables ---
;; Mappings for members and their status/role
(define-map members principal { status: uint })

;; Mapping for proposal details
(define-map proposals uint {
  title: (string-ascii 64),
  description: (string-ascii 256),
  creator: principal,
  proposal-type: uint,
  details: {
    target-address: (optional principal),
    amount: (optional uint)
  },
  votes-for: uint,
  votes-against: uint,
  executed: bool
})

;; Mapping to track if a member has voted on a proposal
(define-map has-voted { proposal-id: uint, voter: principal } bool)

;; Variable to store the next proposal ID
(define-data-var next-proposal-id uint u1)

;; --- Owner and Access Control ---
;; Contract owner for privileged functions
(define-data-var contract-owner principal tx-sender)

(define-read-only (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; --- Public Functions ---
;; @desc Registers a new member with a specific status
(define-public (register-member (member principal) (status uint))
  (begin
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-none (map-get? members member)) (err ERR-ALREADY-MEMBER))
    (ok (map-set members member { status: status }))
  )
)

;; @desc Creates a new proposal
(define-public (create-proposal (title (string-ascii 64)) (description (string-ascii 256)) (proposal-type uint) (target-address (optional principal)) (amount (optional uint)))
  (begin
    (asserts! (is-some (map-get? members tx-sender)) (err ERR-NOT-AUTHORIZED))
    (let ((proposal-id (var-get next-proposal-id)))
      (map-set proposals proposal-id {
        title: title,
        description: description,
        creator: tx-sender,
        proposal-type: proposal-type,
        details: { target-address: target-address, amount: amount },
        votes-for: u0,
        votes-against: u0,
        executed: false
      })
      (var-set next-proposal-id (+ u1 proposal-id))
      (ok proposal-id)
    )
  )
)

;; @desc Submits a weighted vote on a proposal
(define-public (weighted-vote (proposal-id uint) (vote-choice bool))
  (let
    ((member-info (map-get? members tx-sender)))
    (asserts! (is-some member-info) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-some (map-get? proposals proposal-id)) (err ERR-PROPOSAL-NOT-FOUND))
    (asserts! (is-none (map-get? has-voted { proposal-id: proposal-id, voter: tx-sender })) (err ERR-ALREADY-VOTED))
    
    (let ((proposal (unwrap-panic (map-get? proposals proposal-id))))
      (let ((voter-status (get status (unwrap-panic member-info))))
        (let ((vote-weight
                (if (is-eq voter-status ROLE-KETUA-ORMAWA)
                  ROLE-KETUA-ORMAWA
                  (if (is-eq voter-status ROLE-KETUA-BIDANG)
                    ROLE-KETUA-BIDANG
                    ROLE-ANGGOTA-BIASA
                  )
                )))
          (if vote-choice
            (map-set proposals proposal-id (merge proposal { votes-for: (+ (get votes-for proposal) vote-weight) }))
            (map-set proposals proposal-id (merge proposal { votes-against: (+ (get votes-against proposal) vote-weight) }))
          )
        )
      )
    )
    (ok (map-set has-voted { proposal-id: proposal-id, voter: tx-sender } true))
  )
)

;; @desc Executes a proposal if it has been approved
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal-info (unwrap-panic (map-get? proposals proposal-id))))
    (asserts! (is-contract-owner) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (get executed proposal-info)) (err ERR-ALREADY-EXECUTED))
    (asserts! (> (get votes-for proposal-info) (get votes-against proposal-info)) (err ERR-VOTING-FAILED))

    (let ((proposal-type (get proposal-type proposal-info)))
      (if (is-eq proposal-type PROPOSAL-TYPE-BUDGET-ALLOCATION)
        (begin
          ;; Placeholder for budget transfer. This will need a real contract call.
          ;; (try! (contract-call? 'SP2X22.my-token transfer ...))
          (ok (map-set proposals proposal-id (merge proposal-info { executed: true })))
        )
        (if (is-eq proposal-type PROPOSAL-TYPE-PROGRAM-APPROVAL)
          (ok (map-set proposals proposal-id (merge proposal-info { executed: true })))
          (err ERR-INVALID-PROPOSAL-TYPE)
        )
      )
    )
  )
)

;; --- Read-Only Functions (untuk front-end) ---
;; @desc Gets a proposal's details by ID
(define-read-only (get-proposal-by-id (proposal-id uint))
  (map-get? proposals proposal-id)
)

;; @desc Gets the current next proposal ID
(define-read-only (get-next-proposal-id)
  (ok (var-get next-proposal-id))
)

;; @desc Gets a member's status
(define-read-only (get-member-status (member principal))
  (map-get? members member)
)

;; @desc Checks if a member has voted on a proposal
(define-read-only (has-voted-on-proposal (proposal-id uint) (voter principal))
  (map-get? has-voted { proposal-id: proposal-id, voter: voter })
)
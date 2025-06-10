;; P2P Betting Platform Contract
;; A decentralized peer-to-peer betting platform where users can create and accept bets

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-bet-not-found (err u103))
(define-constant err-bet-already-accepted (err u104))
(define-constant err-bet-not-accepted (err u105))
(define-constant err-bet-already-resolved (err u106))
(define-constant err-cannot-bet-against-self (err u107))
(define-constant err-insufficient-balance (err u108))
(define-constant err-bet-expired (err u109))
(define-constant err-bet-not-expired (err u110))

;; Data Variables
(define-data-var next-bet-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% (250/10000)
(define-data-var total-platform-fees uint u0)

;; Bet Status Enum
(define-constant bet-status-open u0)
(define-constant bet-status-accepted u1)
(define-constant bet-status-resolved u2)
(define-constant bet-status-cancelled u3)

;; Bet Structure
(define-map bets uint {
    id: uint,
    creator: principal,
    opponent: (optional principal),
    description: (string-ascii 256),
    bet-amount: uint,
    creator-side: bool, ;; true = creator bets on "yes", false = creator bets on "no"
    status: uint,
    winner: (optional bool), ;; true = "yes" wins, false = "no" wins, none = not resolved
    created-at: uint,
    expires-at: uint,
    resolved-at: (optional uint)
})

;; User Statistics
(define-map user-stats principal {
    total-bets: uint,
    won-bets: uint,
    total-wagered: uint,
    total-winnings: uint
})

;; Active bets per user (for tracking)
(define-map user-active-bets principal (list 100 uint))

;; Escrow for accepted bets
(define-map bet-escrow uint uint)

;; Create a new bet
(define-public (create-bet (description (string-ascii 256)) (bet-amount uint) (creator-side bool) (duration uint))
    (let (
        (bet-id (var-get next-bet-id))
        (expires-at (+ block-height duration))
    )
    (begin
        (asserts! (> bet-amount u0) err-invalid-amount)
        (asserts! (> duration u0) err-invalid-amount)
        (asserts! (>= (stx-get-balance tx-sender) bet-amount) err-insufficient-balance)
        
        ;; Transfer bet amount to contract escrow
        (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)))
        
        ;; Store the bet
        (map-set bets bet-id {
            id: bet-id,
            creator: tx-sender,
            opponent: none,
            description: description,
            bet-amount: bet-amount,
            creator-side: creator-side,
            status: bet-status-open,
            winner: none,
            created-at: block-height,
            expires-at: expires-at,
            resolved-at: none
        })
        
        ;; Store escrow amount
        (map-set bet-escrow bet-id bet-amount)
        
        ;; Update user stats
        (update-user-stats tx-sender bet-amount u0 u0)
        
        ;; Increment bet ID
        (var-set next-bet-id (+ bet-id u1))
        
        (print {
            event: "bet-created",
            bet-id: bet-id,
            creator: tx-sender,
            amount: bet-amount,
            description: description
        })
        
        (ok bet-id))))

;; Accept an existing bet
(define-public (accept-bet (bet-id uint))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) err-bet-not-found))
        (bet-amount (get bet-amount bet-data))
    )
    (begin
        (asserts! (is-eq (get status bet-data) bet-status-open) err-bet-already-accepted)
        (asserts! (not (is-eq tx-sender (get creator bet-data))) err-cannot-bet-against-self)
        (asserts! (< block-height (get expires-at bet-data)) err-bet-expired)
        (asserts! (>= (stx-get-balance tx-sender) bet-amount) err-insufficient-balance)
        
        ;; Transfer opponent's bet amount to contract
        (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)))
        
        ;; Update bet status and opponent
        (map-set bets bet-id (merge bet-data {
            opponent: (some tx-sender),
            status: bet-status-accepted
        }))
        
        ;; Update escrow (now holds both bets)
        (map-set bet-escrow bet-id (* bet-amount u2))
        
        ;; Update user stats
        (update-user-stats tx-sender bet-amount u0 u0)
        
        (print {
            event: "bet-accepted",
            bet-id: bet-id,
            opponent: tx-sender,
            total-pot: (* bet-amount u2)
        })
        
        (ok true))))

;; Resolve a bet (only contract owner for now - could be extended with oracle integration)
(define-public (resolve-bet (bet-id uint) (outcome bool))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) err-bet-not-found))
        (bet-amount (get bet-amount bet-data))
        (total-pot (* bet-amount u2))
        (platform-fee (/ (* total-pot (var-get platform-fee-rate)) u10000))
        (winner-payout (- total-pot platform-fee))
        (creator (get creator bet-data))
        (opponent (unwrap! (get opponent bet-data) err-bet-not-accepted))
        (creator-wins (is-eq (get creator-side bet-data) outcome))
        (winner (if creator-wins creator opponent))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-eq (get status bet-data) bet-status-accepted) err-bet-not-accepted)
        
        ;; Update bet with resolution
        (map-set bets bet-id (merge bet-data {
            status: bet-status-resolved,
            winner: (some outcome),
            resolved-at: (some block-height)
        }))
        
        ;; Transfer winnings to winner
        (try! (as-contract (stx-transfer? winner-payout tx-sender winner)))
        
        ;; Update platform fees
        (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
        
        ;; Update winner's stats
        (update-user-stats winner u0 u1 winner-payout)
        
        ;; Clear escrow
        (map-delete bet-escrow bet-id)
        
        (print {
            event: "bet-resolved",
            bet-id: bet-id,
            winner: winner,
            outcome: outcome,
            payout: winner-payout,
            platform-fee: platform-fee
        })
        
        (ok winner))))

;; Cancel an open bet (only creator can cancel)
(define-public (cancel-bet (bet-id uint))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) err-bet-not-found))
        (bet-amount (get bet-amount bet-data))
    )
    (begin
        (asserts! (is-eq tx-sender (get creator bet-data)) err-not-authorized)
        (asserts! (is-eq (get status bet-data) bet-status-open) err-bet-already-accepted)
        
        ;; Update bet status
        (map-set bets bet-id (merge bet-data {
            status: bet-status-cancelled
        }))
        
        ;; Refund the creator
        (try! (as-contract (stx-transfer? bet-amount tx-sender (get creator bet-data))))
        
        ;; Clear escrow
        (map-delete bet-escrow bet-id)
        
        (print {
            event: "bet-cancelled",
            bet-id: bet-id,
            creator: tx-sender,
            refund: bet-amount
        })
        
        (ok true))))

;; Auto-cancel expired bets
(define-public (cancel-expired-bet (bet-id uint))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) err-bet-not-found))
        (bet-amount (get bet-amount bet-data))
    )
    (begin
        (asserts! (is-eq (get status bet-data) bet-status-open) err-bet-already-accepted)
        (asserts! (>= block-height (get expires-at bet-data)) err-bet-not-expired)
        
        ;; Update bet status
        (map-set bets bet-id (merge bet-data {
            status: bet-status-cancelled
        }))
        
        ;; Refund the creator
        (try! (as-contract (stx-transfer? bet-amount tx-sender (get creator bet-data))))
        
        ;; Clear escrow
        (map-delete bet-escrow bet-id)
        
        (print {
            event: "bet-expired-cancelled",
            bet-id: bet-id,
            creator: (get creator bet-data),
            refund: bet-amount
        })
        
        (ok true))))

;; Helper function to update user statistics
(define-private (update-user-stats (user principal) (wagered uint) (won uint) (winnings uint))
    (let (
        (current-stats (default-to {total-bets: u0, won-bets: u0, total-wagered: u0, total-winnings: u0}
                                   (map-get? user-stats user)))
    )
    (map-set user-stats user {
        total-bets: (+ (get total-bets current-stats) (if (> wagered u0) u1 u0)),
        won-bets: (+ (get won-bets current-stats) won),
        total-wagered: (+ (get total-wagered current-stats) wagered),
        total-winnings: (+ (get total-winnings current-stats) winnings)
    })))

;; Read-only functions

;; Get bet details
(define-read-only (get-bet (bet-id uint))
    (ok (map-get? bets bet-id)))

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (ok (map-get? user-stats user)))

;; Get open bets (simplified - in practice you'd want pagination)
(define-read-only (get-bet-status (bet-id uint))
    (match (map-get? bets bet-id)
        bet-data (ok (some (get status bet-data)))
        (ok none)))

;; Get platform fee rate
(define-read-only (get-platform-fee-rate)
    (ok (var-get platform-fee-rate)))

;; Get total platform fees collected
(define-read-only (get-total-platform-fees)
    (ok (var-get total-platform-fees)))

;; Get next bet ID
(define-read-only (get-next-bet-id)
    (ok (var-get next-bet-id)))

;; Get bet escrow amount
(define-read-only (get-bet-escrow (bet-id uint))
    (ok (map-get? bet-escrow bet-id)))

;; Admin functions

;; Update platform fee rate (only owner)
(define-public (set-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)))

;; Withdraw platform fees (only owner)
(define-public (withdraw-platform-fees (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= amount (var-get total-platform-fees)) err-insufficient-balance)
        (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
        (var-set total-platform-fees (- (var-get total-platform-fees) amount))
        (ok true)))

;; Get contract balance
(define-read-only (get-contract-balance)
    (ok (stx-get-balance (as-contract tx-sender))))

;; Emergency pause functionality could be added here
;; Oracle integration for automatic bet resolution could be added
;; Multi-signature resolution for disputed bets could be implemented
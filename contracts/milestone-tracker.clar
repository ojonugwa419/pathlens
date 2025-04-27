;; PathLens Milestone Tracking Smart Contract
;; Enables users to create, track, and manage career milestones
;; Supports goal association, verification, and progress tracking

;; Error Codes
(define-constant ERR_UNAUTHORIZED u403)
(define-constant ERR_MILESTONE_NOT_FOUND u404)
(define-constant ERR_DUPLICATE_MILESTONE u409)
(define-constant ERR_INVALID_MILESTONE u422)

;; Milestone Status
(define-constant STATUS_PENDING u0)
(define-constant STATUS_IN_PROGRESS u1)
(define-constant STATUS_COMPLETED u2)
(define-constant STATUS_VERIFIED u3)

;; Milestone Data Structure
(define-map milestones 
  {
    owner: principal, 
    milestone-id: uint
  }
  {
    title: (string-utf8 100),      ;; Milestone title
    description: (string-utf8 500), ;; Detailed description
    goal-id: (optional uint),       ;; Optional associated goal
    status: uint,                   ;; Current milestone status
    target-date: uint,              ;; Target completion date (unix timestamp)
    progress-percentage: uint,      ;; Progress tracking (0-100)
    created-at: uint,               ;; Creation timestamp
    updated-at: uint                ;; Last update timestamp
  }
)

;; Tracks the next available milestone ID for each user
(define-map milestone-counters principal uint)

;; Helper: Get next milestone ID for a user
(define-private (get-next-milestone-id (user principal))
  (let 
    ((current-count (default-to u0 (map-get? milestone-counters user))))
    (+ current-count u1)
  )
)

;; Create a new milestone
(define-public (create-milestone 
  (title (string-utf8 100))
  (description (string-utf8 500))
  (goal-id (optional uint))
  (target-date uint)
)
  (let 
    (
      (user tx-sender)
      (milestone-id (get-next-milestone-id user))
      (current-time block-height)
    )
    ;; Validate milestone data
    (asserts! (> (len title) u0) (err ERR_INVALID_MILESTONE))
    (asserts! (> (len description) u0) (err ERR_INVALID_MILESTONE))
    (asserts! (> target-date current-time) (err ERR_INVALID_MILESTONE))

    ;; Create milestone
    (map-set milestones 
      {owner: user, milestone-id: milestone-id}
      {
        title: title,
        description: description,
        goal-id: goal-id,
        status: STATUS_PENDING,
        target-date: target-date,
        progress-percentage: u0,
        created-at: current-time,
        updated-at: current-time
      }
    )

    ;; Update milestone counter
    (map-set milestone-counters user milestone-id)

    (ok milestone-id)
)

;; Update an existing milestone
(define-public (update-milestone
  (milestone-id uint)
  (title (optional (string-utf8 100)))
  (description (optional (string-utf8 500)))
  (goal-id (optional (optional uint)))
  (status (optional uint))
  (progress-percentage (optional uint))
  (target-date (optional uint))
)
  (let 
    (
      (user tx-sender)
      (current-milestone 
        (map-get? milestones {owner: user, milestone-id: milestone-id})
      )
    )
    ;; Milestone must exist
    (asserts! (is-some current-milestone) (err ERR_MILESTONE_NOT_FOUND))

    ;; Must be milestone owner
    (asserts! 
      (is-eq user (get owner {owner: user, milestone-id: milestone-id})) 
      (err ERR_UNAUTHORIZED)
    )

    ;; Optional updates with validation
    (map-set milestones 
      {owner: user, milestone-id: milestone-id}
      (merge 
        (unwrap-panic current-milestone)
        {
          title: (default-to (get title (unwrap-panic current-milestone)) title),
          description: (default-to (get description (unwrap-panic current-milestone)) description),
          goal-id: (default-to (get goal-id (unwrap-panic current-milestone)) goal-id),
          status: (default-to (get status (unwrap-panic current-milestone)) status),
          progress-percentage: (default-to (get progress-percentage (unwrap-panic current-milestone)) progress-percentage),
          target-date: (default-to (get target-date (unwrap-panic current-milestone)) target-date),
          updated-at: block-height
        }
      )
    )

    (ok true)
)

;; Read milestone details
(define-read-only (get-milestone (user principal) (milestone-id uint))
  (map-get? milestones {owner: user, milestone-id: milestone-id})
)

;; Calculate overall milestone progress for a goal
(define-read-only (calculate-goal-progress (user principal) (goal-id uint))
  (let 
    ((milestones-for-goal 
      (filter 
        (milestone-matches-goal goal-id) 
        (get-user-milestones user)
      )
    ))
    (if (> (len milestones-for-goal) u0)
        (/ 
          (fold 
            + 
            (map get-progress-percentage milestones-for-goal) 
            u0
          ) 
          (len milestones-for-goal)
        )
        u0
    )
)

;; Private helper functions
(define-private (milestone-matches-goal (goal-id uint))
  (lambda (milestone) 
    (is-eq (get goal-id milestone) (some goal-id))
  )
)

(define-private (get-progress-percentage (milestone (tuple 
    (owner principal)
    (milestone-id uint)
    (title (string-utf8 100))
    (description (string-utf8 500))
    (goal-id (optional uint))
    (status uint)
    (target-date uint)
    (progress-percentage uint)
    (created-at uint)
    (updated-at uint)
)))
  (get progress-percentage milestone)
)

(define-private (get-user-milestones (user principal))
  (let 
    ((total-milestones (default-to u0 (map-get? milestone-counters user))))
    (filter 
      (is-owner user) 
      (map 
        (lambda (milestone-id) 
          (unwrap-panic 
            (map-get? milestones {owner: user, milestone-id: milestone-id})
          )
        )
        (list-range u1 total-milestones)
      )
    )
)

(define-private (is-owner (user principal))
  (lambda (milestone) 
    (is-eq (get owner milestone) user)
  )
)
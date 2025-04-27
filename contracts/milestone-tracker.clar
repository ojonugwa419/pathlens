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

;; Maximum number of milestones per goal
(define-constant MAX_MILESTONES_PER_GOAL u100)

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

;; Map to track if a milestone belongs to a specific goal
;; Instead of storing a list, we store individual milestone memberships
(define-map milestone-goal-membership
  {owner: principal, goal-id: uint, milestone-id: uint}
  {is-member: bool}
)

;; Helper: Get next milestone ID for a user
(define-private (get-next-milestone-id (user principal))
  (default-to u1 (map-get? milestone-counters user))
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
    (map-set milestone-counters user (+ milestone-id u1))

    ;; If goal-id is provided, associate milestone with the goal
    (match goal-id goal-id-value
      (begin
        (map-set milestone-goal-membership
          {owner: user, goal-id: goal-id-value, milestone-id: milestone-id}
          {is-member: true}
        )
        (ok milestone-id)
      )
      (ok milestone-id)
    )
  )
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

    ;; Handle goal-id changes
    (match goal-id new-goal-id
      (begin
        ;; Get current goal-id
        (match (get goal-id (unwrap-panic current-milestone)) current-goal-id
          (begin
            ;; If goal has changed, update the membership
            (if (not (is-eq new-goal-id (some current-goal-id)))
              (begin
                ;; Remove from old goal if it exists
                (map-delete milestone-goal-membership 
                  {owner: user, goal-id: current-goal-id, milestone-id: milestone-id}
                )
                
                ;; Add to new goal if it exists
                (match new-goal-id new-goal-value
                  (map-set milestone-goal-membership
                    {owner: user, goal-id: new-goal-value, milestone-id: milestone-id}
                    {is-member: true}
                  )
                  true ;; Do nothing if new goal is none
                )
              )
              true ;; Do nothing if goal hasn't changed
            )
          )
          ;; No current goal, add to new goal if provided
          (match new-goal-id new-goal-value
            (map-set milestone-goal-membership
              {owner: user, goal-id: new-goal-value, milestone-id: milestone-id}
              {is-member: true}
            )
            true ;; Do nothing if new goal is none
          )
        )
      )
      true ;; No goal-id change requested
    )

    ;; Update milestone data
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
)

;; Read milestone details
(define-read-only (get-milestone (user principal) (milestone-id uint))
  (map-get? milestones {owner: user, milestone-id: milestone-id})
)

;; Get milestone progress percentage
(define-private (get-milestone-progress (user principal) (milestone-id uint))
  (let ((milestone (map-get? milestones {owner: user, milestone-id: milestone-id})))
    (if (is-some milestone)
        (get progress-percentage (unwrap-panic milestone))
        u0
    )
  )
)

;; Check if a milestone belongs to a goal
(define-read-only (is-milestone-in-goal (user principal) (goal-id uint) (milestone-id uint))
  (let ((membership (map-get? milestone-goal-membership {owner: user, goal-id: goal-id, milestone-id: milestone-id})))
    (if (is-some membership)
        (get is-member (unwrap-panic membership))
        false
    )
  )
)

;; Get user's total milestone count
(define-read-only (get-user-milestone-count (user principal))
  (default-to u0 (map-get? milestone-counters user))
)

;; Calculate overall milestone progress for a goal
(define-read-only (calculate-goal-progress (user principal) (goal-id uint))
  (let
    (
      (total-milestones (get-user-milestone-count user))
      (result (fold check-milestone-for-goal-progress
                   (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10
                         u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
                         u21 u22 u23 u24 u25 u26 u27 u28 u29 u30
                         u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
                         u41 u42 u43 u44 u45 u46 u47 u48 u49 u50
                         u51 u52 u53 u54 u55 u56 u57 u58 u59 u60
                         u61 u62 u63 u64 u65 u66 u67 u68 u69 u70
                         u71 u72 u73 u74 u75 u76 u77 u78 u79 u80
                         u81 u82 u83 u84 u85 u86 u87 u88 u89 u90
                         u91 u92 u93 u94 u95 u96 u97 u98 u99 u100)
                   {total: u0, count: u0, goal-id: goal-id, user: user}))
    )
    (if (> (get count result) u0)
        (/ (get total result) (get count result))
        u0
    )
  )
)

;; Helper to check if milestone belongs to goal and accumulate progress
(define-private (check-milestone-for-goal-progress
  (milestone-id uint) 
  (acc {total: uint, count: uint, goal-id: uint, user: principal})
)
  (let
    (
      (user (get user acc))
      (goal-id (get goal-id acc))
      (is-in-goal (is-milestone-in-goal user goal-id milestone-id))
    )
    (if (and 
           is-in-goal
           (<= milestone-id (get-user-milestone-count user)))
        (let
          (
            (milestone (map-get? milestones {owner: user, milestone-id: milestone-id}))
          )
          (if (is-some milestone)
              {
                total: (+ (get total acc) (get progress-percentage (unwrap-panic milestone))),
                count: (+ (get count acc) u1),
                goal-id: goal-id,
                user: user
              }
              acc
          )
        )
        acc
    )
  )
)

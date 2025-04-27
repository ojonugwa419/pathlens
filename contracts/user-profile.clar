;; PathLens User Profile Contract
;; A secure, blockchain-powered professional profile management system

;; Error Codes
(define-constant ERR_UNAUTHORIZED u403)
(define-constant ERR_PROFILE_ALREADY_EXISTS u409)
(define-constant ERR_PROFILE_NOT_FOUND u404)
(define-constant ERR_INVALID_INPUT u400)

;; Constants for validation
(define-constant MAX_NAME_LENGTH u50)
(define-constant MAX_TITLE_LENGTH u100)
(define-constant MAX_INDUSTRY_LENGTH u50)
(define-constant MAX_SKILLS_COUNT u10)
(define-constant MAX_SKILL_LENGTH u30)
(define-constant MAX_GOALS_LENGTH u250)

;; Profile Visibility Enum
(define-constant VISIBILITY_PRIVATE u0)
(define-constant VISIBILITY_PUBLIC u1)

;; User Profile Map
(define-map profiles 
  {owner: principal}
  {
    name: (string-utf8 50),
    title: (string-utf8 100),
    industry: (string-utf8 50),
    skills: (list 10 (string-utf8 30)),
    goals: (string-utf8 250),
    visibility: uint
  }
)

;; Private Function: Validate Profile Input
(define-private (validate-profile-input 
  (name (string-utf8 50))
  (title (string-utf8 100))
  (industry (string-utf8 50))
  (skills (list 10 (string-utf8 30)))
  (goals (string-utf8 250))
  (visibility uint)
)
  (begin
    (asserts! (> (len name) u0) (err ERR_INVALID_INPUT))
    (asserts! (> (len title) u0) (err ERR_INVALID_INPUT))
    (asserts! (> (len industry) u0) (err ERR_INVALID_INPUT))
    (asserts! (> (len goals) u0) (err ERR_INVALID_INPUT))
    (asserts! (or (is-eq visibility VISIBILITY_PRIVATE) (is-eq visibility VISIBILITY_PUBLIC)) (err ERR_INVALID_INPUT))
    (ok true)
  )
)

;; Create User Profile
(define-public (create-profile
  (name (string-utf8 50))
  (title (string-utf8 100))
  (industry (string-utf8 50))
  (skills (list 10 (string-utf8 30)))
  (goals (string-utf8 250))
  (visibility uint)
)
  (begin
    ;; Validate inputs
    (try! (validate-profile-input name title industry skills goals visibility))
    
    ;; Check if profile already exists
    (asserts! (is-none (map-get? profiles {owner: tx-sender})) (err ERR_PROFILE_ALREADY_EXISTS))
    
    ;; Create profile
    (map-set profiles {owner: tx-sender} {
      name: name,
      title: title,
      industry: industry,
      skills: skills,
      goals: goals,
      visibility: visibility
    })
    
    (ok true)
  )
)

;; Update User Profile
(define-public (update-profile
  (name (string-utf8 50))
  (title (string-utf8 100))
  (industry (string-utf8 50))
  (skills (list 10 (string-utf8 30)))
  (goals (string-utf8 250))
  (visibility uint)
)
  (begin
    ;; Validate inputs
    (try! (validate-profile-input name title industry skills goals visibility))
    
    ;; Ensure profile exists and is owned by tx-sender
    (asserts! (is-some (map-get? profiles {owner: tx-sender})) (err ERR_PROFILE_NOT_FOUND))
    
    ;; Update profile
    (map-set profiles {owner: tx-sender} {
      name: name,
      title: title,
      industry: industry,
      skills: skills,
      goals: goals,
      visibility: visibility
    })
    
    (ok true)
  )
)

;; Get Public Profile (respects visibility)
(define-read-only (get-profile (user principal))
  (let ((profile (map-get? profiles {owner: user})))
    (match profile
      p (if (or 
              (is-eq tx-sender user) 
              (is-eq (get visibility p) VISIBILITY_PUBLIC)
            )
            (some p)
            none
      )
      none
    )
  )
)

;; Delete Profile
(define-public (delete-profile)
  (begin
    ;; Ensure profile exists and is owned by tx-sender
    (asserts! (is-some (map-get? profiles {owner: tx-sender})) (err ERR_PROFILE_NOT_FOUND))
    
    ;; Delete profile
    (map-delete profiles {owner: tx-sender})
    
    (ok true)
  )
)

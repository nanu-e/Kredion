;; Decentralized Reputation System
;; Allows users to build, earn, and manage reputation across different domains
;; and applications while maintaining privacy and control over their reputation data

(define-map reputation-domains
  { domain-id: uint }
  {
    name: (string-utf8 64),
    description: (string-utf8 256),
    admin: principal,
    created-at: uint,
    active: bool,
    endorsement-weight: uint,       ;; Weight of endorsements (out of 100)
    activity-weight: uint,          ;; Weight of activity (out of 100)
    verification-weight: uint,      ;; Weight of verifications (out of 100)
    min-endorsements-required: uint ;; Minimum endorsements needed for reputation score
  }
)

(define-map user-reputation-data
  { domain-id: uint, user: principal }
  {
    reputation-score: uint,         ;; 0-1000 score
    endorsement-count: uint,        ;; Number of endorsements received
    activity-count: uint,           ;; Number of recorded activities
    verification-tier: uint,        ;; 0-5 verification level
    aggregate-score: uint,          ;; Sum of weighted component scores
    updated-at: uint,               ;; Block height of last update
    decay-rate: uint                ;; Rate at which reputation decays if inactive (per 1000 blocks)
  }
)

(define-map endorsements
  { domain-id: uint, endorser: principal, endorsee: principal }
  {
    weight: uint,                   ;; 1-10 weight of endorsement
    created-at: uint,               ;; When endorsement was given
    message: (optional (string-utf8 140)),  ;; Optional message
    expertise-tags: (list 5 (string-ascii 20)), ;; Categories being endorsed
    active: bool                    ;; Whether endorsement is active
  }
)

(define-map verifications
  { domain-id: uint, user: principal, verification-type: (string-ascii 32) }
  {
    verifier: principal,            ;; Who performed verification
    verified-at: uint,              ;; When verification was done
    expiration-block: (optional uint),    ;; When verification expires
    evidence-hash: (buff 32),       ;; Hash of verification evidence
    level: uint,                    ;; 1-5 verification level
    active: bool                    ;; Whether verification is active
  }
)

(define-map reputation-activities
  { domain-id: uint, activity-id: uint }
  {
    user: principal,                ;; User who performed activity
    activity-type: (string-ascii 32), ;; Type of activity
    created-at: uint,               ;; When activity was recorded
    value: uint,                    ;; Value of activity (domain-specific)
    data-hash: (buff 32),           ;; Hash of activity data
    verified: bool,                 ;; Whether activity is verified
    verified-by: (optional principal) ;; Who verified the activity
  }
)

(define-map approved-verifier-providers
  { domain-id: uint, provider: principal }
  {
    provider-name: (string-utf8 64),
    authorized-by: principal,
    authorized-at: uint,
    verification-types: (list 10 (string-ascii 32)),
    active: bool
  }
)

(define-map reputation-proxies
  { domain-id: uint, owner: principal }
  {
    proxy: principal,
    delegated-at: uint,
    expiration-block: (optional uint),
    active: bool
  }
)

(define-map privacy-settings
  { domain-id: uint, user: principal }
  {
    show-score: bool,               ;; Whether score is publicly viewable
    show-endorsements: bool,        ;; Whether endorsements are publicly viewable
    show-activities: bool,          ;; Whether activities are publicly viewable
    show-verifications: bool,       ;; Whether verifications are publicly viewable
    authorized-viewers: (list 10 principal) ;; Principals authorized to view private data
  }
)

;; Next available IDs
(define-data-var next-domain-id uint u0)
(define-map next-activity-id { domain-id: uint } { id: uint })

;; Define our own min function since it's not built-in
(define-private (get-minimum (x uint) (y uint))
  (if (<= x y) x y)
)

;; Define our own max function since we'll need it
(define-private (get-maximum (x uint) (y uint))
  (if (>= x y) x y)
)

;; Validate domain ID
(define-private (validate-domain-id (domain-id uint))
  (if (< domain-id (var-get next-domain-id))
      (ok domain-id)
      (err u"Invalid domain ID"))
)

;; Validate string-utf8-64
(define-private (validate-utf8-text-64 (val (string-utf8 64)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate string-utf8-256
(define-private (validate-utf8-text-256 (val (string-utf8 256)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate string-ascii-32
(define-private (validate-ascii-text-32 (val (string-ascii 32)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty"))
)

;; Validate weight (1-10)
(define-private (validate-weight (weight uint))
  (if (and (>= weight u1) (<= weight u10))
      (ok weight)
      (err u"Weight must be between 1 and 10"))
)

;; Validate verification level (1-5)
(define-private (validate-verification-level (level uint))
  (if (and (>= level u1) (<= level u5))
      (ok level)
      (err u"Level must be between 1 and 5"))
)

;; Check if user can view another user's private data
(define-private (can-access-private-data (domain-id uint) (owner principal) (viewer principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) false)
    
    (if (is-eq owner viewer)
        ;; Owner can always view own data
        true
        ;; Check privacy settings
        (match (map-get? privacy-settings { domain-id: domain-id, user: owner })
          settings (is-some (index-of? (get authorized-viewers settings) viewer))
          false
        )
    )
  )
)

;; Create a new reputation domain
(define-public (create-reputation-domain
                (name (string-utf8 64))
                (description (string-utf8 256))
                (endorsement-weight uint)
                (activity-weight uint)
                (verification-weight uint)
                (min-endorsements-required uint))
  (let
    ((domain-id (var-get next-domain-id))
     (validated-name-resp (validate-utf8-text-64 name))
     (validated-description-resp (validate-utf8-text-256 description)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-name-resp) 
              (err (unwrap-err! validated-name-resp (err u"Invalid name"))))
    (asserts! (is-ok validated-description-resp) 
              (err (unwrap-err! validated-description-resp (err u"Invalid description"))))
    (asserts! (< (+ (+ endorsement-weight activity-weight) verification-weight) u101) 
              (err u"Weights must sum to 100 or less"))
    (asserts! (> min-endorsements-required u0) (err u"Minimum endorsements must be greater than 0"))
    
    ;; Create the domain
    (map-set reputation-domains
      { domain-id: domain-id }
      {
        name: (unwrap-panic validated-name-resp),
        description: (unwrap-panic validated-description-resp),
        admin: tx-sender,
        created-at: block-height,
        active: true,
        endorsement-weight: endorsement-weight,
        activity-weight: activity-weight,
        verification-weight: verification-weight,
        min-endorsements-required: min-endorsements-required
      }
    )
    
    ;; Initialize activity counter
    (map-set next-activity-id
      { domain-id: domain-id }
      { id: u0 }
    )
    
    ;; Increment domain ID counter
    (var-set next-domain-id (+ domain-id u1))
    
    (ok domain-id)
  )
)

;; Endorse a user
(define-public (endorse-user
                (domain-id uint)
                (endorsee principal)
                (weight uint)
                (message (optional (string-utf8 140)))
                (expertise-tags (list 5 (string-ascii 20))))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-weight-resp (validate-weight weight)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-weight-resp) 
              (err (unwrap-err! validated-weight-resp (err u"Invalid weight"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-weight (unwrap-panic validated-weight-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                         (err u"Domain not found")))
          (existing-endorsement (map-get? endorsements 
                                { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee })))
      
      ;; Validate
      (asserts! (not (is-eq tx-sender endorsee)) (err u"Cannot endorse yourself"))
      (asserts! (get active domain) (err u"Domain not active"))
      
      ;; Create or update the endorsement
      (map-set endorsements
        { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee }
        {
          weight: validated-weight,
          created-at: block-height,
          message: message,
          expertise-tags: expertise-tags,
          active: true
        }
      )
      
      ;; Update endorsee's reputation if no previous endorsement
      (if (is-none existing-endorsement)
          (update-endorsement-count validated-domain-id endorsee u1)
          true)
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-reputation-score validated-domain-id endorsee)))
        (ok score))
    )
  )
)

;; Private helper to update endorsement count
(define-private (update-endorsement-count (domain-id uint) (user principal) (delta uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) false)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (reputation (default-to  
                       {
                         reputation-score: u0,
                         endorsement-count: u0,
                         activity-count: u0,
                         verification-tier: u0,
                         aggregate-score: u0,
                         updated-at: block-height,
                         decay-rate: u10  ;; Default 1% decay per 1000 blocks
                       }
                       (map-get? user-reputation-data { domain-id: validated-domain-id, user: user }))))
      
      (map-set user-reputation-data
        { domain-id: validated-domain-id, user: user }
        (merge reputation { endorsement-count: (+ (get endorsement-count reputation) delta) })
      )
      
      ;; Changed from (ok true) to just true since this is a private function
      true
    )
  )
)

;; Remove an endorsement
(define-public (remove-endorsement (domain-id uint) (endorsee principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (endorsement (unwrap! (map-get? endorsements 
                               { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee })
                              (err u"Endorsement not found"))))
      
      ;; Update the endorsement
      (map-set endorsements
        { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee }
        (merge endorsement { active: false })
      )
      
      ;; Update endorsee's reputation (using directly)
      (update-endorsement-count validated-domain-id endorsee (- u0 u1))
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-reputation-score validated-domain-id endorsee)))
        (ok score))
    )
  )
)

;; Record an activity for reputation
(define-public (record-activity
                (domain-id uint)
                (activity-type (string-ascii 32))
                (value uint)
                (data-hash (buff 32)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-activity-type-resp (validate-ascii-text-32 activity-type)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-activity-type-resp) 
              (err (unwrap-err! validated-activity-type-resp (err u"Invalid activity type"))))
    (asserts! (> (len data-hash) u0) (err u"Data hash cannot be empty"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-activity-type (unwrap-panic validated-activity-type-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                         (err u"Domain not found")))
          (activity-counter (unwrap! (map-get? next-activity-id { domain-id: validated-domain-id }) 
                                   (err u"Counter not found")))
          (activity-id (get id activity-counter))
          (reputation (default-to  
                       {
                         reputation-score: u0,
                         endorsement-count: u0,
                         activity-count: u0,
                         verification-tier: u0,
                         aggregate-score: u0,
                         updated-at: block-height,
                         decay-rate: u10  ;; Default 1% decay per 1000 blocks
                       }
                       (map-get? user-reputation-data { domain-id: validated-domain-id, user: tx-sender }))))
      
      ;; Validate
      (asserts! (get active domain) (err u"Domain not active"))
      
      ;; Record the activity
      (map-set reputation-activities
        { domain-id: validated-domain-id, activity-id: activity-id }
        {
          user: tx-sender,
          activity-type: validated-activity-type,
          created-at: block-height,
          value: value,
          data-hash: data-hash,
          verified: false,
          verified-by: none
        }
      )
      
      ;; Update activity counter
      (map-set next-activity-id
        { domain-id: validated-domain-id }
        { id: (+ activity-id u1) }
      )
      
      ;; Update user's activity count
      (map-set user-reputation-data
        { domain-id: validated-domain-id, user: tx-sender }
        (merge reputation { 
          activity-count: (+ (get activity-count reputation) u1),
          updated-at: block-height
        })
      )
      
      ;; Recalculate reputation score and return the activity ID
      (calculate-reputation-score validated-domain-id tx-sender)
      (ok activity-id)
    )
  )
)

;; Verify an activity (by domain admin or delegated verifier)
(define-public (verify-activity
                (domain-id uint)
                (activity-id uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                         (err u"Domain not found")))
          (activity (unwrap! (map-get? reputation-activities 
                            { domain-id: validated-domain-id, activity-id: activity-id })
                           (err u"Activity not found"))))
      
      ;; Validate
      (asserts! (or (is-eq tx-sender (get admin domain))
                   (is-authorized-verifier validated-domain-id tx-sender))
                (err u"Not authorized to verify"))
      (asserts! (not (get verified activity)) (err u"Activity already verified"))
      
      ;; Update activity
      (map-set reputation-activities
        { domain-id: validated-domain-id, activity-id: activity-id }
        (merge activity { 
          verified: true,
          verified-by: (some tx-sender)
        })
      )
      
      ;; Recalculate reputation score for the activity owner and return success
      (let ((score (calculate-reputation-score validated-domain-id (get user activity))))
        (ok score))
    )
  )
)

;; Check if principal is a delegated verifier
(define-private (is-authorized-verifier (domain-id uint) (provider principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
          (default-to 
            false 
            (get active (map-get? approved-verifier-providers 
                        { domain-id: validated-domain-id, provider: provider }))
          )
        )
        false
    )
  )
)

;; Add verification for a user
(define-public (add-verification
    (domain-id uint)
    (user principal)
    (verification-type (string-ascii 32))
    (evidence-hash (buff 32))
    (level uint)
    (expiration-block (optional uint)))
    (let ((validated-domain-id-resp (validate-domain-id domain-id))
          (validated-verification-type-resp (validate-ascii-text-32 verification-type))
          (validated-level-resp (validate-verification-level level)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-domain-id-resp) 
                  (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
        (asserts! (is-ok validated-verification-type-resp) 
                  (err (unwrap-err! validated-verification-type-resp (err u"Invalid verification type"))))
        (asserts! (is-ok validated-level-resp) 
                  (err (unwrap-err! validated-level-resp (err u"Invalid level"))))
        (asserts! (> (len evidence-hash) u0) (err u"Evidence hash cannot be empty"))
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (validated-verification-type (unwrap-panic validated-verification-type-resp))
              (validated-level (unwrap-panic validated-level-resp))
              (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                             (err u"Domain not found")))
              (reputation (default-to {
                  reputation-score: u0,
                  endorsement-count: u0,
                  activity-count: u0,
                  verification-tier: u0,
                  aggregate-score: u0,
                  updated-at: block-height,
                  decay-rate: u10
              } (map-get? user-reputation-data { domain-id: validated-domain-id, user: user })))
              (current-verification-tier (get verification-tier reputation)))
            
            ;; Validate
            (asserts! (or (is-eq tx-sender (get admin domain))
                         (is-authorized-verifier validated-domain-id tx-sender))
                      (err u"Not authorized to verify"))
            
            ;; Add verification
            (map-set verifications
                { domain-id: validated-domain-id, user: user, verification-type: validated-verification-type }
                {
                    verifier: tx-sender,
                    verified-at: block-height,
                    expiration-block: expiration-block,
                    evidence-hash: evidence-hash,
                    level: validated-level,
                    active: true
                }
            )
            
            ;; Update user's verification level (take highest verification level)
            (map-set user-reputation-data
                { domain-id: validated-domain-id, user: user }
                (merge reputation { 
                    verification-tier: (get-maximum current-verification-tier validated-level),
                    updated-at: block-height
                })
            )
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-reputation-score validated-domain-id user)))
                (ok score))
        )
    )
)

(define-public (revoke-verification
    (domain-id uint)
    (user principal)
    (verification-type (string-ascii 32)))
    (let ((validated-domain-id-resp (validate-domain-id domain-id))
          (validated-verification-type-resp (validate-ascii-text-32 verification-type)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-domain-id-resp) 
                  (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
        (asserts! (is-ok validated-verification-type-resp) 
                  (err (unwrap-err! validated-verification-type-resp (err u"Invalid verification type"))))
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (validated-verification-type (unwrap-panic validated-verification-type-resp))
              (verification (unwrap! (map-get? verifications 
                                    { domain-id: validated-domain-id, user: user, verification-type: validated-verification-type })
                                    (err u"Verification not found"))))
            
            ;; Validate
            (asserts! (is-eq tx-sender (get verifier verification)) 
                      (err u"Only verifier can revoke"))
            
            ;; Update verification
            (map-set verifications
                { domain-id: validated-domain-id, user: user, verification-type: validated-verification-type }
                (merge verification { active: false })
            )
            
            ;; Recalculate highest verification level
            (recalculate-verification-tier validated-domain-id user)
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-reputation-score validated-domain-id user)))
                (ok score))
        )
    )
)

(define-private (recalculate-verification-tier 
    (domain-id uint) 
    (user principal))
    (let ((validated-domain-id-resp (validate-domain-id domain-id)))
        (asserts! (is-ok validated-domain-id-resp) false)
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (reputation (default-to {
                  reputation-score: u0,
                  endorsement-count: u0,
                  activity-count: u0,
                  verification-tier: u0,
                  aggregate-score: u0,
                  updated-at: block-height,
                  decay-rate: u10
              } (map-get? user-reputation-data { domain-id: validated-domain-id, user: user }))))
            
            ;; For this example, we'll just reset to level 0
            (map-set user-reputation-data
                { domain-id: validated-domain-id, user: user }
                (merge reputation { verification-tier: u0 })
            )
            
            true
        )
    )
)

;; Calculate reputation score
(define-private (calculate-reputation-score (domain-id uint) (user principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) u0)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap-panic (map-get? reputation-domains { domain-id: validated-domain-id })))
          (reputation (unwrap-panic (map-get? user-reputation-data { domain-id: validated-domain-id, user: user })))
         
          ;; Calculate component scores
          (endorsement-score (calculate-endorsement-score validated-domain-id user 
                                                       (get endorsement-count reputation)))
          (activity-score (calculate-activity-score validated-domain-id user 
                                                 (get activity-count reputation)))
          (verification-score (calculate-verification-score validated-domain-id user 
                                                         (get verification-tier reputation)))
         
          ;; Calculate weighted scores
          (weighted-endorsement (/ (* endorsement-score (get endorsement-weight domain)) u100))
          (weighted-activity (/ (* activity-score (get activity-weight domain)) u100))
          (weighted-verification (/ (* verification-score (get verification-weight domain)) u100))
         
          ;; Calculate total score
          (total-aggregate (+ (+ weighted-endorsement weighted-activity) weighted-verification))
          (decayed-score (apply-decay total-aggregate reputation)))
        
        ;; Update reputation score
        (map-set user-reputation-data
          { domain-id: validated-domain-id, user: user }
          (merge reputation { 
            reputation-score: decayed-score,
            aggregate-score: total-aggregate,
            updated-at: block-height
          })
        )
        
        ;; Return the score directly, not a response
        decayed-score
    )
  )
)

;; Calculate endorsement score component (0-1000)
(define-private (calculate-endorsement-score (domain-id uint) (user principal) (endorsement-count uint))
  ;; In a real implementation, this would be a more complex calculation
  ;; For this example, we'll use a simple scaling function
  
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) u0)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap-panic (map-get? reputation-domains { domain-id: validated-domain-id })))
          (min-required (get min-endorsements-required domain)))
        
        (if (< endorsement-count min-required)
            ;; Below minimum: score = (count / min-required) * 500
            (/ (* endorsement-count u500) min-required)
            ;; Above minimum: score = 500 + (count - min-required) * 50, max 1000
            (get-minimum u1000 (+ u500 (* (- endorsement-count min-required) u50)))
        )
    )
  )
)

;; Calculate activity score component (0-1000)
(define-private (calculate-activity-score (domain-id uint) (user principal) (activity-count uint))
  ;; Simplified calculation
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (get-minimum u1000 (* activity-count u100))
        u0
    )
  )
)

;; Calculate verification score component (0-1000)
(define-private (calculate-verification-score (domain-id uint) (user principal) (verification-tier uint))
  ;; Level 0-5 scaled to 0-1000
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (* verification-tier u200)
        u0
    )
  )
)

;; Apply decay to reputation score based on last update time
(define-private (apply-decay (score uint) (reputation (tuple 
                                           (reputation-score uint)
                                           (endorsement-count uint)
                                           (activity-count uint)
                                           (verification-tier uint)
                                           (aggregate-score uint)
                                           (updated-at uint)
                                           (decay-rate uint))))
  (let
    ((blocks-since-update (- block-height (get updated-at reputation)))
     (decay-periods (/ blocks-since-update u1000))
     (decay-rate (get decay-rate reputation)))
    
    (if (or (is-eq decay-periods u0) (is-eq decay-rate u0))
        ;; No decay
        score
        ;; Apply decay: score * (1 - decay-rate/100)^decay-periods
        ;; Simplified calculation
        (- score (/ (* score (* decay-periods decay-rate)) u1000))
    )
  )
)

;; Add a delegated verification provider
(define-public (add-verifier-provider
                (domain-id uint)
                (provider principal)
                (provider-name (string-utf8 64))
                (verification-types (list 10 (string-ascii 32))))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-provider-name-resp (validate-utf8-text-64 provider-name)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-provider-name-resp) 
              (err (unwrap-err! validated-provider-name-resp (err u"Invalid provider name"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-provider-name (unwrap-panic validated-provider-name-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                         (err u"Domain not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get admin domain)) (err u"Only domain admin can add providers"))
      
      ;; Add provider
      (map-set approved-verifier-providers
        { domain-id: validated-domain-id, provider: provider }
        {
          provider-name: validated-provider-name,
          authorized-by: tx-sender,
          authorized-at: block-height,
          verification-types: verification-types,
          active: true
        }
      )
      
      (ok true)
    )
  )
)

;; Revoke a verification provider
(define-public (revoke-verifier-provider (domain-id uint) (provider principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) 
                         (err u"Domain not found")))
          (provider-data (unwrap! (map-get? approved-verifier-providers 
                                 { domain-id: validated-domain-id, provider: provider })
                                (err u"Provider not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get admin domain)) (err u"Only domain admin can revoke providers"))
      
      ;; Update provider
      (map-set approved-verifier-providers
        { domain-id: validated-domain-id, provider: provider }
        (merge provider-data { active: false })
      )
      
      (ok true)
    )
  )
)

;; Delegate reputation management to another principal
(define-public (delegate-reputation (domain-id uint) (proxy principal) (expiration-block (optional uint)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (map-set reputation-proxies
        { domain-id: validated-domain-id, owner: tx-sender }
        {
          proxy: proxy,
          delegated-at: block-height,
          expiration-block: expiration-block,
          active: true
        }
      )
      
      (ok true)
    )
  )
)

;; Remove reputation delegation
(define-public (remove-delegation (domain-id uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (delegation (unwrap! (map-get? reputation-proxies 
                              { domain-id: validated-domain-id, owner: tx-sender })
                             (err u"Delegation not found"))))
      
      (map-set reputation-proxies
        { domain-id: validated-domain-id, owner: tx-sender }
        (merge delegation { active: false })
      )
      
      (ok true)
    )
  )
)

;; Update privacy settings
(define-public (update-privacy-settings
                (domain-id uint)
                (show-score bool)
                (show-endorsements bool)
                (show-activities bool)
                (show-verifications bool)
                (authorized-viewers (list 10 principal)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp) 
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (map-set privacy-settings
        { domain-id: validated-domain-id, user: tx-sender }
        {
          show-score: show-score,
          show-endorsements: show-endorsements,
          show-activities: show-activities,
          show-verifications: show-verifications,
          authorized-viewers: authorized-viewers
        }
      )
      
      (ok true)
    )
  )
)

;; Read-only functions

;; Get domain details
(define-read-only (get-domain-details (domain-id uint))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) (err u"Domain not found")))
    )
  )
)

;; Get user reputation score
(define-read-only (get-reputation-score (domain-id uint) (user principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (reputation (map-get? user-reputation-data { domain-id: validated-domain-id, user: user }))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, user: user })))
      
      (if (is-none reputation)
          (err u"Reputation not found")
          (if (or (is-none privacy)
                  (get show-score (unwrap-panic privacy))
                  (can-access-private-data validated-domain-id user tx-sender))
              (ok (get reputation-score (unwrap-panic reputation)))
              (err u"Not authorized to view score")
          )
      )
    )
  )
)

;; Get user endorsements
(define-read-only (get-user-endorsements (domain-id uint) (user principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, user: user })))
      
      (if (or (is-none privacy)
              (get show-endorsements (unwrap-panic privacy))
              (can-access-private-data validated-domain-id user tx-sender))
          (ok u"Endorsements would be returned here")
          (err u"Not authorized to view endorsements")
      )
    )
  )
)

;; Get verification details
(define-read-only (get-verification (domain-id uint) (user principal) (verification-type (string-ascii 32)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-verification-type-resp (validate-ascii-text-32 verification-type)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    (asserts! (is-ok validated-verification-type-resp) (err u"Invalid verification type"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-verification-type (unwrap-panic validated-verification-type-resp))
          (verification (map-get? verifications { domain-id: validated-domain-id, user: user, verification-type: validated-verification-type }))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, user: user })))
      
      (if (is-none verification)
          (err u"Verification not found")
          (if (or (is-none privacy)
                  (get show-verifications (unwrap-panic privacy))
                  (can-access-private-data validated-domain-id user tx-sender))
              (ok (unwrap-panic verification))
              (err u"Not authorized to view verification")
          )
      )
    )
  )
)

;; Check if a provider is authorized for verification
(define-read-only (is-provider-authorized (domain-id uint) (provider principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (is-authorized-verifier validated-domain-id provider))
    )
  )
)

;; Get privacy settings
(define-read-only (get-privacy-settings (domain-id uint) (user principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (default-to 
            {
              show-score: true,
              show-endorsements: true,
              show-activities: false,
              show-verifications: false,
              authorized-viewers: (list)
            }
            (map-get? privacy-settings { domain-id: validated-domain-id, user: user })
          )
      )
    )
  )
)
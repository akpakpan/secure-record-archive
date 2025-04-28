;; Secure Record Archive Solution
;; A comprehensive platform for storing, managing, and sharing digital records securely
;; Developed for blockchain-based information management and access control

;; Error Definition Section
;; These constants represent various error conditions that may occur during contract execution
(define-constant ERR_ACCESS_DENIED (err u100))
(define-constant ERR_BAD_FORMAT (err u101))
(define-constant ERR_ARCHIVE_MISSING (err u102))
(define-constant ERR_DUPLICATE_ARCHIVE (err u103))
(define-constant ERR_DESCRIPTION_INVALID (err u104))
(define-constant ERR_NO_RIGHTS (err u105))
(define-constant ERR_TIME_CONSTRAINT (err u106))
(define-constant ERR_PRIVILEGE_LEVEL (err u107))
(define-constant ERR_GROUP_INVALID (err u108))
(define-constant PLATFORM_ADMINISTRATOR tx-sender)

;; Permission Levels for Shared Access
;; These define the possible access permissions that can be granted to other users
(define-constant PERMISSION_VIEW "read")
(define-constant PERMISSION_EDIT "write")
(define-constant PERMISSION_FULL "admin")

;; Global State Variables
;; Track the total number of archives in the system
(define-data-var archive-counter uint u0)

;; Primary Data Structures
;; The main storage map for all archived materials in the system
(define-map archive-storage
    { archive-id: uint }
    {
        heading: (string-ascii 50),
        creator: principal,
        digest: (string-ascii 64),
        description: (string-ascii 200),
        timestamp: uint,
        update-timestamp: uint,
        classification: (string-ascii 20),
        tags: (list 5 (string-ascii 30))
    }
)

;; Access Control Map - Tracks who has been granted access to which archives
(define-map archive-access-control
    { archive-id: uint, user: principal }
    {
        permission-type: (string-ascii 10),
        granted-time: uint,
        valid-until: uint,
        edit-allowed: bool
    }
)

;; ===== Input Validation Functions =====
;; Ensures proper formatting and validity of all user inputs

;; Validates the archive heading length and character requirements
(define-private (valid-heading? (heading (string-ascii 50)))
    (and
        (> (len heading) u0)
        (<= (len heading) u50)
    )
)

;; Ensures the digest hash meets the required format specifications
(define-private (valid-digest? (digest (string-ascii 64)))
    (and
        (is-eq (len digest) u64)
        (> (len digest) u0)
    )
)

;; Validates all tags meet the system requirements
(define-private (valid-tags? (tag-list (list 5 (string-ascii 30))))
    (and
        (>= (len tag-list) u1)
        (<= (len tag-list) u5)
        (is-eq (len (filter valid-tag? tag-list)) (len tag-list))
    )
)

;; Validates a single tag for proper formatting
(define-private (valid-tag? (tag (string-ascii 30)))
    (and
        (> (len tag) u0)
        (<= (len tag) u30)
    )
)

;; Checks if the description meets system requirements
(define-private (valid-description? (description (string-ascii 200)))
    (and
        (>= (len description) u1)
        (<= (len description) u200)
    )
)

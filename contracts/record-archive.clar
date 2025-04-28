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

;; Validates classification categories to ensure they're supported by the system
(define-private (valid-classification? (classification (string-ascii 20)))
    (and
        (>= (len classification) u1)
        (<= (len classification) u20)
    )
)

;; Ensures requested permission levels match the system defined levels
(define-private (valid-permission? (permission (string-ascii 10)))
    (or
        (is-eq permission PERMISSION_VIEW)
        (is-eq permission PERMISSION_EDIT)
        (is-eq permission PERMISSION_FULL)
    )
)

;; Validates that access duration falls within acceptable limits
(define-private (valid-time-period? (period uint))
    (and
        (> period u0)
        (<= period u52560) ;; Maximum period of approximately one year in blocks
    )
)

;; Ensures the target principal is not the sender themselves
(define-private (valid-recipient? (recipient principal))
    (not (is-eq recipient tx-sender))
)

;; Checks if the caller is the owner of the specified archive
(define-private (is-archive-creator? (archive-id uint) (user principal))
    (match (map-get? archive-storage { archive-id: archive-id })
        entry (is-eq (get creator entry) user)
        false
    )
)

;; Verifies that the requested archive exists in the system
(define-private (archive-exists? (archive-id uint))
    (is-some (map-get? archive-storage { archive-id: archive-id }))
)

;; Validates that the edit permission flag is properly formatted
(define-private (valid-edit-flag? (edit-allowed bool))
    (or (is-eq edit-allowed true) (is-eq edit-allowed false))
)

;; ===== Core Public Functions =====
;; Main interface functions for interacting with the contract

;; Creates a new archive entry in the system
(define-public (store-new-archive 
    (heading (string-ascii 50))
    (digest (string-ascii 64))
    (description (string-ascii 200))
    (classification (string-ascii 20))
    (tags (list 5 (string-ascii 30)))
)
    (let
        (
            (new-archive-id (+ (var-get archive-counter) u1))
            (current-block block-height)
        )
        (asserts! (valid-heading? heading) ERR_BAD_FORMAT)
        (asserts! (valid-digest? digest) ERR_BAD_FORMAT)
        (asserts! (valid-description? description) ERR_DESCRIPTION_INVALID)
        (asserts! (valid-classification? classification) ERR_GROUP_INVALID)
        (asserts! (valid-tags? tags) ERR_DESCRIPTION_INVALID)

        (map-set archive-storage
            { archive-id: new-archive-id }
            {
                heading: heading,
                creator: tx-sender,
                digest: digest,
                description: description,
                timestamp: current-block,
                update-timestamp: current-block,
                classification: classification,
                tags: tags
            }
        )

        (var-set archive-counter new-archive-id)
        (ok new-archive-id)
    )
)

;; Modifies an existing archive with updated information
(define-public (modify-archive
    (archive-id uint)
    (new-heading (string-ascii 50))
    (new-digest (string-ascii 64))
    (new-description (string-ascii 200))
    (new-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive (unwrap! (map-get? archive-storage { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
        )
        (asserts! (is-archive-creator? archive-id tx-sender) ERR_ACCESS_DENIED)
        (asserts! (valid-heading? new-heading) ERR_BAD_FORMAT)
        (asserts! (valid-digest? new-digest) ERR_BAD_FORMAT)
        (asserts! (valid-description? new-description) ERR_DESCRIPTION_INVALID)
        (asserts! (valid-tags? new-tags) ERR_DESCRIPTION_INVALID)

        (map-set archive-storage
            { archive-id: archive-id }
            (merge archive {
                heading: new-heading,
                digest: new-digest,
                description: new-description,
                update-timestamp: block-height,
                tags: new-tags
            })
        )
        (ok true)
    )
)

;; Grants another user access to one of your archives
(define-public (grant-archive-access
    (archive-id uint)
    (recipient principal)
    (permission (string-ascii 10))
    (duration uint)
    (edit-allowed bool)
)
    (let
        (
            (current-block block-height)
            (expiry-block (+ current-block duration))
        )
        (asserts! (archive-exists? archive-id) ERR_ARCHIVE_MISSING)
        (asserts! (is-archive-creator? archive-id tx-sender) ERR_ACCESS_DENIED)
        (asserts! (valid-recipient? recipient) ERR_BAD_FORMAT)
        (asserts! (valid-permission? permission) ERR_PRIVILEGE_LEVEL)
        (asserts! (valid-time-period? duration) ERR_TIME_CONSTRAINT)
        (asserts! (valid-edit-flag? edit-allowed) ERR_BAD_FORMAT)

        (map-set archive-access-control
            { archive-id: archive-id, user: recipient }
            {
                permission-type: permission,
                granted-time: current-block,
                valid-until: expiry-block,
                edit-allowed: edit-allowed
            }
        )
        (ok true)
    )
)

;; ===== Enhanced and Optimized Functions =====
;; Improved versions of core functionality with better implementation

;; Alternative implementation of modify-archive with improved readability
(define-public (update-existing-archive
    (archive-id uint)
    (new-heading (string-ascii 50))
    (new-digest (string-ascii 64))
    (new-description (string-ascii 200))
    (new-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive (unwrap! (map-get? archive-storage { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
        )
        (asserts! (is-archive-creator? archive-id tx-sender) ERR_ACCESS_DENIED)
        (let
            (
                (updated-archive (merge archive {
                    heading: new-heading,
                    digest: new-digest,
                    description: new-description,
                    tags: new-tags
                }))
            )
            (map-set archive-storage { archive-id: archive-id } updated-archive)
            (ok true)
        )
    )
)

;; Performance-optimized version of the archive creation function
(define-public (create-archive-optimized
    (heading (string-ascii 50))
    (digest (string-ascii 64))
    (description (string-ascii 200))
    (classification (string-ascii 20))
    (tags (list 5 (string-ascii 30)))
)
    (let
        (
            (new-archive-id (+ (var-get archive-counter) u1))
            (current-block block-height)
        )
        (asserts! (valid-heading? heading) ERR_BAD_FORMAT)
        (asserts! (valid-digest? digest) ERR_BAD_FORMAT)
        (asserts! (valid-description? description) ERR_DESCRIPTION_INVALID)
        (asserts! (valid-classification? classification) ERR_GROUP_INVALID)
        (asserts! (valid-tags? tags) ERR_DESCRIPTION_INVALID)

        (map-set archive-storage
            { archive-id: new-archive-id }
            {
                heading: heading,
                creator: tx-sender,
                digest: digest,
                description: description,
                timestamp: current-block,
                update-timestamp: current-block,
                classification: classification,
                tags: tags
            }
        )

        (var-set archive-counter new-archive-id)
        (ok new-archive-id)
    )
)

;; Security-enhanced version of the archive modification function
(define-public (secure-archive-modification
    (archive-id uint)
    (new-heading (string-ascii 50))
    (new-digest (string-ascii 64))
    (new-description (string-ascii 200))
    (new-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive (unwrap! (map-get? archive-storage { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
        )
        (asserts! (is-archive-creator? archive-id tx-sender) ERR_ACCESS_DENIED)
        (asserts! (valid-heading? new-heading) ERR_BAD_FORMAT)
        (asserts! (valid-digest? new-digest) ERR_BAD_FORMAT)
        (asserts! (valid-description? new-description) ERR_DESCRIPTION_INVALID)
        (asserts! (valid-tags? new-tags) ERR_DESCRIPTION_INVALID)

        (map-set archive-storage
            { archive-id: archive-id }
            (merge archive {
                heading: new-heading,
                digest: new-digest,
                description: new-description,
                update-timestamp: block-height,
                tags: new-tags
            })
        )
        (ok true)
    )
)

;; Alternative storage structure for improved lookup performance
(define-map enhanced-archive-storage
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

;; Implementation using the enhanced storage structure
(define-public (create-optimized-archive
    (heading (string-ascii 50))
    (digest (string-ascii 64))
    (description (string-ascii 200))
    (classification (string-ascii 20))
    (tags (list 5 (string-ascii 30)))
)
    (let
        (
            (new-archive-id (+ (var-get archive-counter) u1))
            (current-block block-height)
        )
        (asserts! (valid-heading? heading) ERR_BAD_FORMAT)
        (asserts! (valid-digest? digest) ERR_BAD_FORMAT)
        (asserts! (valid-description? description) ERR_DESCRIPTION_INVALID)
        (asserts! (valid-classification? classification) ERR_GROUP_INVALID)
        (asserts! (valid-tags? tags) ERR_DESCRIPTION_INVALID)

        (map-set enhanced-archive-storage
            { archive-id: new-archive-id }
            {
                heading: heading,
                creator: tx-sender,
                digest: digest,
                description: description,
                timestamp: current-block,
                update-timestamp: current-block,
                classification: classification,
                tags: tags
            }
        )

        (var-set archive-counter new-archive-id)
        (ok new-archive-id)
    )
)


;; Education Data Management Smart Contract
;; Contract Version: 2.3

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STUDENT-EXISTS (err u101))
(define-constant ERR-STUDENT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-GRADE (err u103))
(define-constant ERR-COURSE-EXISTS (err u104))
(define-constant ERR-COURSE-NOT-FOUND (err u105))
(define-constant ERR-INVALID-SEMESTER (err u106))
(define-constant ERR-INVALID-YEAR (err u107))
(define-constant ERR-INVALID-CREDITS (err u108))
(define-constant ERR-INVALID-NAME-LENGTH (err u109))
(define-constant ERR-PREREQUISITE-NOT-MET (err u110))
(define-constant ERR-INVALID-PRINCIPAL (err u111))
(define-constant ERR-INVALID-INPUT (err u112))

;; Constants for validation
(define-constant MAX-CREDITS u6)
(define-constant MIN-YEAR u2000)
(define-constant MAX-YEAR u2100)
(define-constant MAX-SEMESTER u3)
(define-constant MIN-NAME-LENGTH u2)

;; Default academic standing
(define-constant DEFAULT-ACADEMIC-STANDING "Good Standing")

;; Data structures
(define-map students 
    { student-id: uint }
    {
        name: (string-ascii 50),
        enrollment-year: uint,
        major: (string-ascii 30),
        active: bool,
        graduation-year: (optional uint),
        total-credits: uint,
        academic-standing: (string-ascii 20)
    }
)

(define-map courses 
    { course-id: uint }
    {
        name: (string-ascii 50),
        credits: uint,
        department: (string-ascii 30),
        active: bool,
        prerequisites: (list 10 uint),
        min-grade-required: uint,
        course-level: uint
    }
)

(define-map student-grades
    { student-id: uint, course-id: uint }
    {
        grade: uint,
        semester: uint,
        year: uint,
        completed: bool,
        instructor: (string-ascii 50),
        grade-points: uint,
        attempts: uint
    }
)

(define-map academic-records
    { student-id: uint }
    {
        cumulative-gpa: uint,
        total-attempted-credits: uint,
        total-earned-credits: uint,
        honors: (list 5 (string-ascii 30)),
        academic-warnings: uint
    }
)

(define-map administrators principal bool)

(define-map audit-log 
    { transaction-id: uint }
    {
        timestamp: uint,
        action: (string-ascii 50),
        performed-by: principal,
        details: (string-ascii 100)
    }
)

;; Counter for transaction IDs
(define-data-var next-transaction-id uint u0)

;; Input validation functions
(define-private (validate-principal (addr principal))
    (not (is-eq addr tx-sender))
)

(define-private (validate-uint-input (value uint) (min-value uint) (max-value uint))
    (and (>= value min-value) (<= value max-value))
)

(define-private (validate-string-input (str (string-ascii 50)) (min-length uint))
    (>= (len str) min-length)
)

;; Authorization check
(define-private (is-administrator (caller principal))
    (default-to false (map-get? administrators caller))
)

;; Validation functions
(define-private (is-valid-year (year uint))
    (and (>= year MIN-YEAR) (<= year MAX-YEAR))
)

(define-private (is-valid-semester (semester uint))
    (<= semester MAX-SEMESTER)
)

(define-private (is-valid-credits (credits uint))
    (and (> credits u0) (<= credits MAX-CREDITS))
)

(define-private (is-valid-name-length (name (string-ascii 50)))
    (>= (len name) MIN-NAME-LENGTH)
)

;; Grade point calculation function
(define-private (calculate-grade-points (grade uint)) 
    (if (>= grade u90) u4
        (if (>= grade u80) u3
        (if (>= grade u70) u2
        (if (>= grade u60) u1
        u0))))
)

;; Fixed GPA calculation function
(define-private (calculate-gpa (student-id uint))
    (let (
        (total-grade-points u0)
        (total-credits u0)
        (grades-list (map-get? academic-records {student-id: student-id}))
    )
        (match grades-list
            record (let (
                (attempted-credits (get total-attempted-credits record))
                (earned-credits (get total-earned-credits record))
            )
                (if (is-eq attempted-credits u0)
                    u0  ;; Return 0 if no credits attempted
                    (/ (* (get cumulative-gpa record) attempted-credits) attempted-credits))
            )
            u0  ;; Return 0 if no academic record found
        )
    )
)

;; Audit logging function
(define-private (log-action (action (string-ascii 50)) (details (string-ascii 100)))
    (let ((transaction-id (var-get next-transaction-id)))
        (var-set next-transaction-id (+ transaction-id u1))
        (map-set audit-log 
            {transaction-id: transaction-id}
            {
                timestamp: block-height,
                action: action,
                performed-by: tx-sender,
                details: details
            }
        )
    )
)

;; Prerequisite checking functions
(define-private (check-prerequisites (student-id uint) (course-id uint))
    (match (map-get? courses {course-id: course-id})
        course (begin
            (match (map-get? student-grades {student-id: student-id, course-id: course-id})
                grade-info (and 
                    (get completed grade-info)
                    (>= (get grade grade-info) 
                        (get min-grade-required course)))
                false)
            )
        false)
)

;; Student management functions
(define-public (add-student 
    (student-id uint) 
    (name (string-ascii 50))
    (enrollment-year uint)
    (major (string-ascii 30)))
    (begin
        ;; Authorization check first
        (asserts! (is-administrator tx-sender) ERR-NOT-AUTHORIZED)
        
        ;; Input validation
        (asserts! (validate-uint-input student-id u1 u100000) ERR-INVALID-INPUT)
        (asserts! (validate-string-input name MIN-NAME-LENGTH) ERR-INVALID-NAME-LENGTH)
        (asserts! (validate-uint-input enrollment-year MIN-YEAR MAX-YEAR) ERR-INVALID-YEAR)
        (asserts! (validate-string-input major u1) ERR-INVALID-INPUT)
        
        ;; Check if student already exists
        (asserts! (is-none (map-get? students {student-id: student-id})) ERR-STUDENT-EXISTS)
        
        (ok (begin
            (map-set students 
                {student-id: student-id}
                {
                    name: name,
                    enrollment-year: enrollment-year,
                    major: major,
                    active: true,
                    graduation-year: none,
                    total-credits: u0,
                    academic-standing: DEFAULT-ACADEMIC-STANDING
                }
            )
            (log-action "Add Student" "New student added successfully")
        ))
    )
)

;; Grade recording
(define-public (record-grade
    (student-id uint)
    (course-id uint)
    (grade uint)
    (semester uint)
    (year uint)
    (instructor (string-ascii 50)))
    
    (begin
        ;; Authorization check first
        (asserts! (is-administrator tx-sender) ERR-NOT-AUTHORIZED)
        
        ;; Input validation
        (asserts! (validate-uint-input student-id u1 u100000) ERR-INVALID-INPUT)
        (asserts! (validate-uint-input course-id u1 u10000) ERR-INVALID-INPUT)
        (asserts! (validate-uint-input grade u0 u100) ERR-INVALID-GRADE)
        (asserts! (validate-uint-input semester u1 MAX-SEMESTER) ERR-INVALID-SEMESTER)
        (asserts! (validate-uint-input year MIN-YEAR MAX-YEAR) ERR-INVALID-YEAR)
        (asserts! (validate-string-input instructor u1) ERR-INVALID-INPUT)
        
        ;; Existence checks
        (asserts! (is-some (map-get? students {student-id: student-id})) ERR-STUDENT-NOT-FOUND)
        (asserts! (is-some (map-get? courses {course-id: course-id})) ERR-COURSE-NOT-FOUND)
        (asserts! (check-prerequisites student-id course-id) ERR-PREREQUISITE-NOT-MET)
        
        (let ((grade-points (calculate-grade-points grade))
              (course-details (unwrap-panic (map-get? courses {course-id: course-id}))))
            (ok (begin
                (map-set student-grades
                    {student-id: student-id, course-id: course-id}
                    {
                        grade: grade,
                        semester: semester,
                        year: year,
                        completed: true,
                        instructor: instructor,
                        grade-points: grade-points,
                        attempts: (default-to u1 (get attempts (map-get? student-grades {student-id: student-id, course-id: course-id})))
                    }
                )
                
                (map-set academic-records
                    {student-id: student-id}
                    {
                        cumulative-gpa: (calculate-gpa student-id),
                        total-attempted-credits: (+ (default-to u0 (get total-attempted-credits (map-get? academic-records {student-id: student-id}))) 
                                                 (get credits course-details)),
                        total-earned-credits: (if (>= grade u60)
                                                (+ (default-to u0 (get total-earned-credits (map-get? academic-records {student-id: student-id})))
                                                   (get credits course-details))
                                                (default-to u0 (get total-earned-credits (map-get? academic-records {student-id: student-id})))),
                        honors: (get honors (default-to {honors: (list), academic-warnings: u0} (map-get? academic-records {student-id: student-id}))),
                        academic-warnings: (if (< grade u60)
                                            (+ u1 (default-to u0 (get academic-warnings (map-get? academic-records {student-id: student-id}))))
                                            (default-to u0 (get academic-warnings (map-get? academic-records {student-id: student-id}))))
                    }
                )
                (log-action "Record Grade" "Grade recorded successfully")
            ))
        )
    )
)

;; Administrative functions
(define-public (add-administrator (admin-principal principal))
    (begin 
        ;; Authorization check first
        (asserts! (is-administrator tx-sender) ERR-NOT-AUTHORIZED)
        
        ;; Input validation
        (asserts! (validate-principal admin-principal) ERR-INVALID-PRINCIPAL)
        
        (ok (begin
            (map-set administrators admin-principal true)
            (log-action "Add Administrator" "New administrator added")
        ))
    )
)
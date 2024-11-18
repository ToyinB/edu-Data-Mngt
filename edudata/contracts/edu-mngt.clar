;; Education Data Management Smart Contract
;; Contract Version: 2.0
;; 
;; This smart contract manages educational data on the Stacks blockchain including:
;; - Student records and personal information
;; - Course catalog and department information
;; - Grade records and academic performance tracking
;; - Administrative access control and permissions
;; - GPA calculations and academic metrics
;;
;; Security Features:
;; - Role-based access control
;; - Input validation
;; - Data integrity checks
;; - Audit logging

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

;; Constants for validation
(define-constant MAX-CREDITS u6)
(define-constant MIN-YEAR u2000)
(define-constant MAX-YEAR u2100)
(define-constant MAX-SEMESTER u3)
(define-constant MIN-NAME-LENGTH u2)

;; Data structures
(define-map students 
    { student-id: uint }
    {
        name: (string-utf8 50),
        enrollment-year: uint,
        major: (string-utf8 30),
        active: bool,
        graduation-year: (optional uint),
        total-credits: uint,
        academic-standing: (string-utf8 20)
    }
)

(define-map courses 
    { course-id: uint }
    {
        name: (string-utf8 50),
        credits: uint,
        department: (string-utf8 30),
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
        instructor: (string-utf8 50),
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
        honors: (list 5 (string-utf8 30)),
        academic-warnings: uint
    }
)

(define-map administrators (string-ascii 42) bool)

;; Audit logging
(define-map audit-log 
    { transaction-id: uint }
    {
        timestamp: uint,
        action: (string-utf8 50),
        performed-by: principal,
        details: (string-utf8 100)
    }
)

;; Authorization check
(define-private (is-administrator (caller principal))
    (default-to false (map-get? administrators (principal-to-string caller)))
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

(define-private (is-valid-name-length (name (string-utf8 50)))
    (>= (len name) MIN-NAME-LENGTH)
)

(define-private (check-prerequisites (student-id uint) (course-id uint))
    (let (
        (course (unwrap-panic (map-get? courses {course-id: course-id})))
        (prerequisites (get prerequisites course))
        )
        (fold check-prerequisite prerequisites true)
    )
)

(define-private (check-prerequisite (prereq-id uint) (prev-result bool))
    (let (
        (grade-info (map-get? student-grades {student-id: student-id, course-id: prereq-id}))
        )
        (and 
            prev-result
            (is-some grade-info)
            (>= (get grade (unwrap-panic grade-info)) 
                (get min-grade-required (unwrap-panic (map-get? courses {course-id: prereq-id}))))
        )
    )
)

;; GPA calculation functions
(define-private (calculate-grade-points (grade uint))
    (cond
        (>= grade u93) u40  ;; A  = 4.0
        (>= grade u90) u37  ;; A- = 3.7
        (>= grade u87) u33  ;; B+ = 3.3
        (>= grade u83) u30  ;; B  = 3.0
        (>= grade u80) u27  ;; B- = 2.7
        (>= grade u77) u23  ;; C+ = 2.3
        (>= grade u73) u20  ;; C  = 2.0
        (>= grade u70) u17  ;; C- = 1.7
        (>= grade u67) u13  ;; D+ = 1.3
        (>= grade u63) u10  ;; D  = 1.0
        u0                  ;; F  = 0.0
    )
)

(define-private (calculate-gpa (student-id uint))
    (let (
        (grades-list (map-get? student-grades {student-id: student-id, course-id: u0}))
        (total-points (fold + (map get-grade-points grades-list) u0))
        (total-credits (fold + (map get-course-credits grades-list) u0))
        )
        (if (> total-credits u0)
            (/ (* total-points u100) total-credits)
            u0
        )
    )
)

(define-private (get-grade-points (grade-info (optional {grade: uint, semester: uint, year: uint, completed: bool, instructor: (string-utf8 50), grade-points: uint, attempts: uint})))
    (if (is-some grade-info)
        (get grade-points (unwrap-panic grade-info))
        u0
    )
)

(define-private (get-course-credits (grade-info (optional {grade: uint, semester: uint, year: uint, completed: bool, instructor: (string-utf8 50), grade-points: uint, attempts: uint})))
    (if (and (is-some grade-info) (get completed (unwrap-panic grade-info)))
        (get credits (unwrap-panic (map-get? courses {course-id: (get course-id grade-info)})))
        u0
    )
)

;; Student management functions with enhanced validation
(define-public (add-student 
    (student-id uint) 
    (name (string-utf8 50))
    (enrollment-year uint)
    (major (string-utf8 30)))
    (let ((caller tx-sender))
        (asserts! (is-administrator caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? students {student-id: student-id})) ERR-STUDENT-EXISTS)
        (asserts! (is-valid-year enrollment-year) ERR-INVALID-YEAR)
        (asserts! (is-valid-name-length name) ERR-INVALID-NAME-LENGTH)
        (ok (map-set students 
            {student-id: student-id}
            {
                name: name,
                enrollment-year: enrollment-year,
                major: major,
                active: true,
                graduation-year: none,
                total-credits: u0,
                academic-standing: "Good Standing"
            }
        ))
    )
)

;; Grade recording with enhanced validation and GPA update
(define-public (record-grade
    (student-id uint)
    (course-id uint)
    (grade uint)
    (semester uint)
    (year uint)
    (instructor (string-utf8 50)))
    (let (
        (caller tx-sender)
        (grade-points (calculate-grade-points grade))
        )
        (asserts! (is-administrator caller) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? students {student-id: student-id})) ERR-STUDENT-NOT-FOUND)
        (asserts! (is-some (map-get? courses {course-id: course-id})) ERR-COURSE-NOT-FOUND)
        (asserts! (and (>= grade u0) (<= grade u100)) ERR-INVALID-GRADE)
        (asserts! (is-valid-semester semester) ERR-INVALID-SEMESTER)
        (asserts! (is-valid-year year) ERR-INVALID-YEAR)
        (asserts! (check-prerequisites student-id course-id) ERR-PREREQUISITE-NOT-MET)
        
        ;; Record grade
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
        
        ;; Update academic record
        (let ((new-gpa (calculate-gpa student-id)))
            (map-set academic-records
                {student-id: student-id}
                {
                    cumulative-gpa: new-gpa,
                    total-attempted-credits: (+ (default-to u0 (get total-attempted-credits (map-get? academic-records {student-id: student-id}))) 
                                             (get credits (unwrap-panic (map-get? courses {course-id: course-id})))),
                    total-earned-credits: (if (>= grade u60)
                                            (+ (default-to u0 (get total-earned-credits (map-get? academic-records {student-id: student-id})))
                                               (get credits (unwrap-panic (map-get? courses {course-id: course-id}))))
                                            (default-to u0 (get total-earned-credits (map-get? academic-records {student-id: student-id})))),
                    honors: (get honors (default-to {honors: (list), academic-warnings: u0} (map-get? academic-records {student-id: student-id}))),
                    academic-warnings: (if (< grade u60)
                                        (+ u1 (default-to u0 (get academic-warnings (map-get? academic-records {student-id: student-id}))))
                                        (default-to u0 (get academic-warnings (map-get? academic-records {student-id: student-id}))))
                }
            )
            (ok true)
        )
    )
)

;; Advanced read functions
(define-read-only (get-student-transcript (student-id uint))
    (let (
        (student-info (map-get? students {student-id: student-id}))
        (academic-record (map-get? academic-records {student-id: student-id}))
        )
        {
            student: student-info,
            academics: academic-record
        }
    )
)

(define-read-only (get-course-statistics (course-id uint))
    (let (
        (course-info (map-get? courses {course-id: course-id}))
        )
        {
            course: course-info,
            active: (get active (unwrap-panic course-info)),
            prerequisites: (get prerequisites (unwrap-panic course-info))
        }
    )
)

;; Audit logging function
(define-private (log-action (action (string-utf8 50)) (details (string-utf8 100)))
    (let (
        (tx-id (len (map-get? audit-log {transaction-id: u0})))
        )
        (map-set audit-log
            {transaction-id: tx-id}
            {
                timestamp: block-height,
                action: action,
                performed-by: tx-sender,
                details: details
            }
        )
    )
)

;; Administrative functions with audit logging
(define-public (add-administrator (admin-principal principal))
    (let ((caller tx-sender))
        (asserts! (is-administrator caller) ERR-NOT-AUTHORIZED)
        (map-set administrators (principal-to-string admin-principal) true)
        (log-action "Add Administrator" (concat (principal-to-string admin-principal) " added as administrator"))
        (ok true)
    )
)
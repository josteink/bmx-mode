
(require 'ert)
(require 'bmx-mode)

;;;
;;; label tests
;;;

(ert-deftest can-normalize-label ()
  (should (string-equal ":foo" (bmx--label-prefix ":foo")))
  (should (string-equal ":foo" (bmx--label-prefix "foo"))))

(ert-deftest can-unnormalize-label ()
  (should (string-equal "foo" (bmx--label-unprefix ":foo")))
  (should (string-equal "foo" (bmx--label-unprefix "foo"))))

(ert-deftest gets-label-references ()
  (let* ((buffer (find-file "./test-files/label-references.bat"))
         (references (bmx--get-labels)))
    ;; should be in sorted order!
    (should (equal references '(":ABC" ":ABCabc_123" ":END" ":MID" ":START")))
    (kill-buffer buffer)))

(ert-deftest filters-labels-correctly ()
  (let* ((full-list '(":abc" ":abcdef" ":def")))
    (should (equal (bmx--get-matching-labels ":" full-list)
                   full-list))
    (should (equal (bmx--get-matching-labels ":abc" full-list)
                   '(":abc" ":abcdef")))
    ;; case insensitive match!
    (should (equal (bmx--get-matching-labels ":DEF" full-list)
                   '(":def")))))

(ert-deftest label-at-point-works-at-point ()
  (let ((test-cases '(("test-case 0" . (":PROPER0"))
                      ("test-case 1" . (":proper1" ":proper2" ":proper3" ":PROPER4"))
                      ("test-case 2" . (":proper5" ":PROPER6" ":proper7" ":PROPER8")))))
    (dolist (test-case test-cases)
      (let ((buffer (find-file "./test-files/label-at-point.bat"))
            (marker (car test-case))
            (labels (cdr test-case)))
        (beginning-of-buffer)

        (search-forward marker)
        (beginning-of-line 1)

        (dolist (label labels)
          (beginning-of-line)
          (next-line 1)
          (should (string-equal label (bmx--label-at-point)))
          (forward-char 5)
          (should (string-equal label (bmx--label-at-point))))

        (kill-buffer buffer)))))

(ert-deftest label-at-point-handles-multiple-labels-on-same-line ()
  (let ((buffer (find-file "test-files/label-at-point-multiple.bat")))
    (beginning-of-buffer)
    (search-forward ":")
    (should (string-equal ":ckret" (bmx--label-at-point)))
    (search-forward ":")
    (should (string-equal ":copy_files" (bmx--label-at-point)))
    (kill-buffer buffer)))

(ert-deftest finds-references-correctly ()
  (let* ((buffer (find-file "./test-files/label-references.bat")))
    (bmx--label-find-references ":ABC")
    (switch-to-buffer "*Occur*")
    (assert (eq nil (search-forward ":ABCabc_123" nil t)))
    (kill-buffer "*Occur*")
    (kill-buffer buffer)))

;; TODO
;; Navigation-based tests?

;;;
;;; variables tests
;;;

(ert-deftest can-normalize-variable ()
  (should (string-equal "%foo%" (bmx--variable-normalize "%foo%")))
  (should (string-equal "%foo%" (bmx--variable-normalize "foo"))))

(ert-deftest can-unnormalize-variable ()
  (should (string-equal "foo" (bmx--variable-unnormalize "%foo%")))
  (should (string-equal "foo" (bmx--variable-unnormalize "foo"))))

(ert-deftest gets-variable-references ()
  (let* ((buffer (find-file "./test-files/variable-references.bat"))
         (references (bmx--get-variables)))
    ;; should be in sorted order!
    (should (equal references '("%ABC%" "%ABCabc_123%" "%END%" "%MID%" "%START%")))
    (kill-buffer buffer)))

(ert-deftest filters-variables-correctly ()
  (let* ((full-list '("%abc%" "%abcdef%" "%def%")))
    (should (equal (bmx--get-matching-variables "%" full-list)
                   full-list))
    (should (equal (bmx--get-matching-variables "%abc" full-list)
                   '("%abc%" "%abcdef%")))
    ;; case insensitive match!
    (should (equal (bmx--get-matching-variables "%DEF" full-list)
                   '("%def%")))))

(ert-run-tests-interactively t)

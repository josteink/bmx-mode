
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
    (should (equal (bmx--get-matching-labels ":def" full-list)
                   '(":def")))))

(ert-deftest finds-references-correctly ()
  (let* ((buffer (find-file "./test-files/label-references.bat")))
    (bmx--label-find-references ":ABC")
    (switch-to-buffer "*Occur*")
    (assert (eq nil (search-forward ":ABCabc_123" nil t)))

    ;; TODO testcase
    ;; Must find 
    ))


(ert-run-tests-interactively t)


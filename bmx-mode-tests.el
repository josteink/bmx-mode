
(require 'ert)


;; development only packages, not declared as a package-dependency
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))

;; we depend on lots of stuff.
(dolist (p '(s dash company))
  (when (not (require p nil t))
    (package-refresh-contents)
    (package-install p)))

(require 'bmx-mode)

;;;
;;; label tests
;;;

(ert-deftest can-normalize-label ()
  (should (string-equal ":foo" (bmx--label-normalize ":foo")))
  (should (string-equal ":foo" (bmx--label-normalize "foo"))))

(ert-deftest can-unnormalize-label ()
  (should (string-equal "foo" (bmx--label-unnormalize ":foo")))
  (should (string-equal "foo" (bmx--label-unnormalize "foo"))))

(ert-deftest gets-label-references ()
  (let ((buffer (find-file "./test-files/label-references.bat"))
        (references (bmx--get-labels)))
    ;; should be in sorted order!
    (should (equal references '(":ABC" ":ABCabc_123" ":END" ":MID" ":START")))
    (kill-buffer buffer)))

(ert-deftest filters-labels-correctly ()
  (let ((full-list '(":abc" ":abcdef" ":def")))
    (should (equal (bmx--get-matching-labels ":" full-list)
                   full-list))
    (should (equal (bmx--get-matching-labels ":abc" full-list)
                   '(":abc" ":abcdef")))
    ;; case insensitive match!
    (should (equal (bmx--get-matching-labels ":DEF" full-list)
                   '(":def")))))

(ert-deftest label-at-point-works-at-point ()
  (let ((buffer (find-file "./test-files/label-at-point.bat"))
        (test-cases '(("test-case 0" . (":PROPER0"))
                      ("test-case 1" . (":proper1" ":proper2" ":proper3" ":PROPER4"))
                      ("test-case 2" . (":proper5" ":PROPER6" ":proper7" ":PROPER8")))))
    (dolist (test-case test-cases)
      (let ((marker (car test-case))
            (labels (cdr test-case)))
        (goto-char (point-min))

        (search-forward marker)
        (beginning-of-line 1)

        (dolist (label labels)
          (beginning-of-line)
          (forward-line 1)
          (should (string-equal label (bmx--label-at-point)))
          (forward-char 5)
          (should (string-equal label (bmx--label-at-point))))))

    (kill-buffer buffer)))

(ert-deftest can-navigate-to-label ()
  (let ((buffer (find-file "./test-files/label-navigation.bat"))
        (test-cases '(("test-case 1" . ":LABEL1")
                      ("test-case 2" . "LABEL2")
                      ("test-case 3" . "label3"))))

    (dolist (test-case test-cases)
      (let ((marker (car test-case))
            (label (cdr test-case)))
        (goto-char (point-min))
        (search-forward marker)
        (beginning-of-line 1)
        (forward-line 1)

        (let ((expected-point (point)))
          (goto-char (point-min))
          (bmx--label-navigate-to label)
          (beginning-of-line 1)
          (should (= expected-point (point))))))))

(ert-deftest label-at-point-handles-multiple-labels-on-same-line ()
  (let ((buffer (find-file "test-files/label-at-point-multiple.bat")))
    (goto-char (point-min))
    (search-forward ":")
    (should (string-equal ":ckret" (bmx--label-at-point)))
    (search-forward ":")
    (should (string-equal ":copy_files" (bmx--label-at-point)))
    (kill-buffer buffer)))

(ert-deftest finds-references-correctly ()
  (let ((buffer (find-file "./test-files/label-references.bat")))
    (bmx--label-find-references ":ABC")
    (switch-to-buffer "*Occur*")
    (should (eq nil (search-forward ":ABCabc_123" nil t)))
    (kill-buffer "*Occur*")
    (kill-buffer buffer)))

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

(ert-deftest variable-at-point-works-at-point ()
  (let ((buffer (find-file "./test-files/variable-at-point.bat"))
        (test-cases '(("test-case 0" . ("%PROPER0%"))
                      ("test-case 1" . ("%proper1%"))
                      ("test-case 2" . ("%proper2%"))
                      ("test-case 3" . ("%proper3%" "%proper4%"))
                      ("test-case 4" . ("%proper5%")))))
    (dolist (test-case test-cases)
      (let ((marker (car test-case))
            (variables (cdr test-case)))
        (goto-char (point-min))

        (search-forward marker)
        (beginning-of-line 1)
        (forward-line 1)
        (forward-word 2)

        (dolist (variable variables)
          (should (string-equal variable (bmx--variable-at-point)))
          (forward-word 1))))

    (kill-buffer buffer)))

(ert-deftest can-navigate-to-variables ()
  (let ((buffer (find-file "./test-files/variable-navigation.bat"))
        (test-cases '(("test-case 1" . "variable1")
                      ("test-case 2" . "VARIABLE2")
                      ("test-case 3" . "variable3"))))

    (dolist (test-case test-cases)
      (let ((marker (car test-case))
            (variable (cdr test-case)))
        (goto-char (point-min))
        (search-forward marker)

        (let ((expected-point (- (search-forward "value") 5)))
          (bmx--variable-navigate-to variable)
          (should (= expected-point (point))))))))

(ert-deftest finds-variables-correctly ()
  (let ((buffer (find-file "./test-files/variable-references.bat")))
    (bmx--variable-find-references "%abc%")
    (switch-to-buffer "*Occur*")
    (should (eq nil (search-forward "%ABCabc_123%" nil t)))
    (should (not (eq nil (search-forward "%ABC%"))))
    (kill-buffer "*Occur*")
    (kill-buffer buffer)))

(ert-deftest finds-variables-with-quoted-syntax-correctly ()
  (let ((buffer (find-file "./test-files/variable-references.bat")))
    (bmx--variable-find-references "%end%")
    (switch-to-buffer "*Occur*")
    (should (not (eq nil (search-forward "set \"end=end\"" nil t))))
    (kill-buffer "*Occur*")
    (kill-buffer buffer)))

;; (ert-run-tests-interactively t)

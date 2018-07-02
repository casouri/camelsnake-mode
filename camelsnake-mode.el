;;; camelsnake-mode.el --- Insert snake case with camel case key stroke

;;; Commentary:
;; 

;;; Code:

(defvar camelsnake-mode-spec-alist ()
  "Lists special sepcifications for specific major modes.
Takes the form of:
\(major-mode . \(\(char-list . list\)
               \(char-before-list . list\)
               \(delimiter . char\)
               \(judge . function\)\)\)
char-list sets `camelsnake-char-list',
char-before-list sets `camelsnake-char-before-list',
delimiter sets `camelsnake-delimiter',
judge sets `camelsnake-disable-judge'.")

(defvar-local camelsnake-char-list '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)
  "Characters that camelsnake will modify.")

(defvar-local camelsnake-char-before-list '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)
  "Characters before the just inserted character that will make camelsnake modify input.")

(defvar-local camelsnake-delimiter ?-
  "The default delimiter.")

(defvar-local camelsnake-disable-judge nil
  "The user defined function that decides if camelsnake should modify character.
This function is called when user just typed the critical character, e.g.
\"camelC|\" \(\"|\" is the point\).

This function takes no argument, return t to allow modification, return nil otherwise.

Mind that this function is called in `post-self-insert-hook', so don't put heavy stuff in it.")

;;
;; Entry
;;

(define-minor-mode camelsnake-mode
  "Insert snake case with camel case key stroke."
  :global t
  (if camelsnake-mode
      (add-hook 'after-change-major-mode-hook #'camelsnake-configure-on-major-mode)
    (remove-hook 'after-change-major-mode-hook #'camelsnake-configure-on-major-mode)))

;;
;; Main logic
;;

(defun camelsnake-modify-insert ()
  "Modify the Previos Inserted. Meant to be added to `post-self-insert-hook'."
  (let ((just-inserted (char-before (point)))
        (before-inserted (char-before (1- (point)))))
    ;; if the char just inserted is a Captical _and_
    ;; the one before it is not, insert the delimiter.
    (when (and (member just-inserted camelsnake-char-list)
               (member before-inserted camelsnake-char-before-list)
               (if camelsnake-disable-judge
                   (funcall camelsnake-disable-judge)
                 t))
      (backward-char)
      (insert (or (alist-get 'delimiter (alist-get major-mode camelsnake-mode-spec-alist))
                  camelsnake-delimiter))
      (forward-char)
      (downcase-region (1- (point)) (point)))))

(defun camelsnake-configure-on-major-mode ()
  "Configure camelsnake-mode on the major mode."
  (interactive)
  (let* ((spec-alist (alist-get major-mode camelsnake-mode-spec-alist))
         (char-list (alist-get 'char-list spec-alist))
         (char-before-list (alist-get 'char-before-list spec-alist))
         (delimiter (alist-get 'delimiter spec-alist))
         (on (alist-get 'on spec-alist))
         (judge (alist-get 'judge spec-alist)))
    (if on
        (add-hook 'post-self-insert-hook #'camelsnake-modify-insert nil t)
      (remove-hook 'post-self-insert-hook #'camelsnake-modify-insert t))
    (when char-list (setq camelsnake-char-list char-list))
    (when char-before-list (setq camelsnake-char-before-list char-before-list))
    (when delimiter (setq camelsnake-delimiter delimiter))
    (when judge (setq camelsnake-disable-judge judge))))

;;
;; Add on
;;

(defvar camelsnake-python-judge-last-line nil
  "Cache for `camelsnake-python-judge'.")

(defvar camelsnake-python-judge-result nil
  "Cache for `camelsnake-python-judge'.")

(defun camelsnake-python-judge ()
  "Judge function for camelsnake in Python."
  (if (eq (what-line) camelsnake-python-judge-last-line)
      camelsnake-python-judge-result
    (let ((point (point))
          (result (not (search-backward "class" (line-beginning-position) t))))
      (goto-char point)
      (setq camelsnake-python-judge-last-line (what-line))
      (setq camelsnake-python-judge-result result))))


(provide 'camelsnake-mode)

;;; camelsnake-mode.el ends here

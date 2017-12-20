
(require 'minibuffer-line)

;; (setq minibuffer-line-format '((:eval
;; 				(let ((time-string (format-time-string "%l:%M %b %d %a"))
;; 				      (let ((end-string (concat time-string)))
;; 					(let ((padding (make-string (- (frame-text-cols) (string-width end-string) ?))))
;;                                    (concat padding end-string))))))))


(defun strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(setq minibuffer-line-format
      '((:eval
	 (let ((time-string (format-time-string "%s"))
	       (mode-string (format-mode-line mode-line-format)))
	   (let ((end-string (concat time-string mode-string)))
	     (let ((padding (make-string (- (frame-text-cols) (string-width end-string)) ? )))
	       (concat padding end-string)))))))


(minibuffer-line-mode)

;; TODO: put mode line stuff in the echo area maybe, and hide mode line.
;; (make sure to still indicate active buffer somehow)
;; maybe scoll bars and just the line number would save space at the bottom mode line


;; variable for the timer object
(defvar idle-timer-cookbook-timer 1xxzx)

;; callback function
(defun idle-timer-cookbook-callback ()
  (message "I have been called (%s)" (current-time-string)))

;; start functions
(defun idle-timer-cookbook-run-once ()
  (interactive)
  (when (timerp idle-timer-cookbook-timer)
    (cancel-timer idle-timer-cookbook-timer))
  (setq idle-timer-cookbook-timer
          (run-with-idle-timer 1 nil #'idle-timer-cookbook-callback)))

(defun idle-timer-cookbook-start ()
  (interactive)
  (when (timerp idle-timer-cookbook-timer)
    (cancel-timer idle-timer-cookbook-timer))
  (setq idle-timer-cookbook-timer
          (run-with-timer 1 1 #'idle-timer-cookbook-callback)))

;; stop function
(defun idle-timer-cookbook-stop ()
  (interactive)
  (when (timerp idle-timer-cookbook-timer)
    (cancel-timer idle-timer-cookbook-timer))
  (setq idle-timer-cookbook-timer nil))


(require 'symon)

(defvar my-mode-line-format
  '("%e"
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    mode-line-position
    (vc-mode vc-mode)
    mode-line-modes
    mode-line-misc-info
    mode-line-end-spaces))


(define-symon-monitor symon-mode-line
  :display (propertize (format-mode-line my-mode-line-format)))

(setq symon-delay 0)
(setq symon-history-size 10)

(setq symon-monitors '(symon-mode-line))

(setq-default mode-line-format nil)

(symon-mode)

(with-current-buffer " *Echo Area*" (face-remap-add-relative 'default :background "#222"))
(with-current-buffer " *Echo Area 1*" (face-remap-add-relative 'default :background "#222"))
